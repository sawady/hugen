{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Game (game, window, renderer, scenes, clean, windowSize) where

import Control.Concurrent (threadDelay)
import Control.Lens
import Control.Monad.Except (MonadError (catchError))
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Text (Text)
import Foreign.C (CInt)
import qualified SDL
import qualified SDL.Image
import qualified Scene
import Sprite (loadFromSheet)
import qualified Sprite
import System.Exit (exitFailure, exitSuccess)

-- Unified Game data type
data Game = Game
  { _window :: SDL.Window,
    _renderer :: SDL.Renderer,
    _scenes :: M.Map String Scene.Scene,
    _sceneName :: String,
    _clean :: [IO ()],
    _activeKeys :: Set.Set SDL.Keycode,
    _windowSize :: SDL.V2 CInt
  }

makeLenses ''Game

-- | Constants
ticksPerSecond :: Int
ticksPerSecond = 60

tickDuration :: Int
tickDuration = 1000000 `div` ticksPerSecond -- Microseconds per tick

type GameIO a = StateT Game IO a

-- Main game entry point
game :: Text -> SDL.WindowConfig -> IO ()
game title config = do
  gameState <- initSDL title config
  void $ (`execStateT` gameState) $ do
    loadAssets
    sceneName .= "sonic"
    zoom scene Scene.onStart
    gameloop

-- Initialize SDL and the game resources
initSDL :: Text -> SDL.WindowConfig -> IO Game
initSDL title config = do
  -- Initialize SDL
  SDL.initializeAll `andCatch` "Error initializing SDL2"
  let cleanSDL = SDL.quit

  -- Initialize SDL Image
  SDL.Image.initialize [SDL.Image.InitPNG] `andCatch` "Error initializing SDL2 Image"
  let cleanImage = SDL.Image.quit

  -- Create the window
  w <- SDL.createWindow title config `andCatch` "Error creating Window"
  let cleanWindow = SDL.destroyWindow w

  -- Create the renderer
  r <- SDL.createRenderer w (-1) SDL.defaultRenderer `andCatch` "Error creating Renderer"
  let cleanRenderer = SDL.destroyRenderer r

  -- Return the fully constructed `Game` object
  return $
    Game
      { _window = w,
        _renderer = r,
        _scenes = M.empty,
        _sceneName = "empty",
        _clean = [cleanRenderer, cleanWindow, cleanImage, cleanSDL],
        _activeKeys = Set.empty,
        _windowSize = SDL.V2 800 600 -- Initialize window size
      }

loadAssets :: GameIO ()
loadAssets = do
  _sonic <- loadSpriteFromSheet "sonic" [4, 4, 4, 4]
  _bg <- loadSprite "bg"
  addScene $ Scene.new "sonic" [_sonic, _bg]

addScene :: Scene.Scene -> GameIO ()
addScene s = scenes %= M.insert (s ^. Scene.name) s

renderScene :: GameIO ()
renderScene = do
    ss <- use (scene . Scene.sprites)
    r <- use renderer
    mapM_ (Sprite.render r) ss

loadSprite :: String -> GameIO Sprite.Sprite
loadSprite name = do
  r <- use renderer
  liftIO $ Sprite.load r name (image name) `andCatch` "Error loading Texture"

loadSpriteFromSheet :: String -> [Int] -> GameIO Sprite.Sprite
loadSpriteFromSheet name frames = do
  r <- use renderer
  liftIO $ Sprite.loadFromSheet r name (image name) frames `andCatch` "Error loading Texture"

image :: String -> String
image name = "resources/" ++ name ++ ".png"

-- Generalized error handling and cleanup
andCatch :: IO a -> String -> IO a
andCatch action errorMsg = catchError action (exitWith errorMsg)

exitWith :: (Show a) => String -> a -> IO b
exitWith errorMsg e = do
  putStrLn $ errorMsg ++ ":"
  print e
  exitFailure

cleanup :: GameIO ()
cleanup =
  do
    use clean >>= liftIO . sequence_
    use (scenes . traverse . Scene.sprites) >>= mapM_ Sprite.destroy

-- Clean up resources and exit
exitClean :: GameIO a
exitClean = do
  liftIO $ putStrLn "Exit clean!"
  cleanup
  liftIO exitSuccess

-- Handle SDL events
handleEvent :: SDL.Event -> GameIO ()
handleEvent event =
  case SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEvent ->
      let keycode = SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent)
       in case SDL.keyboardEventKeyMotion keyboardEvent of
            SDL.Pressed -> activeKeys %= Set.insert keycode
            SDL.Released -> activeKeys %= Set.delete keycode
    SDL.WindowResizedEvent e -> handleResize $ fmap fromIntegral (SDL.windowResizedEventSize e)
    SDL.QuitEvent -> exitClean
    _ -> return ()

handleResize :: SDL.V2 CInt -> GameIO ()
handleResize newSize = do
  oldSize <- use windowSize
  let SDL.V2 oldWidth oldHeight = oldSize
      SDL.V2 newWidth newHeight = newSize
      scaleX = fromIntegral newWidth / fromIntegral oldWidth
      scaleY = fromIntegral newHeight / fromIntegral oldHeight

  -- Update all sprites
  scene . Scene.sprites . mapped %= Sprite.resizeSprite scaleX scaleY

  -- Update stored window size
  windowSize .= newSize

scene :: Lens' Game Scene.Scene
scene = lens getScene setScene
  where
    getScene g =
      let name = g ^. sceneName
       in g ^. scenes . at name . non (Scene.new "" [])

    setScene g newScene =
      let name = g ^. sceneName
       in g & scenes . ix name .~ newScene

update :: GameIO ()
update = do
  keys <- use activeKeys
  zoom scene $ Scene.update keys

-- Game loop
gameloop :: GameIO ()
gameloop = do
  start <- SDL.ticks

  -- Poll and handle events
  handleEvents
  wantsQuit

  update
  render

  -- Wait for the next tick
  end <- SDL.ticks
  let elapsed = end - start
      delayTime = max 0 (tickDuration - fromIntegral elapsed)
  liftIO $ threadDelay delayTime

  -- Loop with updated time
  gameloop

wantsQuit :: GameIO ()
wantsQuit = do
  keys <- use activeKeys
  when (SDL.KeycodeQ `Set.member` keys) exitClean

handleEvents :: GameIO ()
handleEvents = SDL.pollEvents >>= mapM_ handleEvent

render :: GameIO ()
render = do
  r <- use renderer
  SDL.clear r
  renderScene
  SDL.present r
