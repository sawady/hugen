{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Game (game, window, renderer, scenes, clean, windowSize) where

import Control.Concurrent (threadDelay)
import Control.Lens
    ( Lens',
      (&),
      (^.),
      use,
      non,
      lens,
      (%=),
      (.=),
      (.~),
      mapped,
      makeLenses,
      At(at),
      Ixed(ix),
      Zoom(zoom) )
import Control.Monad.State
    ( void, when, MonadIO(liftIO), StateT, execStateT )
import qualified Data.Ini.Config as Config
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Foreign.C (CInt)
import qualified GameInput
import qualified SDL
import qualified SDL.Image
import qualified Scene
import Sprite (loadFromSheet)
import qualified Sprite
import System.Exit (exitSuccess)
import Utils
import qualified Config

-- Unified Game data type
data Game = Game
  { _window :: SDL.Window,
    _renderer :: SDL.Renderer,
    _scenes :: M.Map String Scene.Scene,
    _sceneName :: String,
    _clean :: [IO ()],
    _input :: GameInput.GameInput,
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
  content <- Text.readFile "resources/config.ini"
  Config.load $ \cfg -> do
      let w = cfg ^. Config.width
          h = cfg ^. Config.height
      gameState <- initSDL title (w, h) config
      void $ (`execStateT` gameState) $ do
        loadAssets
        sceneName .= "sonic"
        zoom scene Scene.onStart
        gameloop

-- Initialize SDL and the game resources
initSDL :: Text -> (CInt, CInt) -> SDL.WindowConfig -> IO Game
initSDL title (width, height) config = do
  -- Initialize SDL
  SDL.initializeAll `catching` "Error initializing SDL2"
  let cleanSDL = SDL.quit

  -- Initialize SDL Image
  SDL.Image.initialize [SDL.Image.InitPNG] `catching` "Error initializing SDL2 Image"
  let cleanImage = SDL.Image.quit

  -- Create the window
  w <- SDL.createWindow title config `catching` "Error creating Window"
  let cleanWindow = SDL.destroyWindow w
  SDL.windowMinimumSize w SDL.$= SDL.V2 width height

  -- Create the renderer
  r <- SDL.createRenderer w (-1) SDL.defaultRenderer `catching` "Error creating Renderer"
  let cleanRenderer = SDL.destroyRenderer r

  -- Return the fully constructed `Game` object
  return $
    Game
      { _window = w,
        _renderer = r,
        _scenes = M.empty,
        _sceneName = "empty",
        _clean = [cleanRenderer, cleanWindow, cleanImage, cleanSDL],
        _input = GameInput.create,
        _windowSize = SDL.V2 width height -- Initialize window size
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
  r <- use renderer
  use (scene . Scene.sprites) >>= mapM_ (Sprite.render r)

loadSprite :: String -> GameIO Sprite.Sprite
loadSprite name = do
  r <- use renderer
  liftIO $ Sprite.load r name (image name) `catching` "Error loading Texture"

loadSpriteFromSheet :: String -> [Int] -> GameIO Sprite.Sprite
loadSpriteFromSheet name frames = do
  r <- use renderer
  liftIO $ Sprite.loadFromSheet r name (image name) frames

image :: String -> String
image name = "resources/" ++ name ++ ".png"

-- Clean up resources and exit
exitClean :: GameIO a
exitClean = do
  liftIO $ putStrLn "Exit clean!"
  cleanup
  liftIO exitSuccess

cleanup :: GameIO ()
cleanup =
  do
    use clean >>= liftIO . sequence_
    use (scenes . traverse . Scene.sprites) >>= mapM_ Sprite.destroy

-- Handle SDL events
handleEvent :: SDL.Event -> GameIO ()
handleEvent event =
  case SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEvent -> input %= GameInput.addKeyboardEvent keyboardEvent
    SDL.WindowResizedEvent e -> handleResize $ fmap fromIntegral (SDL.windowResizedEventSize e)
    SDL.MouseMotionEvent e -> input %= GameInput.addMouseMovementEvent e
    SDL.MouseButtonEvent e -> input %= GameInput.addMouseButtonEvent e
    SDL.QuitEvent -> exitClean
    _ -> return ()

handleResize :: SDL.V2 CInt -> GameIO ()
handleResize newSize = do
  oldSize <- use windowSize
  let SDL.V2 oldWidth oldHeight = oldSize
      SDL.V2 newWidth newHeight = newSize
      scaleX :: Double = fromIntegral newWidth / fromIntegral oldWidth
      scaleY :: Double = fromIntegral newHeight / fromIntegral oldHeight

  -- Update all sprites of current scene
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
  i <- use input
  when (GameInput.pressed SDL.KeycodeQ i) exitClean

handleEvents :: GameIO ()
handleEvents = SDL.pollEvents >>= mapM_ handleEvent

render :: GameIO ()
render = do
  r <- use renderer
  SDL.clear r
  renderScene
  SDL.present r

update :: GameIO ()
update = do
  i <- use input
  zoom scene $ Scene.update i