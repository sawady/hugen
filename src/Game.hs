{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Game (game, window, renderer, sprites, clean) where

import Control.Lens
import Control.Monad.Except (MonadError (catchError))
import Control.Monad.State
import qualified Data.Set as Set
import Data.Text (Text)
import qualified SDL
import qualified SDL.Image
import Sprite (loadFromSheet)
import qualified Sprite
import System.Exit (exitFailure, exitSuccess)
import Control.Lens.Internal.CTypes (Word32)
import Control.Concurrent (threadDelay)

-- Unified Game data type
data Game = Game
  { _window :: SDL.Window,
    _renderer :: SDL.Renderer,
    _sprites :: [Sprite.Sprite],
    _clean :: [IO ()],
    _activeKeys :: Set.Set SDL.Keycode
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
    sprites . ix 1 %= Sprite.scale 2
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
        _sprites = [],
        _clean = [cleanRenderer, cleanWindow, cleanImage, cleanSDL],
        _activeKeys = Set.empty
      }

loadAssets :: GameIO ()
loadAssets = do
  loadSpriteFromSheet "sonic" [4, 4, 4, 4]
  loadSprite "bg"

loadSprite :: String -> GameIO ()
loadSprite name = do
  r <- use renderer
  s <- liftIO $ Sprite.load r (image name) `andCatch` "Error loading Texture"
  addSprite s

loadSpriteFromSheet :: String -> [Int] -> StateT Game IO ()
loadSpriteFromSheet name frames = do
  r <- use renderer
  s <- liftIO $ Sprite.loadFromSheet r (image name) frames `andCatch` "Error loading Texture"
  addSprite s

image :: String -> String
image name = "resources/" ++ name ++ ".png"

addSprite :: Sprite.Sprite -> GameIO ()
addSprite s = sprites %= (s :)

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
    use sprites >>= mapM_ Sprite.destroy

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
    SDL.QuitEvent -> exitClean
    _ -> return ()

update :: GameIO ()
update = do
  let speed = 10 -- Movement speed in pixels per second
  keys <- use activeKeys
  when (SDL.KeycodeUp `Set.member` keys) $
    sprites . ix 1 . Sprite.posY -= speed
  when (SDL.KeycodeDown `Set.member` keys) $
    sprites . ix 1 . Sprite.posY += speed
  when (SDL.KeycodeLeft `Set.member` keys) $
    sprites . ix 1 . Sprite.posX -= speed
  when (SDL.KeycodeRight `Set.member` keys) $
    sprites . ix 1 . Sprite.posX += speed
  sprites . ix 1 %= Sprite.nextFrame

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
  use sprites >>= mapM_ (Sprite.render r)
  SDL.present r
