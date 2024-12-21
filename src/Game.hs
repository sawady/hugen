{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Game (game) where

import Control.Lens
import Control.Monad.State
import Data.Text (Text)
import Renderer (Renderer)
import qualified SDL
import qualified SDL.Image
import System.Exit (exitFailure, exitSuccess)
import Window (Window)
import Control.Monad.Except ( MonadError(catchError) )

-- Unified Game data type
data Game = Game
  { _window :: Window,
    _renderer :: Renderer,
    _background :: SDL.Texture,
    _clean :: [IO ()]
  }

makeLenses ''Game

type Action a = StateT Game IO a

-- Main game entry point
game :: Text -> SDL.WindowConfig -> IO ()
game title config = do
  gameState <- initSDL title config
  void $ execStateT gameloop gameState

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

  -- Load the background texture
  b <- SDL.Image.loadTexture r "./resources/bg.png" `andCatch` "Error loading Texture"
  let cleanTexture = SDL.destroyTexture b

  -- Return the fully constructed `Game` object
  return $
    Game
      { _window = w,
        _renderer = r,
        _background = b,
        _clean = [cleanTexture, cleanRenderer, cleanWindow, cleanImage, cleanSDL]
      }

-- Generalized error handling and cleanup
andCatch :: IO a -> String -> IO a
andCatch action errorMsg = catchError action (exitWith errorMsg)

exitWith :: Show a => String -> a -> IO b
exitWith errorMsg e = do
  putStrLn $ errorMsg ++ ":"
  print e
  exitFailure

-- Clean up resources and exit
exitClean :: Action a
exitClean = do
  liftIO $ putStrLn "Exit clean!"
  actions <- use clean
  liftIO $ sequence_ actions
  liftIO exitSuccess

-- Handle SDL events
handleEvent :: SDL.Event -> Action ()
handleEvent event = case SDL.eventPayload event of
  SDL.KeyboardEvent keyboardEvent
    | SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed ->
        case SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) of
          SDL.KeycodeEscape -> exitClean
          SDL.KeycodeQ -> exitClean
          _ -> return ()
  SDL.QuitEvent -> exitClean
  _ -> return ()

-- Game loop
gameloop :: Action ()
gameloop = do
  -- Poll and handle events
  SDL.pollEvents >>= mapM_ handleEvent

  -- Render the frame
  r <- use renderer
  b <- use background
  liftIO $ SDL.clear r
  liftIO $ SDL.copy r b Nothing Nothing
  liftIO $ SDL.present r

  -- Delay for frame timing (16ms for ~60fps)
  liftIO $ SDL.delay 16

  -- Recursive call
  gameloop