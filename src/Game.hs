module Game (Game (..), newGame, loop) where

import Control.Exception (SomeException, catch)
import Control.Monad.State (MonadIO (liftIO), MonadState (put), StateT (runStateT), gets, modify, execStateT)
import Data.Text (Text)
import Renderer (Renderer)
import qualified SDL
import System.Exit (exitFailure, exitSuccess)
import Window (Window)
import Prelude hiding (init)

data Game = Game {_window :: Window, _renderer :: Renderer, _clean :: IO ()}

newGame :: Text -> SDL.WindowConfig -> IO Game
newGame title config = do
  ((window, renderer), clean) <- liftIO $ runStateT (initSDL title config) (return ())
  execStateT loop $ Game {_window = window, _renderer = renderer, _clean = clean}

initSDL :: Text -> SDL.WindowConfig -> StateT (IO ()) IO (Window, Renderer)
initSDL title config = do
  put $ putStrLn "All Cleaned!"

  try SDL.initializeAll "Error initializing SDL2"
  modify (SDL.quit >>)

  window <- try (SDL.createWindow title config) "Error creating Window"
  modify (SDL.destroyWindow window >>)

  renderer <- try (SDL.createRenderer window (-1) SDL.defaultRenderer) "Error creating Renderer"
  modify (SDL.destroyRenderer renderer >>)

  return (window, renderer)

loop :: StateT Game IO ()
loop = do
  renderer <- gets _renderer
  SDL.clear renderer
  SDL.present renderer
  SDL.delay 5000
  exitClean

try :: (MonadIO m) => IO a -> String -> m a
try action errorMsg = liftIO $ catch action $ errorClean errorMsg

errorClean :: String -> SomeException -> IO a
errorClean msg exception = do
  putStrLn $ msg ++ ":"
  print exception
  exitFailure

exitClean :: StateT Game IO ()
exitClean = do
  clean <- gets _clean
  liftIO clean
  liftIO exitSuccess
