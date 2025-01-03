module Utils (measure, catching, exitWith) where

import Control.Monad.Except (MonadError (catchError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified SDL
import System.Exit (exitFailure)

measure :: (MonadIO m) => m a -> String -> m a
measure action label = do
  start <- SDL.ticks
  result <- action
  end <- SDL.ticks
  liftIO $ putStrLn $ label ++ " took: " ++ show (end - start) ++ "ms"
  return result

-- Generalized error handling and cleanup
catching :: IO a -> String -> IO a
catching action errorMsg = catchError action (exitWith errorMsg)

exitWith :: (Show a) => String -> a -> IO b
exitWith errorMsg e = do
  putStrLn $ errorMsg ++ ":"
  print e
  exitFailure
