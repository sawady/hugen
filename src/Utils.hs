module Utils (measure) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified SDL

measure :: (MonadIO m) => m a -> String -> m a
measure action label = do
  start <- SDL.ticks
  result <- action
  end <- SDL.ticks
  liftIO $ putStrLn $ label ++ " took: " ++ show (end - start) ++ "ms"
  return result
