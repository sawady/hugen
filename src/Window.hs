module Window (Window, windowConfig, createWindow, WindowConfig, WindowSetting (..)) where

import Control.Monad.Cont (MonadIO)
import Data.Text (Text)
import Foreign.C (CInt)
import qualified SDL

type Window = SDL.Window

data WindowConfig = W {_title :: Text, _config :: SDL.WindowConfig}

data WindowSetting = Border | HighDPI | InputGrabbed | WindowMode SDL.WindowMode | GraphicsContext | Position | Resizable | InitialSize | Visible

-- | Create a window with the given title and configuration.
--
-- Throws 'SDLException' on failure.
windowConfig :: Text -> CInt -> CInt -> [WindowSetting] -> WindowConfig
windowConfig title width height = foldl apply $ W {_title = title, _config = SDL.defaultWindow {SDL.windowPosition = SDL.Centered, SDL.windowInitialSize = SDL.V2 width height}}
  where
    apply w c = undefined

createWindow :: (MonadIO m) => WindowConfig -> m Window
createWindow w = SDL.createWindow (_title w) (_config w)
