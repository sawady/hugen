{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Config (Config, load, title, width, height) where

import Control.Lens
import Data.Ini.Config
import qualified Data.Text.IO as Text
import Foreign.C (CInt)
import Utils
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)

data Config = Config
  { _width :: CInt,
    _height :: CInt,
    _title :: Text
  }

makeLenses ''Config

parser :: IniParser Config
parser = section "WINDOW" $ Config <$> fieldOf "width" number <*> fieldOf "height" number <*> fieldOf "title" string

load :: MonadIO m => m Config
load =
  do
    content <- liftIO $ Text.readFile "resources/config.ini"
    case parseIniFile content parser of
      Left msg -> liftIO $ exitWith "Cannot parse config file" msg
      Right cfg -> return cfg
