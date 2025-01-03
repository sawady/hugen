{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Config (Config, load, width, height) where

import Control.Lens
import Data.Ini.Config
import qualified Data.Text.IO as Text
import Foreign.C (CInt)
import Utils

data Config = Config
  { _width :: CInt,
    _height :: CInt
  }

makeLenses ''Config

parser :: IniParser Config
parser = section "WINDOW" $ Config <$> fieldOf "width" number <*> fieldOf "height" number

load :: (Config -> IO a) -> IO a
load next =
  do
    content <- Text.readFile "resources/config.ini"
    case parseIniFile content parser of
      Left msg -> exitWith "Cannot parse config file" msg
      Right cfg -> next cfg
