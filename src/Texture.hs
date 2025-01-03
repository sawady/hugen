module Texture (load) where

import qualified SDL
import qualified SDL.Image
import Utils (catching)

load :: SDL.Renderer -> FilePath -> IO SDL.Texture
load renderer filePath = SDL.Image.loadTexture renderer filePath `catching` "Error loading Texture"
