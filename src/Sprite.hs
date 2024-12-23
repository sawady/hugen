{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Sprite
  ( Sprite,
    load,
    loadFromSheet,
    render,
    setAnimationFrame,
    frameDurations,
    frameTimer,
    texture,
    position,
    posX,
    posY,
    dimensions,
    rect,
    destroy,
    nextFrame,
    scale,
  )
where

import Control.Lens
import Control.Lens.Internal.CTypes (Word32)
import Control.Monad.IO.Class (MonadIO)
import Foreign.C (CInt)
import Linear.V2 (_x, _y)
import qualified SDL
import qualified SDL.Image
import SDL.Vect (Point (..), V2 (..))

-- Sprite data type
data Sprite = Sprite
  { _texture :: SDL.Texture, -- The texture for the sprite
    _position :: Point V2 CInt, -- Position of the sprite (x, y)
    _dimensions :: V2 CInt, -- Width and height of the sprite
    _rect :: Maybe (SDL.Rectangle CInt), -- Source rectangle for animations
    _frameSize :: V2 CInt, -- Size of each frame (width, height)
    _maxFrames :: Int, -- Number of frames in the row
    _currentFrame :: Int, -- Current animation frame
    _frameDurations :: [Int], -- Duration (in milliseconds) of each frame
    _frameTimer :: Word32 -- Time elapsed for the current frame
  }

makeLenses ''Sprite

-- Load a sprite from an image file
load :: (MonadIO m) => SDL.Renderer -> FilePath -> m Sprite
load renderer filePath = do
  t <- SDL.Image.loadTexture renderer filePath
  SDL.TextureInfo {..} <- SDL.queryTexture t
  let d = V2 textureWidth textureHeight
  return $
    Sprite
      { _texture = t,
        _position = P (V2 0 0), -- Default position
        _dimensions = d,
        _rect = Nothing, -- Render the entire texture by default
        _maxFrames = 1,
        _frameSize = d,
        _currentFrame = 0,
        _frameDurations = [1],
        _frameTimer = 1
      }

-- Load a sprite from a spritesheet
loadFromSheet :: (MonadIO m) => SDL.Renderer -> FilePath -> [Int] -> m Sprite
loadFromSheet renderer filePath durations = do
  sprite <- load renderer filePath
  let totalFrames = length durations
      V2 tw th = sprite ^. dimensions
      fw = tw `div` fromIntegral totalFrames -- Frame width
      fh = th -- Frame height (full texture height)
      fSize = V2 fw fh
  return $
    setAnimationFrame 0 $
      sprite
        & frameSize .~ fSize
        & maxFrames .~ totalFrames
        & dimensions .~ fSize
        & frameDurations .~ durations
        & frameTimer .~ 0

nextFrame :: Word32 -> Sprite -> Sprite
nextFrame dt sprite =
  let timer = sprite ^. frameTimer + dt
      durations = sprite ^. frameDurations
      frame = sprite ^. currentFrame
      mx = min (sprite ^. maxFrames) (length durations) -- Safeguard
      frameDuration = durations !! frame
   in if timer >= fromIntegral frameDuration
        then setAnimationFrame ((frame + 1) `mod` mx) $
             sprite & frameTimer .~ (timer - fromIntegral frameDuration)
        else sprite & frameTimer .~ timer

-- Set the animation frame
setAnimationFrame :: Int -> Sprite -> Sprite
setAnimationFrame frame sprite =
  let V2 fw fh = sprite ^. frameSize
      maxColumns = sprite ^. maxFrames
      column = frame `mod` maxColumns
      row = frame `div` maxColumns
      x = fromIntegral column * fw
      y = fromIntegral row * fh
      r = SDL.Rectangle (P (SDL.V2 x y)) (SDL.V2 fw fh)
   in sprite & rect ?~ r & currentFrame .~ frame

-- Render a sprite on the screen
render :: (MonadIO m) => SDL.Renderer -> Sprite -> m ()
render renderer Sprite {..} = do
  let destRect = Just $ SDL.Rectangle _position _dimensions
  -- liftIO $ print _rect
  -- liftIO $ print _currentFrame
  -- liftIO $ print _maxFrames
  SDL.copy renderer _texture _rect destRect

scale :: Int -> Sprite -> Sprite
scale factor sprite =
  let fsize = sprite ^. frameSize
   in sprite & dimensions .~ fsize * fromIntegral factor

posX :: Lens' Sprite CInt
posX = position . _Wrapped' . _x

posY :: Lens' Sprite CInt
posY = position . _Wrapped' . _y

-- Destroy a sprite's texture
destroy :: (MonadIO m) => Sprite -> m ()
destroy s = SDL.destroyTexture (s ^. texture)