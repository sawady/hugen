{-# LANGUAGE InstanceSigs #-}
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
    name,
    position,
    posX,
    posY,
    dimensions,
    rect,
    destroy,
    nextFrame,
    scale,
    scaleTo,
    resizeSprite,
  )
where

import Control.Lens
import Control.Monad.IO.Class (MonadIO (liftIO))
import Foreign.C (CInt)
import Linear.V2 (_x, _y)
import qualified SDL
import SDL.Vect (Point (..), V2 (..))
import qualified Texture

-- Sprite data type
data Sprite = Sprite
  { _name :: String,
    _texture :: SDL.Texture, -- The texture for the sprite
    _position :: Point V2 CInt, -- Position of the sprite (x, y)
    _dimensions :: V2 CInt, -- Width and height of the sprite
    _rect :: Maybe (SDL.Rectangle CInt), -- Source rectangle for animations
    _frameSize :: V2 CInt, -- Size of each frame (width, height)
    _maxFrames :: Int, -- Number of frames in the row
    _currentFrame :: Int, -- Current animation frame
    _frameDurations :: [Int], -- Duration (in milliseconds) of each frame
    _frameTimer :: Int -- Time elapsed for the current frame
  }

makeLenses ''Sprite

instance Eq Sprite where
  (==) :: Sprite -> Sprite -> Bool
  (==) s1 s2 = _name s1 == _name s2

-- Load a sprite from an image file
load :: (MonadIO m) => SDL.Renderer -> String -> FilePath -> m Sprite
load renderer n filePath = do
  t <- liftIO $ Texture.load renderer filePath
  SDL.TextureInfo {..} <- SDL.queryTexture t
  let d = V2 textureWidth textureHeight
  return $
    Sprite
      { _name = n,
        _texture = t,
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
loadFromSheet :: (MonadIO m) => SDL.Renderer -> String -> FilePath -> [Int] -> m Sprite
loadFromSheet renderer n filePath durations = do
  sprite <- Sprite.load renderer n filePath
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

nextFrame :: Sprite -> Sprite
nextFrame sprite =
  let timer = sprite ^. frameTimer + 1
      durations = sprite ^. frameDurations
      frame = sprite ^. currentFrame
      mx = min (sprite ^. maxFrames) (length durations) -- Safeguard
      frameDuration = durations !! frame
   in if timer >= frameDuration
        then
          setAnimationFrame ((frame + 1) `mod` mx) $
            sprite & frameTimer .~ (timer - frameDuration)
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

scaleTo :: CInt -> CInt -> Sprite -> Sprite
scaleTo newWidth newHeight sprite =
  sprite & dimensions .~ V2 newWidth newHeight

resizeSprite :: Double -> Double -> Sprite -> Sprite
resizeSprite scaleX scaleY sprite =
  let V2 width height = sprite ^. dimensions
      newWidth = round (fromIntegral width * scaleX)
      newHeight = round (fromIntegral height * scaleY)
      P (V2 posX' posY') = sprite ^. position
      newPosX = round (fromIntegral posX' * scaleX)
      newPosY = round (fromIntegral posY' * scaleY)
   in sprite
        & dimensions .~ V2 newWidth newHeight
        & position .~ P (V2 newPosX newPosY)

-- Destroy a sprite's texture
destroy :: (MonadIO m) => Sprite -> m ()
destroy s = SDL.destroyTexture (s ^. texture)