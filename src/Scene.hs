{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Scene (Scene, new, name, sprites, onStart, update, addSprite, destroySprite) where

import Control.Lens
import Control.Monad.State (StateT, when)
import Data.Foldable (Foldable (foldl'))
import qualified Data.Map as M
import qualified Data.Set as S
import qualified SDL
import qualified Sprite

data Scene = Scene {_name :: String, _sprites :: M.Map String Sprite.Sprite}

instance Eq Scene where
  (==) :: Scene -> Scene -> Bool
  (==) s1 s2 = _name s1 == _name s2

makeLenses ''Scene

new :: String -> [Sprite.Sprite] -> Scene
new n =
  foldl' (flip addSprite) $
    Scene
      { _name = n,
        _sprites = M.empty
      }

addSprite :: Sprite.Sprite -> Scene -> Scene
addSprite sprite = sprites %~ M.insert (sprite ^. Sprite.name) sprite

destroySprite :: String -> Scene -> Scene
destroySprite k = sprites %~ M.delete k

onStart :: StateT Scene IO ()
onStart = do
  sprites . ix "sonic" %= Sprite.scale 2

update :: S.Set SDL.Keycode -> StateT Scene IO ()
update keys = do
  let speed = 15
  when (SDL.KeycodeUp `S.member` keys) $
    sprites . ix "sonic" . Sprite.posY -= speed
  when (SDL.KeycodeDown `S.member` keys) $
    sprites . ix "sonic" . Sprite.posY += speed
  when (SDL.KeycodeLeft `S.member` keys) $
    sprites . ix "sonic" . Sprite.posX -= speed
  when (SDL.KeycodeRight `S.member` keys) $
    sprites . ix "sonic" . Sprite.posX += speed
  sprites . ix "sonic" %= Sprite.nextFrame
