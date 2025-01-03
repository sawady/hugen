{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Scene (Scene, new, name, sprites, onStart, update, addSprite, destroySprite) where

import Control.Lens
import Control.Monad.State (StateT, when)
import Data.Foldable (Foldable (foldl'))
import qualified Data.Map as Map
import GameInput (GameInput, pressed)
import qualified SDL
import qualified Sprite

data Scene = Scene {_name :: String, _sprites :: Map.Map String Sprite.Sprite}

instance Eq Scene where
  (==) :: Scene -> Scene -> Bool
  (==) s1 s2 = _name s1 == _name s2

makeLenses ''Scene

new :: String -> [Sprite.Sprite] -> Scene
new n =
  foldl' (flip addSprite) $
    Scene
      { _name = n,
        _sprites = Map.empty
      }

addSprite :: Sprite.Sprite -> Scene -> Scene
addSprite sprite = sprites %~ Map.insert (sprite ^. Sprite.name) sprite

destroySprite :: String -> Scene -> Scene
destroySprite k = sprites %~ Map.delete k

onStart :: StateT Scene IO ()
onStart = do
  sprites . ix "sonic" %= Sprite.scale 2

update :: GameInput -> StateT Scene IO ()
update input = do
  let speed = 15
  when (pressed SDL.KeycodeUp input) $
    sprites . ix "sonic" . Sprite.posY -= speed
  when (pressed SDL.KeycodeDown input) $
    sprites . ix "sonic" . Sprite.posY += speed
  when (pressed SDL.KeycodeLeft input) $
    sprites . ix "sonic" . Sprite.posX -= speed
  when (pressed SDL.KeycodeRight input) $
    sprites . ix "sonic" . Sprite.posX += speed
  sprites . ix "sonic" %= Sprite.nextFrame
