{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import SDL (($=))
import qualified SDL

isPressed :: SDL.Keycode -> SDL.Event -> Bool
isPressed keycode event =
  case SDL.eventPayload event of
    SDL.KeyboardEvent e ->
      SDL.keyboardEventKeyMotion e == SDL.Pressed
        && SDL.keysymKeycode (SDL.keyboardEventKeysym e) == keycode
    _ -> False

qPressed :: [SDL.Event] -> Bool
qPressed = any (isPressed SDL.KeycodeQ)

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "My SDL Application" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  appLoop renderer
  SDL.destroyWindow window

appLoop :: SDL.Renderer -> IO ()
appLoop renderer = do
  events <- SDL.pollEvents

  SDL.rendererDrawColor renderer $= SDL.V4 0 0 255 255
  SDL.clear renderer
  SDL.present renderer

  unless (qPressed events) (appLoop renderer)
