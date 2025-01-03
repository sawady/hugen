{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module GameInput (GameInput, create, activeKeys, mousePosition, mouseButtons, addKeyboardEvent, pressed, addMouseMovementEvent, addMouseButtonEvent, mouseButtonPressed) where

import Control.Lens
import qualified Data.Set as Set
import qualified SDL
import Data.Int (Int32)

data GameInput = GameInput
  { _activeKeys :: Set.Set SDL.Keycode,
    _mousePosition :: SDL.Point SDL.V2 Int32,
    _mouseButtons :: Set.Set SDL.MouseButton
  }

makeLenses ''GameInput

create :: GameInput
create = GameInput
  { _activeKeys = Set.empty,
    _mousePosition = SDL.P (SDL.V2 0 0),
    _mouseButtons = Set.empty
  }

-- Keyboard Event Handling
addKeyboardEvent :: SDL.KeyboardEventData -> GameInput -> GameInput
addKeyboardEvent keyboardEvent =
  case SDL.keyboardEventKeyMotion keyboardEvent of
    SDL.Pressed -> activeKeys %~ Set.insert keycode
    SDL.Released -> activeKeys %~ Set.delete keycode
  where
    keycode = SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent)

pressed :: SDL.Keycode -> GameInput -> Bool
pressed kcode g = kcode `Set.member` _activeKeys g

-- Mouse Movement Event Handling
addMouseMovementEvent :: SDL.MouseMotionEventData -> GameInput -> GameInput
addMouseMovementEvent motionEvent = mousePosition .~ SDL.mouseMotionEventPos motionEvent

-- Mouse Button Event Handling
addMouseButtonEvent :: SDL.MouseButtonEventData -> GameInput -> GameInput
addMouseButtonEvent buttonEvent =
  case SDL.mouseButtonEventMotion buttonEvent of
    SDL.Pressed -> mouseButtons %~ Set.insert button
    SDL.Released -> mouseButtons %~ Set.delete button
  where
    button = SDL.mouseButtonEventButton buttonEvent

-- Check if a mouse button is pressed
mouseButtonPressed :: SDL.MouseButton -> GameInput -> Bool
mouseButtonPressed button g = button `Set.member` _mouseButtons g
