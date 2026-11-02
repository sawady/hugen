-- | Keyboard to 'Pad'. The only place where SDL keys meet the core's input.
module Hugen.SDL.Keyboard
  ( Layout,
    player1Layout,
    padFrom,
  )
where

import Hugen.Input
import qualified SDL

-- | Which key drives which button.
type Layout = [(SDL.Scancode, Button)]

-- | Arrows to move; Z X C for A B C; A S D for X Y Z; Enter for start.
player1Layout :: Layout
player1Layout =
  [ (SDL.ScancodeUp, KUp),
    (SDL.ScancodeDown, KDown),
    (SDL.ScancodeLeft, KLeft),
    (SDL.ScancodeRight, KRight),
    (SDL.ScancodeZ, KA),
    (SDL.ScancodeX, KB),
    (SDL.ScancodeC, KC),
    (SDL.ScancodeA, KX),
    (SDL.ScancodeS, KY),
    (SDL.ScancodeD, KZ),
    (SDL.ScancodeReturn, KStart)
  ]

-- | Read a pad from SDL's keyboard state.
padFrom :: Layout -> (SDL.Scancode -> Bool) -> Pad
padFrom layout down = pad [b | (k, b) <- layout, down k]
