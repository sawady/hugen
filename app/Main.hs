module Main (main) where

import Game (game)
import qualified SDL

main :: IO ()
main = game SDL.defaultWindow
