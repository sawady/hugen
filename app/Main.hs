{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Game (game)
import qualified SDL

main :: IO ()
main = game "My SDL Application" SDL.defaultWindow
