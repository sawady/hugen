{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Game (game)
import qualified SDL

main :: IO ()
main = game "Hugen" SDL.defaultWindow
