module Main (main) where

import qualified CoreSpec
import qualified MatchSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  CoreSpec.spec
  MatchSpec.spec
