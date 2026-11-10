module Main (main) where

import qualified CombatSpec
import qualified CoreSpec
import qualified MatchSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  CoreSpec.spec
  MatchSpec.spec
  CombatSpec.spec
