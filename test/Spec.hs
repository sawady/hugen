module Main (main) where

import qualified CombatSpec
import qualified CoreSpec
import qualified EntitySpec
import qualified MatchSpec
import qualified ThrowSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  CoreSpec.spec
  MatchSpec.spec
  CombatSpec.spec
  EntitySpec.spec
  ThrowSpec.spec
