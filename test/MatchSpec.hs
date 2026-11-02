-- | Whole-match tests: scripted scenarios with Blocky and properties over
-- random inputs (determinism, symmetry, invariants).
module MatchSpec (spec) where

import Gen
import Hugen.Camera
import Hugen.Character
import Hugen.Common
import Hugen.Example.Blocky
import Hugen.Example.Demo
import Hugen.Expr (MoveType (..), Stance (..))
import Hugen.Fighter
import Hugen.Input
import Hugen.Match
import Test.Hspec
import Test.QuickCheck

start :: Match
start = newMatch demoDef

-- | Run player 1's pads against an idle player 2.
p1Only :: [Pad] -> Match
p1Only ps = simulate demoDef (zip ps (repeat noInput)) start

run :: Inputs -> Match
run (Inputs xs) = simulate demoDef xs start

spec :: Spec
spec = do
  describe "scenarios" $ do
    it "standing still changes nothing but the clocks" $ do
      let m = p1Only (replicate 100 noInput)
      fX (mP1 m) `shouldBe` fX (mP1 start)
      fState (mP1 m) `shouldBe` stStand
      fLife (mP2 m) `shouldBe` 1000

    it "walks forward faster than backward" $ do
      let fwd = fX (mP1 (p1Only (runs [(20, pad [KRight])]))) - fX (mP1 start)
          back = fX (mP1 start) - fX (mP1 (p1Only (runs [(20, pad [KLeft])])))
      fwd `shouldSatisfy` (> back)
      back `shouldSatisfy` (> 0)

    it "jumps and lands back in the standing state" $ do
      let up = p1Only (runs [(5, pad [KUp])])
          landed = p1Only (runs [(5, pad [KUp]), (80, noInput)])
      fY (mP1 up) `shouldSatisfy` (< 0)
      fStance (mP1 up) `shouldBe` Airborne
      fY (mP1 landed) `shouldBe` 0
      fState (mP1 landed) `shouldBe` stStand

    it "a jab out of range does not hit" $ do
      let m = p1Only (runs [(1, pad [KA]), (30, noInput)])
      fLife (mP2 m) `shouldBe` 1000

    it "jab cancelled into the rush deals both hits" $ do
      let m = p1Only comboScript
      fLife (mP2 m) `shouldBe` 1000 - hdDamage hitDef - 90
      fLife (mP1 m) `shouldBe` 1000

    it "every attack in the demo connects exactly once" $ do
      let m = simulate demoDef demoScript start
      fLife (mP2 m) `shouldBe` 1000 - 30 - 90 - 50 - 110

    it "freezes both fighters on impact" $ do
      let ticks = scanl (flip (step demoDef)) start (zip comboScript (repeat noInput))
      firstHit <- case [m | m <- ticks, fLife (mP2 m) < 1000] of
        (m : _) -> pure m
        [] -> expectationFailure "the jab never hit" >> pure start
      fPause (mP1 firstHit) `shouldSatisfy` (> 0)
      fPause (mP2 firstHit) `shouldSatisfy` (> 0)
      fMoveType (mP2 firstHit) `shouldBe` BeingHit

    it "turns around after jumping over the opponent" $ do
      let m = p1Only (runs [(40, pad [KRight]), (4, pad [KUp, KRight]), (80, noInput)])
      fX (mP1 m) `shouldSatisfy` (> fX (mP2 m))
      fFacing (mP1 m) `shouldBe` FacingLeft

    it "ends the round when a fighter runs out of life" $ do
      let weak = blocky {charConsts = (charConsts blocky) {cLife = 50}}
          md = demoDef {mdP2 = weak}
          m = simulate md (zip comboScript (repeat noInput)) (newMatch md)
      mOutcome m `shouldBe` Just (Winner P1)
      fLife (mP2 m) `shouldBe` 0

  describe "properties" $ do
    it "is deterministic: the same inputs give the same match" $
      property $ \i -> run i == run i

    it "is a fold: running a prefix and then the rest is the same as running all" $
      property $ \(Inputs xs) (Inputs ys) ->
        simulate demoDef (xs ++ ys) start == simulate demoDef ys (simulate demoDef xs start)

    it "favors no side: the mirrored match is the mirror image" $
      property $ \(Inputs xs) ->
        let a = simulate demoDef xs start
            b = simulate demoDef (mirrorInputs xs) start
         in (mP1 b, mP2 b, camX (mCamera b))
              === (mirrorFighter (mP2 a), mirrorFighter (mP1 a), negate (camX (mCamera a)))

    it "keeps life in range and fighters inside the stage and above the floor" $
      property $ \i ->
        let m = run i
            st = mdStage demoDef
            ok f =
              fLife f >= 0
                && fLife f <= 1000
                && fX f >= stageLeft st
                && fX f <= stageRight st
                && fY f <= 0
         in ok (mP1 m) && ok (mP2 m)
