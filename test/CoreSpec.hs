-- | Unit tests and properties of the building blocks: fixed point, input,
-- commands, boxes and animations.
module CoreSpec (spec) where

import Gen ()
import Hugen.Anim
import Hugen.Box
import Hugen.Character (validate)
import Hugen.Command
import Hugen.Example.Blocky (blocky)
import Hugen.Fixed
import Hugen.Input
import Test.Hspec
import Test.QuickCheck

-- | Feed pads (oldest first) to an empty history.
history :: Facing -> [Pad] -> History
history facing = foldl' (\h p -> recordInput (relative facing p) h) (emptyHistory 120)

-- | Hold each pad for the given ticks.
held :: [(Int, [Button])] -> [Pad]
held = concatMap (\(n, bs) -> replicate n (pad bs))

qcfA :: Command
qcfA = command "qcf_a" [Press Down, Press DownFwd, Press Fwd, Buttons [BtnA]]

matchesR :: Command -> [Pad] -> Bool
matchesR c = (`matches` c) . history FacingRight

spec :: Spec
spec = do
  describe "Fx" $ do
    it "reads decimal literals exactly" $
      (2.5 :: Fx) `shouldBe` Fx 640
    it "multiplies with the sign kept symmetric" $
      property $ \a b -> negate a * b == negate (a * (b :: Fx))
    it "adds and subtracts exactly" $
      property $ \a b -> (a + b) - b == (a :: Fx)

  describe "Input" $ do
    it "reads right as forward when facing right" $
      riDir (relative FacingRight (pad [KRight])) `shouldBe` Fwd
    it "reads right as back when facing left" $
      riDir (relative FacingLeft (pad [KRight])) `shouldBe` Back
    it "cancels opposite directions" $
      riDir (relative FacingRight (pad [KLeft, KRight, KDown])) `shouldBe` Down
    it "facing left is facing right with the pad mirrored" $
      property $ \p -> relative FacingLeft p == relative FacingRight (mirrorPad p)
    it "round-trips through bits" $
      property $ \p -> padFromBits (padBits p) == p

  describe "Command" $ do
    it "recognizes a quarter circle forward + A on the tick of the button" $
      matchesR qcfA (held [(3, [KDown]), (3, [KDown, KRight]), (3, [KRight]), (1, [KRight, KA])])
        `shouldBe` True
    it "accepts the button pressed together with the last direction" $
      matchesR qcfA (held [(2, [KDown]), (2, [KDown, KRight]), (1, [KRight, KA])]) `shouldBe` True
    it "does not fire before the button" $
      matchesR qcfA (held [(3, [KDown]), (3, [KDown, KRight]), (3, [KRight])]) `shouldBe` False
    it "does not fire again on the next tick (the buffer does that)" $
      matchesR qcfA (held [(3, [KDown]), (3, [KDown, KRight]), (3, [KRight]), (2, [KRight, KA])])
        `shouldBe` False
    it "is too slow when the motion takes longer than the command time" $
      matchesR qcfA (held [(1, [KDown]), (10, [KDown, KRight]), (10, [KRight]), (1, [KA])])
        `shouldBe` False
    it "measures time from the first press, not from holding" $
      matchesR qcfA (held [(40, [KDown]), (2, [KDown, KRight]), (2, [KRight]), (1, [KA])])
        `shouldBe` True
    it "tolerates sloppy input in between" $
      matchesR qcfA (held [(2, [KDown]), (2, [KRight]), (2, [KDown, KRight]), (2, [KRight]), (1, [KA])])
        `shouldBe` True
    it "is mirrored when facing left" $
      matches
        (history FacingLeft (held [(2, [KDown]), (2, [KDown, KLeft]), (2, [KLeft]), (1, [KA])]))
        qcfA
        `shouldBe` True
    it "needs two separate presses for F, F" $ do
      let dash = command "ff" [Press Fwd, Press Fwd]
      matchesR dash (held [(3, [KRight]), (2, []), (1, [KRight])]) `shouldBe` True
      matchesR dash (held [(6, [KRight])]) `shouldBe` False
    it "checks the charge time" $ do
      let sonicBoom = command "charge" [Charge Back 30, Press Fwd, Buttons [BtnA]]
      matchesR sonicBoom (held [(40, [KLeft]), (2, [KRight]), (1, [KRight, KA])]) `shouldBe` True
      matchesR sonicBoom (held [(10, [KLeft]), (2, [KRight]), (1, [KRight, KA])]) `shouldBe` False
    it "groups buttons pressed close together" $ do
      let ab = command "ab" [Buttons [BtnA, BtnB]]
      matchesR ab (held [(1, [KA, KB])]) `shouldBe` True
      matchesR ab (held [(1, [KA]), (1, [KA, KB])]) `shouldBe` True
      matchesR ab (held [(5, [KA]), (1, [KA, KB])]) `shouldBe` False
    it "requires the held direction on the tick of the button" $ do
      let fwdA = command "fwd_a" [Hold Fwd, Buttons [BtnA]]
      matchesR fwdA (held [(5, [KRight]), (1, [KRight, KA])]) `shouldBe` True
      matchesR fwdA (held [(5, [KRight]), (1, [KA])]) `shouldBe` False
    it "never completes a button command without a fresh press" $
      property $ \pads ->
        let h = history FacingRight pads
            fresh = BtnA `elem` riBtns (inputAt h 0) && BtnA `notElem` riBtns (inputAt h 1)
         in not (matches h qcfA) || fresh
    it "keeps a completed command active for its buffer time" $ do
      let h = history FacingRight (held [(2, [KDown]), (2, [KDown, KRight]), (2, [KRight]), (1, [KA])])
          b0 = updateBuffer False (recognize [qcfA] h) emptyBuffer
          later k = iterate (updateBuffer False []) b0 !! k
      isActive "qcf_a" (later (defaultBuffer - 1)) `shouldBe` True
      isActive "qcf_a" (later defaultBuffer) `shouldBe` False
    it "does not age the buffer while frozen in hit pause" $ do
      let b0 = updateBuffer False [qcfA] emptyBuffer
          frozen = iterate (updateBuffer True []) b0 !! 20
      isActive "qcf_a" frozen `shouldBe` True

  describe "Box" $ do
    it "overlap is symmetric" $
      property $ \a b -> overlaps a b == overlaps b (a :: Box)
    it "overlap survives mirroring" $
      property $ \a b -> overlaps (mirrorBox a) (mirrorBox b) == overlaps a b
    it "boxes that only touch do not overlap" $
      overlaps (box 0 0 10 10) (box 10 0 20 10) `shouldBe` False
    it "places a box mirrored when facing left" $
      placeBox FacingLeft (100, 0) (box 10 (-20) 30 0) `shouldBe` box 70 (-20) 90 0

  describe "Anim" $ do
    let a = anim [frame (SpriteRef 0 0) 2 [], frame (SpriteRef 0 1) 3 []]
        positions = iterate (advanceAnim a) startAnim
    it "shows each frame for its duration and loops" $
      map apElem (take 7 positions) `shouldBe` [0, 0, 1, 1, 1, 0, 0]
    it "reports the end of the animation with animTime 0" $
      map (animTimeLeft a) (take 6 positions) `shouldBe` [-5, -4, -3, -2, -1, 0]

  describe "Blocky" $
    it "is a well-formed character" $
      validate blocky `shouldBe` []
