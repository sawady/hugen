{-# OPTIONS_GHC -Wno-orphans #-}

-- | QuickCheck generators for inputs and boxes.
module Gen
  ( Inputs (..),
    genPad,
    mirrorFighter,
  )
where

import Hugen.Box
import Hugen.Fighter (Fighter (..))
import Hugen.Fixed
import Hugen.Input
import Test.QuickCheck

instance Arbitrary Fx where
  arbitrary = Fx <$> choose (-200000, 200000)
  shrink (Fx a) = Fx <$> shrink a

instance Arbitrary Box where
  arbitrary = box <$> coord <*> coord <*> coord <*> coord
    where
      coord = fromPixels <$> choose (-100, 100)

instance Arbitrary Pad where
  arbitrary = genPad
  shrink p = [pad bs | bs <- shrinkList (const []) (heldButtons p)]

instance Arbitrary Facing where
  arbitrary = elements [FacingRight, FacingLeft]

-- | Directions most of the time, attack buttons now and then, like a
-- player mashing.
genPad :: Gen Pad
genPad = do
  dirs <-
    frequency
      [ (4, pure []),
        (2, elements [[KLeft], [KRight], [KUp], [KDown]]),
        (2, elements [[KDown, KLeft], [KDown, KRight], [KUp, KLeft], [KUp, KRight]]),
        (1, sublistOf [KUp, KDown, KLeft, KRight])
      ]
  btns <- frequency [(5, pure []), (2, (: []) <$> elements [KA, KB, KC, KX]), (1, sublistOf [KA, KB, KC, KX, KY, KZ])]
  pure (pad (dirs ++ btns))

-- | A match's worth of input for both players, as runs of held pads.
newtype Inputs = Inputs [(Pad, Pad)]

instance Show Inputs where
  show (Inputs xs) = "Inputs (" ++ show (length xs) ++ " ticks) " ++ show (take 40 xs)

instance Arbitrary Inputs where
  arbitrary = do
    n <- choose (1, 40)
    rs <- vectorOf n run
    pure (Inputs (concat rs))
    where
      run = do
        len <- choose (1, 16)
        p1 <- genPad
        p2 <- genPad
        pure (replicate len (p1, p2))
  shrink (Inputs xs) = Inputs <$> shrinkList (const []) xs

-- | The same fighter on the other side of a stage centered on 0.
mirrorFighter :: Fighter -> Fighter
mirrorFighter f = f {fX = negate (fX f), fFacing = flipFacing (fFacing f)}
