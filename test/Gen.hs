{-# OPTIONS_GHC -Wno-orphans #-}

-- | QuickCheck generators for inputs and boxes.
module Gen
  ( Inputs (..),
    Brawl (..),
    genPad,
    genHitDef,
    mirrorFighter,
  )
where

import Hugen.Box
import Hugen.Character (HitDef (..), hitDef)
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

-- | Input for two fighters that start in range: mostly attacks, holding
-- away and crouching, so that guards, chip damage and knockdowns show up.
newtype Brawl = Brawl [(Pad, Pad)]

instance Show Brawl where
  show (Brawl xs) = "Brawl (" ++ show (length xs) ++ " ticks) " ++ show (take 40 xs)

instance Arbitrary Brawl where
  arbitrary = do
    n <- choose (1, 40)
    rs <- vectorOf n run
    pure (Brawl (concat rs))
    where
      run = do
        len <- choose (1, 12)
        pair <- frequency [(2, (,) <$> genBrawlPad <*> genBrawlPad), (3, attackVsGuard)]
        pure (replicate len pair)
      -- one side attacks while the other holds away, from the starting sides
      attackVsGuard = do
        attack <- genBrawlButtons
        p1Attacks <- arbitrary
        crouch <- arbitrary
        let low = [KDown | crouch]
        pure $
          if p1Attacks
            then (pad attack, pad (KRight : low))
            else (pad (KLeft : low), pad attack)
  shrink (Brawl xs) = Brawl <$> shrinkList (const []) xs

genBrawlPad :: Gen Pad
genBrawlPad = do
  dirs <-
    frequency
      [ (3, pure []),
        (3, pure [KLeft]),
        (3, pure [KRight]),
        (3, pure [KDown, KLeft]),
        (3, pure [KDown, KRight]),
        (2, pure [KDown]),
        (1, pure [KUp])
      ]
  btns <- genBrawlButtons
  pure (pad (dirs ++ btns))

genBrawlButtons :: Gen [Button]
genBrawlButtons = frequency [(4, pure []), (3, pure [KA]), (3, pure [KB])]

-- | Any attack, within the life of a default character.
genHitDef :: Gen HitDef
genHitDef = do
  damage <- choose (0, 200)
  stun <- choose (1, 30)
  pause <- choose (0, 16)
  push <- fromPixels <$> choose (0, 8)
  lift <- fromPixels <$> choose (-8, -1)
  level <- arbitraryBoundedEnum
  strength <- arbitraryBoundedEnum
  chip <- choose (0, 40)
  guardStun <- choose (1, 30)
  pure
    hitDef
      { hdDamage = damage,
        hdHitStun = stun,
        hdHitPause = pause,
        hdPushback = push,
        hdAirLift = lift,
        hdLevel = level,
        hdStrength = strength,
        hdChip = chip,
        hdGuardStun = guardStun
      }

-- | The same fighter on the other side of a stage centered on 0.
mirrorFighter :: Fighter -> Fighter
mirrorFighter f = f {fX = negate (fX f), fFacing = flipFacing (fFacing f)}
