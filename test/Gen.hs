{-# OPTIONS_GHC -Wno-orphans #-}

-- | QuickCheck generators for inputs and boxes.
module Gen
  ( Inputs (..),
    Brawl (..),
    Barrage (..),
    Grapple (..),
    genPad,
    genHitDef,
    genThrowDef,
    mirrorFighter,
    isMirrorOf,
    sideEntities,
    fireballMotion,
  )
where

import Hugen.Box
import Hugen.Camera (Camera (..))
import Hugen.Character (EntityKind, HitDef (..), ThrowDef (..), hitDef, throwDef)
import Hugen.Fighter (Fighter (..))
import Hugen.Fixed
import Hugen.Input
import Hugen.Match
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

-- | Any throw that goes on to the given state.
genThrowDef :: Int -> Gen ThrowDef
genThrowDef st = do
  damage <- choose (0, 200)
  holdTicks <- choose (1, 40)
  escape <- choose (0, 40)
  ox <- fromPixels <$> choose (-40, 40)
  oy <- fromPixels <$> choose (-40, 10)
  lx <- fromPixels <$> choose (-6, 6)
  ly <- fromPixels <$> choose (-8, 0)
  pure
    (throwDef st)
      { tdDamage = damage,
        tdHold = holdTicks,
        tdEscape = escape,
        tdOffset = (ox, oy),
        tdLaunch = (lx, ly)
      }

-- | Input for two fighters that start in range of a throw: throws, often
-- answered by the other side pressing throw a few ticks later (an escape,
-- in or out of the window), with guarding, crouching, attacks and steps in
-- between.
newtype Grapple = Grapple [(Pad, Pad)]

instance Show Grapple where
  show (Grapple xs) = "Grapple (" ++ show (length xs) ++ " ticks) " ++ show (take 40 xs)

instance Arbitrary Grapple where
  arbitrary = do
    n <- choose (1, 30)
    Grapple . concat <$> vectorOf n segment
    where
      segment =
        frequency
          [ (4, throwBy True),
            (4, throwBy False),
            (1, pure [(pad [KC], pad [KC])]),
            (4, held)
          ]
      -- one side throws; the other holds a pad and may press throw later
      throwBy p1 = do
        other <- genGrapplePad
        answer <- frequency [(1, pure Nothing), (2, Just <$> choose (0, 14))]
        wait <- choose (0, 30)
        let thrower = pad [KC] : replicate (15 + wait) noInput
            defender = case answer of
              Nothing -> replicate (length thrower) other
              Just k -> take (length thrower) (replicate k other ++ [pad [KC]] ++ repeat other)
        pure (if p1 then zip thrower defender else zip defender thrower)
      held = do
        len <- choose (1, 12)
        p1 <- genGrapplePad
        p2 <- genGrapplePad
        pure (replicate len (p1, p2))

genGrapplePad :: Gen Pad
genGrapplePad =
  frequency
    [ (3, pure noInput),
      (2, pure (pad [KLeft])),
      (2, pure (pad [KRight])),
      (2, pure (pad [KDown, KLeft])),
      (2, pure (pad [KDown, KRight])),
      (1, pure (pad [KUp])),
      (2, pure (pad [KA])),
      (2, pure (pad [KB])),
      (2, pure (pad [KC]))
    ]

-- | The same fighter on the other side of a stage centered on 0.
mirrorFighter :: Fighter -> Fighter
mirrorFighter f = f {fX = negate (fX f), fFacing = flipFacing (fFacing f)}

-- | A side's entities in creation order, without their ids.
sideEntities :: Side -> Match -> [(EntityKind, Fighter)]
sideEntities s m = [(eKind e, eBody e) | e <- mEntities m, eOwner e == s]

-- | @b `isMirrorOf` a@: the fighters swapped and reflected, the camera
-- reflected, and each side's entities those of the other side, reflected and
-- in the same order. Ids are left out on purpose: when both sides create an
-- entity on the same tick, player 1's gets the lower id, so ids interleave
-- differently in the mirrored match. Nothing else may depend on them.
isMirrorOf :: Match -> Match -> Property
isMirrorOf b a =
  (mP1 b, mP2 b, camX (mCamera b), sideEntities P1 b, sideEntities P2 b, mNextId b)
    === ( mirrorFighter (mP2 a),
          mirrorFighter (mP1 a),
          negate (camX (mCamera a)),
          map mirrorE (sideEntities P2 a),
          map mirrorE (sideEntities P1 a),
          mNextId a
        )
  where
    mirrorE (k, f) = (k, mirrorFighter f)

-- | Blocky's fireball motion (down, down-back, back + A) for a fighter whose
-- back is the given key.
fireballMotion :: Button -> [Pad]
fireballMotion backKey =
  concat [replicate 2 (pad [KDown]), replicate 2 (pad [KDown, backKey]), [pad [backKey, KA]]]

-- | Input for two fighters at the usual distance that throw fireballs at
-- each other: often one at a time, sometimes both at once, with walking,
-- jumping, guarding and attacking in between.
newtype Barrage = Barrage [(Pad, Pad)]

instance Show Barrage where
  show (Barrage xs) = "Barrage (" ++ show (length xs) ++ " ticks) " ++ show (take 40 xs)

instance Arbitrary Barrage where
  arbitrary = do
    n <- choose (1, 30)
    Barrage . concat <$> vectorOf n segment
    where
      segment =
        frequency
          [ (3, throwBy True),
            (3, throwBy False),
            (1, pure (zip (fireballMotion KLeft) (fireballMotion KRight))),
            (4, held genBrawlPad),
            (1, held (elements [pad [KUp], pad [KUp, KLeft], pad [KUp, KRight]]))
          ]
      -- the other side holds a pad meanwhile
      throwBy p1 = do
        other <- genBrawlPad
        wait <- choose (0, 20)
        let motion = fireballMotion (if p1 then KLeft else KRight) ++ replicate wait noInput
        pure (if p1 then zip motion (repeat other) else zip (repeat other) motion)
      held g = do
        len <- choose (1, 16)
        p1 <- g
        p2 <- g
        pure (replicate len (p1, p2))
  shrink (Barrage xs) = Barrage <$> shrinkList (const []) xs
