-- | Collision boxes.
--
-- Boxes are written relative to the fighter's axis (the point between its
-- feet), facing right, with y growing downward like on screen: a box from
-- @(-10, -60)@ to @(10, 0)@ is a 20x60 body standing on the axis.
--
-- Following MUGEN, an animation frame carries two lists of boxes:
--
-- * /hurtboxes/ (MUGEN's Clsn2): where the fighter can be hit;
-- * /hitboxes/ (MUGEN's Clsn1): where the fighter's attack hits.
module Hugen.Box
  ( Box (..),
    box,
    placeBox,
    overlaps,
    anyOverlap,
    mirrorBox,
    translateBox,
  )
where

import Hugen.Fixed
import Hugen.Input (Facing (..))

-- | An axis-aligned rectangle; @boxLeft < boxRight@ and @boxTop < boxBottom@.
data Box = Box
  { boxLeft :: !Fx,
    boxTop :: !Fx,
    boxRight :: !Fx,
    boxBottom :: !Fx
  }
  deriving (Eq, Show)

-- | Build a box from two opposite corners, in any order.
box :: Fx -> Fx -> Fx -> Fx -> Box
box x1 y1 x2 y2 = Box (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2)

-- | Reflect a box around the vertical axis.
mirrorBox :: Box -> Box
mirrorBox (Box l t r b) = Box (negate r) t (negate l) b

translateBox :: Fx -> Fx -> Box -> Box
translateBox dx dy (Box l t r b) = Box (l + dx) (t + dy) (r + dx) (b + dy)

-- | A box from a fighter's frame, in stage coordinates.
placeBox :: Facing -> (Fx, Fx) -> Box -> Box
placeBox facing (x, y) bx = translateBox x y oriented
  where
    oriented = case facing of
      FacingRight -> bx
      FacingLeft -> mirrorBox bx

-- | Strict overlap: boxes that only touch along an edge do not collide.
-- The test is symmetric and survives mirroring, so neither side of the
-- screen nor player 1 versus player 2 gets an advantage.
overlaps :: Box -> Box -> Bool
overlaps a b =
  boxLeft a < boxRight b
    && boxLeft b < boxRight a
    && boxTop a < boxBottom b
    && boxTop b < boxBottom a

anyOverlap :: [Box] -> [Box] -> Bool
anyOverlap as bs = or [overlaps a b | a <- as, b <- bs]
