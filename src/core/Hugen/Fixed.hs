-- | Fixed-point numbers for gameplay math.
--
-- The simulation never uses floating point: every position, velocity and
-- distance is an 'Fx', an integer counting 1/256ths of a pixel. Integer
-- arithmetic gives the same result on every machine and compiler, which is
-- what replays and (eventually) rollback netcode need.
--
-- Literals work as expected thanks to the 'Num' and 'Fractional' instances:
-- @2.5 :: Fx@ is exactly @Fx 640@.
--
-- Multiplication and division round toward zero ('quot'), so that
-- @negate a * b == negate (a * b)@. That keeps the engine mirror-symmetric:
-- a fighter facing left behaves exactly like one facing right, reflected.
module Hugen.Fixed
  ( Fx (..),
    fracBits,
    unit,
    fromPixels,
    toPixels,
    toDouble,
    halve,
  )
where

import Data.Bits (shiftL)

-- | A fixed-point number with 'fracBits' fractional bits.
newtype Fx = Fx Int
  deriving (Eq, Ord)

-- | Number of fractional bits.
fracBits :: Int
fracBits = 8

-- | The raw value of @1 :: Fx@.
unit :: Int
unit = 1 `shiftL` fracBits

instance Show Fx where
  show = show . toDouble

instance Num Fx where
  Fx a + Fx b = Fx (a + b)
  Fx a - Fx b = Fx (a - b)
  Fx a * Fx b = Fx ((a * b) `quot` unit)
  negate (Fx a) = Fx (negate a)
  abs (Fx a) = Fx (abs a)
  signum (Fx a) = Fx (signum a * unit)
  fromInteger n = Fx (fromInteger n * unit)

instance Fractional Fx where
  Fx a / Fx b = Fx ((a * unit) `quot` b)
  fromRational r = Fx (round (r * fromIntegral unit))

-- | Whole pixels to 'Fx'.
fromPixels :: Int -> Fx
fromPixels n = Fx (n * unit)

-- | 'Fx' to whole pixels, rounding toward zero. Only for presentation.
toPixels :: Fx -> Int
toPixels (Fx a) = a `quot` unit

-- | Only for presentation and debugging, never for gameplay.
toDouble :: Fx -> Double
toDouble (Fx a) = fromIntegral a / fromIntegral unit

-- | Half, rounding toward zero (sign-symmetric).
halve :: Fx -> Fx
halve (Fx a) = Fx (a `quot` 2)
