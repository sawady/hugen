-- | Controller input, as the simulation sees it.
--
-- The shell (SDL, a replay file, a network peer, a test) produces one 'Pad'
-- per player per tick. A 'Pad' is absolute: it says "left", not "back". The
-- core turns it into a facing-relative 'RelInput' before matching commands,
-- so a character's move list is written once and works on both sides.
module Hugen.Input
  ( -- * Absolute input
    Button (..),
    Pad,
    noInput,
    pad,
    press,
    isHeld,
    heldButtons,
    mirrorPad,
    padBits,
    padFromBits,

    -- * Facing
    Facing (..),
    facingSign,
    flipFacing,

    -- * Relative input
    Dir (..),
    Btn (..),
    RelInput (..),
    relative,
    dirHas,
    allDirs,
    allBtns,
  )
where

import Data.Bits (clearBit, setBit, testBit, (.&.))
import Data.Word (Word16)

-- | Physical buttons, MUGEN-style: four directions, six attack buttons and
-- start.
data Button
  = KUp
  | KDown
  | KLeft
  | KRight
  | KA
  | KB
  | KC
  | KX
  | KY
  | KZ
  | KStart
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The state of one controller on one tick: a bit per held 'Button'.
-- Compact on purpose, a replay is a list of these.
newtype Pad = Pad Word16
  deriving (Eq, Ord)

instance Show Pad where
  show p = "pad " ++ show (heldButtons p)

noInput :: Pad
noInput = Pad 0

pad :: [Button] -> Pad
pad = foldl' (flip press) noInput

press :: Button -> Pad -> Pad
press b (Pad w) = Pad (setBit w (fromEnum b))

isHeld :: Button -> Pad -> Bool
isHeld b (Pad w) = testBit w (fromEnum b)

heldButtons :: Pad -> [Button]
heldButtons p = [b | b <- [minBound .. maxBound], isHeld b p]

padBits :: Pad -> Word16
padBits (Pad w) = w

padFromBits :: Word16 -> Pad
padFromBits w = Pad (w .&. mask)
  where
    mask = foldl' setBit 0 (map fromEnum [minBound .. maxBound :: Button])

-- | Swap left and right.
mirrorPad :: Pad -> Pad
mirrorPad p@(Pad w) = Pad (set KRight (isHeld KLeft p) (set KLeft (isHeld KRight p) w))
  where
    set b on x = (if on then setBit else clearBit) x (fromEnum b)

-- | Which way a fighter looks. Velocities and boxes are written facing right
-- and mirrored when facing left.
data Facing = FacingRight | FacingLeft
  deriving (Eq, Ord, Show, Enum, Bounded)

facingSign :: (Num a) => Facing -> a
facingSign FacingRight = 1
facingSign FacingLeft = -1

flipFacing :: Facing -> Facing
flipFacing FacingRight = FacingLeft
flipFacing FacingLeft = FacingRight

-- | The eight directions plus neutral, relative to facing ("numpad
-- notation" 1-9 in fighting game communities).
data Dir
  = Neutral
  | Up
  | Down
  | Back
  | Fwd
  | UpBack
  | UpFwd
  | DownBack
  | DownFwd
  deriving (Eq, Ord, Show, Enum, Bounded)

allDirs :: [Dir]
allDirs = [minBound .. maxBound]

-- | Attack buttons and start.
data Btn = BtnA | BtnB | BtnC | BtnX | BtnY | BtnZ | BtnS
  deriving (Eq, Ord, Show, Enum, Bounded)

allBtns :: [Btn]
allBtns = [minBound .. maxBound]

-- | Input relative to the fighter's facing.
data RelInput = RelInput
  { riDir :: !Dir,
    riBtns :: ![Btn]
  }
  deriving (Eq, Ord, Show)

-- | Resolve a 'Pad' against a facing. Opposite directions held together
-- cancel out (left+right is neutral horizontally, up+down is neutral
-- vertically), so no input can mean two contradictory things.
relative :: Facing -> Pad -> RelInput
relative facing p = RelInput (dirOf v h) btns
  where
    l = isHeld KLeft p
    r = isHeld KRight p
    u = isHeld KUp p
    d = isHeld KDown p
    v :: Int
    v = case (u, d) of
      (True, False) -> 1
      (False, True) -> -1
      _ -> 0
    -- +1 is forward
    h :: Int
    h = case (l, r) of
      (True, False) -> -facingSign facing
      (False, True) -> facingSign facing
      _ -> 0
    btns =
      [ b
      | (k, b) <- zip [KA, KB, KC, KX, KY, KZ, KStart] allBtns,
        isHeld k p
      ]

dirOf :: Int -> Int -> Dir
dirOf v h = case (v, h) of
  (0, 0) -> Neutral
  (1, 0) -> Up
  (-1, 0) -> Down
  (0, -1) -> Back
  (0, 1) -> Fwd
  (1, -1) -> UpBack
  (1, 1) -> UpFwd
  (-1, -1) -> DownBack
  _ -> DownFwd

-- | Does the held direction include the given one? @dirHas DownFwd Down@ is
-- true: this is MUGEN's @$@ ("loose") direction.
dirHas :: Dir -> Dir -> Bool
dirHas held want = case want of
  Neutral -> held == Neutral
  Up -> held `elem` [Up, UpBack, UpFwd]
  Down -> held `elem` [Down, DownBack, DownFwd]
  Back -> held `elem` [Back, UpBack, DownBack]
  Fwd -> held `elem` [Fwd, UpFwd, DownFwd]
  _ -> held == want
