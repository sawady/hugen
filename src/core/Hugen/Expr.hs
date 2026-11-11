{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Trigger expressions: the conditions that fire state controllers.
--
-- This is a deep embedding: an 'Expr' is a typed syntax tree, not a Haskell
-- function. It is evaluated by the engine ("Hugen.Fighter"), but it can also
-- be shown, inspected (which commands does a state read? which states can it
-- reach?) and, later, parsed from a text format. The combinators below make
-- it read like MUGEN's triggers:
--
-- > on (cmd "qcf_a" .&& ctrl .&& grounded) (changeState 1000)
-- > on (animTime .== 0) (changeState 0)
module Hugen.Expr
  ( Expr (..),
    CmpOp (..),
    ArithOp (..),
    UnOp (..),
    Stance (..),
    MoveType (..),

    -- * Literals
    lit,
    true,
    false,

    -- * Queries about the fighter
    time,
    animTime,
    animElem,
    animElemTime,
    animElemStarts,
    stateNo,
    prevStateNo,
    life,
    var,
    ctrl,
    stance,
    moveType,
    grounded,
    airborne,
    posY,
    velX,
    velY,
    moveContact,
    hitOver,
    cmd,
    holding,
    numProjectiles,

    -- * Queries about the opponent
    p2DistX,
    p2DistY,
    p2MoveType,
    p2Stance,

    -- * Operators
    (.==),
    (./=),
    (.<),
    (.<=),
    (.>),
    (.>=),
    (.&&),
    (.||),
    notE,
    ifE,
    (./.),
  )
where

import Hugen.Fixed (Fx)
import Hugen.Input (Dir)

-- | MUGEN's statetype: decides physics defaults and which hits connect.
data Stance = Standing | Crouching | Airborne | Lying
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | MUGEN's movetype.
data MoveType = Idle | Attacking | BeingHit
  deriving (Eq, Ord, Show, Enum, Bounded)

data CmpOp = Eq | Ne | Lt | Le | Gt | Ge
  deriving (Eq, Show)

data ArithOp = Add | Sub | Mul
  deriving (Eq, Show)

data UnOp = Abs | Signum
  deriving (Eq, Show)

data Expr a where
  Lit :: (Show a) => a -> Expr a
  -- Fighter
  Time :: Expr Int
  AnimTime :: Expr Int
  AnimElem :: Expr Int
  AnimElemTime :: Expr Int
  StateNo :: Expr Int
  PrevStateNo :: Expr Int
  Life :: Expr Int
  Var :: Int -> Expr Int
  Ctrl :: Expr Bool
  StanceOf :: Expr Stance
  MoveTypeOf :: Expr MoveType
  PosY :: Expr Fx
  VelX :: Expr Fx
  VelY :: Expr Fx
  MoveContact :: Expr Bool
  HitOver :: Expr Bool
  CommandActive :: String -> Expr Bool
  Holding :: Dir -> Expr Bool
  NumProjectiles :: Expr Int
  -- Opponent
  P2DistX :: Expr Fx
  P2DistY :: Expr Fx
  P2MoveType :: Expr MoveType
  P2Stance :: Expr Stance
  -- Operators
  Not :: Expr Bool -> Expr Bool
  And :: Expr Bool -> Expr Bool -> Expr Bool
  Or :: Expr Bool -> Expr Bool -> Expr Bool
  Cmp :: (Ord b, Show b) => CmpOp -> Expr b -> Expr b -> Expr Bool
  Arith :: (Num a) => ArithOp -> Expr a -> Expr a -> Expr a
  Unary :: (Num a) => UnOp -> Expr a -> Expr a
  DivFx :: Expr Fx -> Expr Fx -> Expr Fx
  If :: Expr Bool -> Expr a -> Expr a -> Expr a

deriving instance Show (Expr a)

instance (Num a, Show a) => Num (Expr a) where
  (+) = Arith Add
  (-) = Arith Sub
  (*) = Arith Mul
  abs = Unary Abs
  signum = Unary Signum
  fromInteger = Lit . fromInteger

-- | Only for literals like @2.5@ and division of 'Fx' expressions.
instance Fractional (Expr Fx) where
  fromRational = Lit . fromRational
  (/) = DivFx

lit :: (Show a) => a -> Expr a
lit = Lit

true, false :: Expr Bool
true = Lit True
false = Lit False

-- | Ticks since the fighter entered its current state (0 on the first tick).
time :: Expr Int
time = Time

-- | Ticks until the animation ends: negative while it plays, 0 when done.
animTime :: Expr Int
animTime = AnimTime

-- | Index of the current animation frame, starting at 0.
animElem :: Expr Int
animElem = AnimElem

animElemTime :: Expr Int
animElemTime = AnimElemTime

-- | True on the first tick of the given animation frame.
animElemStarts :: Int -> Expr Bool
animElemStarts n = animElem .== Lit n .&& animElemTime .== 0

stateNo, prevStateNo, life :: Expr Int
stateNo = StateNo
prevStateNo = PrevStateNo
life = Life

-- | Fighter variable @n@ (0 if never set), like MUGEN's @var(n)@.
var :: Int -> Expr Int
var = Var

-- | Can the player act? (MUGEN's @ctrl@.)
ctrl :: Expr Bool
ctrl = Ctrl

stance :: Expr Stance
stance = StanceOf

moveType :: Expr MoveType
moveType = MoveTypeOf

grounded, airborne :: Expr Bool
grounded = stance ./= Lit Airborne
airborne = stance .== Lit Airborne

-- | Height relative to the floor; negative is up.
posY :: Expr Fx
posY = PosY

-- | Velocity, x relative to facing (positive is forward).
velX, velY :: Expr Fx
velX = VelX
velY = VelY

-- | Did the current attack connect?
moveContact :: Expr Bool
moveContact = MoveContact

-- | In a hit state, has hit stun run out?
hitOver :: Expr Bool
hitOver = HitOver

-- | Is the named command active in the buffer?
cmd :: String -> Expr Bool
cmd = CommandActive

-- | Is the direction (loose, relative to facing) held right now?
holding :: Dir -> Expr Bool
holding = Holding

-- | Projectiles of the fighter's side in the match at the start of the
-- tick (MUGEN's @NumProj@). For an entity, those of its owner's side,
-- itself included if it is one.
numProjectiles :: Expr Int
numProjectiles = NumProjectiles

-- | Horizontal distance between the axes, positive when the opponent is in
-- front.
p2DistX :: Expr Fx
p2DistX = P2DistX

-- | Opponent's height minus own height (negative when the opponent is
-- higher).
p2DistY :: Expr Fx
p2DistY = P2DistY

p2MoveType :: Expr MoveType
p2MoveType = P2MoveType

p2Stance :: Expr Stance
p2Stance = P2Stance

infix 4 .==, ./=, .<, .<=, .>, .>=

infixr 3 .&&

infixr 2 .||

(.==), (./=), (.<), (.<=), (.>), (.>=) :: (Ord a, Show a) => Expr a -> Expr a -> Expr Bool
(.==) = Cmp Eq
(./=) = Cmp Ne
(.<) = Cmp Lt
(.<=) = Cmp Le
(.>) = Cmp Gt
(.>=) = Cmp Ge

(.&&), (.||) :: Expr Bool -> Expr Bool -> Expr Bool
(.&&) = And
(.||) = Or

notE :: Expr Bool -> Expr Bool
notE = Not

ifE :: Expr Bool -> Expr a -> Expr a -> Expr a
ifE = If

-- | Division of 'Fx' expressions, rounding toward zero.
(./.) :: Expr Fx -> Expr Fx -> Expr Fx
(./.) = DivFx
