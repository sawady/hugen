{-# LANGUAGE GADTs #-}

-- | A fighter during a match: the runtime half of a 'Character'.
--
-- Everything that changes during a match is in 'Fighter' and nothing else:
-- position, state, animation, life, input history, command buffer. It is a
-- plain value with 'Eq' and 'Show', so it can be compared, saved and
-- restored (replays, tests, rollback).
module Hugen.Fighter
  ( Fighter (..),
    newFighter,
    eval,
    feedInput,
    Effect (..),
    think,
    thinkEntity,
    enterState,
    physics,
    advance,
    hurtboxes,
    hitboxes,
    currentSprite,
    isBehind,
    guardAgainst,
  )
where

import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (fromMaybe)
import Hugen.Anim
import Hugen.Box
import Hugen.Character
import Hugen.Command
import Hugen.Expr
import Hugen.Fixed
import Hugen.Input

data Fighter = Fighter
  { fX :: !Fx,
    -- | Height over the floor; 0 is standing on it, negative is up.
    fY :: !Fx,
    -- | Horizontal velocity relative to facing (positive is forward).
    fVX :: !Fx,
    fVY :: !Fx,
    fFacing :: !Facing,
    fState :: !StateId,
    fPrevState :: !StateId,
    -- | Ticks in the current state.
    fTime :: !Int,
    fCtrl :: !Bool,
    fStance :: !Stance,
    fMoveType :: !MoveType,
    fPhysics :: !Physics,
    fAnim :: !AnimId,
    fAnimPos :: !AnimPos,
    fLife :: !Int,
    fVars :: !(IntMap.IntMap Int),
    fHistory :: !History,
    fBuffer :: !CommandBuffer,
    -- | The attack made active in this state, if any.
    fAttack :: !(Maybe HitDef),
    -- | Did that attack connect?
    fContact :: !Bool,
    -- | Ticks left of hit stun or guard stun.
    fHitStun :: !Int,
    -- | Ticks left frozen in hit pause.
    fPause :: !Int,
    -- | Hits taken in the air since the fighter last touched the floor.
    fJuggle :: !Int
  }
  deriving (Eq, Show)

newFighter :: Character -> Facing -> Fx -> Fighter
newFighter c facing x =
  enterState c (cStartState k) $
    Fighter
      { fX = x,
        fY = 0,
        fVX = 0,
        fVY = 0,
        fFacing = facing,
        fState = cStartState k,
        fPrevState = cStartState k,
        fTime = 0,
        fCtrl = True,
        fStance = Standing,
        fMoveType = Idle,
        fPhysics = PhysStand,
        fAnim = 0,
        fAnimPos = startAnim,
        fLife = cLife k,
        fVars = IntMap.empty,
        fHistory = emptyHistory (historyTicks c),
        fBuffer = emptyBuffer,
        fAttack = Nothing,
        fContact = False,
        fHitStun = 0,
        fPause = 0,
        fJuggle = 0
      }
  where
    k = charConsts c

-- | Evaluate a trigger expression for @self@, against an @opponent@.
eval :: Character -> Fighter -> Fighter -> Expr a -> a
eval c self opp = go
  where
    go :: Expr b -> b
    go e = case e of
      Lit v -> v
      Time -> fTime self
      AnimTime -> maybe 0 (`animTimeLeft` fAnimPos self) (lookupAnim c (fAnim self))
      AnimElem -> apElem (fAnimPos self)
      AnimElemTime -> apElemTime (fAnimPos self)
      StateNo -> fState self
      PrevStateNo -> fPrevState self
      Life -> fLife self
      Var n -> IntMap.findWithDefault 0 n (fVars self)
      Ctrl -> fCtrl self
      StanceOf -> fStance self
      MoveTypeOf -> fMoveType self
      PosY -> fY self
      VelX -> fVX self
      VelY -> fVY self
      MoveContact -> fContact self
      HitOver -> fHitStun self <= 0
      CommandActive name -> isActive name (fBuffer self)
      Holding d -> dirHas (riDir (inputAt (fHistory self) 0)) d
      P2DistX -> (fX opp - fX self) * facingSign (fFacing self)
      P2DistY -> fY opp - fY self
      P2MoveType -> fMoveType opp
      P2Stance -> fStance opp
      Not a -> not (go a)
      And a b -> go a && go b
      Or a b -> go a || go b
      Cmp op a b -> cmpWith op (go a) (go b)
      Arith op a b -> arith op (go a) (go b)
      Unary op a -> case op of
        Abs -> abs (go a)
        Signum -> signum (go a)
      DivFx a b ->
        let d = go b
         in if d == 0 then 0 else go a / d
      If p a b -> if go p then go a else go b

cmpWith :: (Ord a) => CmpOp -> a -> a -> Bool
cmpWith op = case op of
  Eq -> (==)
  Ne -> (/=)
  Lt -> (<)
  Le -> (<=)
  Gt -> (>)
  Ge -> (>=)

arith :: (Num a) => ArithOp -> a -> a -> a
arith op = case op of
  Add -> (+)
  Sub -> (-)
  Mul -> (*)

-- | Record this tick's pad and update the command buffer. Runs even during
-- hit pause, so input typed then is not lost.
feedInput :: Character -> Pad -> Fighter -> Fighter
feedInput c p f = f {fHistory = h, fBuffer = buf}
  where
    h = recordInput (relative (fFacing f) p) (fHistory f)
    buf = updateBuffer (fPause f > 0) (recognize (charCommands c) h) (fBuffer f)

-- | What running the controllers asks of the match, besides changing the
-- fighter itself.
data Effect
  = -- | Create an entity, with the facing and position (in stage
    -- coordinates) it gets.
    SpawnAt !EntityDef !Facing !(Fx, Fx)
  | -- | Remove the entity that ran the controller.
    RemoveSelf
  deriving (Eq, Show)

-- | Run the global controllers and then the current state's controllers.
-- A state change stops the list it happens in and runs the new state on
-- the same tick, at most 'maxChanges' times per tick so that two states
-- that jump to each other cannot hang the engine. The effects come out in
-- the order their controllers ran.
think :: Character -> Fighter -> Fighter -> (Fighter, [Effect])
think c = thinkWith (charGlobal c) c

-- | 'think' for an entity: only its state's controllers run, never the
-- character's global ones (which turn commands into moves).
thinkEntity :: Character -> Fighter -> Fighter -> (Fighter, [Effect])
thinkEntity = thinkWith []

thinkWith :: [Controller] -> Character -> Fighter -> Fighter -> (Fighter, [Effect])
thinkWith global c opp f0
  | fPause f0 > 0 = (f0, [])
  | otherwise = case run global f0 [] of
      Changed f es -> inState (maxChanges - 1) f es
      Continue f es -> inState maxChanges f es
  where
    inState :: Int -> Fighter -> [Effect] -> (Fighter, [Effect])
    inState budget f es
      | budget <= 0 = (f, reverse es)
      | otherwise = case run (stateControllers f) f es of
          Changed f' es' -> inState (budget - 1) f' es'
          Continue f' es' -> (f', reverse es')
    stateControllers f = maybe [] sdControllers (lookupState c (fState f))
    -- effects are accumulated in reverse
    run :: [Controller] -> Fighter -> [Effect] -> Flow
    run [] f es = Continue f es
    run (k : ks) f es
      | eval c f opp (ctrlTrigger k) = case ctrlAction k of
          ChangeState n -> Changed (enterState c n f) es
          Spawn d -> run ks f (spawnFrom d f : es)
          DestroySelf -> run ks f (RemoveSelf : es)
          a -> run ks (perform c opp a f) es
      | otherwise = run ks f es

data Flow = Continue Fighter [Effect] | Changed Fighter [Effect]

-- | Where an entity appears: its offset is relative to the spawner's facing.
spawnFrom :: EntityDef -> Fighter -> Effect
spawnFrom d f = SpawnAt d (fFacing f) (fX f + dx * facingSign (fFacing f), fY f + dy)
  where
    (dx, dy) = edOffset d

maxChanges :: Int
maxChanges = 8

perform :: Character -> Fighter -> Action -> Fighter -> Fighter
perform c opp a f = case a of
  ChangeState n -> enterState c n f
  ChangeAnim n -> setAnim n f
  SetCtrl b -> f {fCtrl = b}
  VelSet mx my ->
    f
      { fVX = maybe (fVX f) ev mx,
        fVY = maybe (fVY f) ev my
      }
  VelAdd x y -> f {fVX = fVX f + ev x, fVY = fVY f + ev y}
  PosAdd x y -> f {fX = fX f + ev x * facingSign (fFacing f), fY = fY f + ev y}
  SetVar n e -> f {fVars = IntMap.insert n (ev e) (fVars f)}
  Attack h -> f {fAttack = Just h, fContact = False}
  Turn -> f {fFacing = flipFacing (fFacing f)}
  -- handled by 'think', which reports them as effects
  Spawn _ -> f
  DestroySelf -> f
  where
    ev :: Expr b -> b
    ev = eval c f opp

setAnim :: AnimId -> Fighter -> Fighter
setAnim n f = f {fAnim = n, fAnimPos = startAnim}

-- | Enter a state: reset its clock, apply what the state sets on entry and
-- drop the previous state's attack. The juggle count only survives from one
-- airborne reaction to another. An undefined state behaves as an empty one
-- ('validate' reports it).
enterState :: Character -> StateId -> Fighter -> Fighter
enterState c n f = withVel . withCtrl . withAnim $ base
  where
    sd = maybe (stateDef Standing Idle PhysNone) id (lookupState c n)
    base =
      f
        { fPrevState = fState f,
          fState = n,
          fTime = 0,
          fStance = sdStance sd,
          fMoveType = sdMoveType sd,
          fPhysics = sdPhysics sd,
          fAttack = Nothing,
          fContact = False,
          fJuggle = if sdStance sd == Airborne && sdMoveType sd == BeingHit then fJuggle f else 0
        }
    withAnim g = maybe g (`setAnim` g) (sdAnim sd)
    withCtrl g = maybe g (\b -> g {fCtrl = b}) (sdCtrl sd)
    withVel g = maybe g (\(vx, vy) -> g {fVX = vx, fVY = vy}) (sdVel sd)

-- | Move by the velocity, then apply friction or gravity. An airborne
-- fighter that reaches the floor while falling lands: it is put on the
-- floor and sent to the character's land state.
physics :: Character -> Fighter -> Fighter
physics c f
  | fPause f > 0 = f
  | otherwise = case fPhysics f of
      PhysStand -> moved {fVX = fVX f * cStandFriction k}
      PhysCrouch -> moved {fVX = fVX f * cCrouchFriction k}
      PhysNone -> moved
      PhysAir
        | fY moved >= 0 && fVY f > 0 ->
            enterState c landing moved {fY = 0, fVX = 0, fVY = 0}
        | otherwise -> moved {fVY = fVY f + cGravity k}
  where
    k = charConsts c
    landing = fromMaybe (cLandState k) (lookupState c (fState f) >>= sdLandState)
    moved =
      f
        { fX = fX f + fVX f * facingSign (fFacing f),
          fY = fY f + fVY f
        }

-- | End of tick: advance the clocks, or spend a tick of hit pause.
advance :: Character -> Fighter -> Fighter
advance c f
  | fPause f > 0 = f {fPause = fPause f - 1}
  | otherwise =
      f
        { fTime = fTime f + 1,
          fAnimPos = maybe (fAnimPos f) (`advanceAnim` fAnimPos f) (lookupAnim c (fAnim f)),
          fHitStun = max 0 (fHitStun f - 1)
        }

currentFrameOf :: Character -> Fighter -> Maybe Frame
currentFrameOf c f = lookupAnim c (fAnim f) >>= (`currentFrame` fAnimPos f)

-- | Hurtboxes of the current frame, in stage coordinates.
hurtboxes :: Character -> Fighter -> [Box]
hurtboxes c f = maybe [] (map (placeBox (fFacing f) (fX f, fY f)) . frameHurt) (currentFrameOf c f)

-- | Hitboxes of the current frame, in stage coordinates.
hitboxes :: Character -> Fighter -> [Box]
hitboxes c f = maybe [] (map (placeBox (fFacing f) (fX f, fY f)) . frameHit) (currentFrameOf c f)

currentSprite :: Character -> Fighter -> Maybe SpriteRef
currentSprite c f = frameSprite <$> currentFrameOf c f

-- | Is the opponent behind the fighter?
isBehind :: Fighter -> Fighter -> Bool
isBehind self opp = (fX opp - fX self) * facingSign (fFacing self) < 0

-- | The guard a fighter holds this tick against an attacker at @attackerX@,
-- given the pad of this tick. It takes holding away from the attacker, on
-- the floor, either with control and idle or already in guard stun (which is
-- how a guard is kept up, and switched high or low, during a string of
-- attacks). The pad is absolute and read against the attacker's position, so
-- a cross-up has to be guarded the other way before the fighter has turned.
guardAgainst :: Character -> Pad -> Fx -> Fighter -> Maybe GuardHeight
guardAgainst c p attackerX f
  | able && dirHas dir Back = Just (if dirHas dir Down then GuardCrouching else GuardStanding)
  | otherwise = Nothing
  where
    k = charConsts c
    dir = riDir (relative toward p)
    toward = case compare attackerX (fX f) of
      GT -> FacingRight
      LT -> FacingLeft
      EQ -> fFacing f
    onFloor = fStance f == Standing || fStance f == Crouching
    inGuardStun = fState f == cGuardStandState k || fState f == cGuardCrouchState k
    able = onFloor && ((fCtrl f && fMoveType f == Idle) || inGuardStun)
