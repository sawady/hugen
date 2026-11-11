-- | Character definitions: states, controllers, attacks, animations and
-- constants. Everything here is static data shared by every match; the
-- per-match state lives in "Hugen.Fighter".
--
-- The model follows MUGEN's CNS files:
--
-- * A character is always in exactly one numbered state ('StateDef').
-- * Entering a state may set the animation, control flag and velocity.
-- * Every tick the state's 'Controller's run in order. Each one has a
--   trigger ('Expr' 'Bool') and an 'Action'. A 'ChangeState' stops the rest
--   of the list and the new state starts running on the same tick.
-- * Before the current state, the character's global controllers run
--   (MUGEN's state -1). That is where commands become moves.
module Hugen.Character
  ( StateId,
    Physics (..),
    HitLevel (..),
    HitStrength (..),
    GuardHeight (..),
    guardBlocks,
    HitDef (..),
    hitDef,
    ThrowDef (..),
    throwDef,
    Armed (..),
    EntityKind (..),
    EntityDef (..),
    Action (..),
    Controller (..),
    on,
    StateDef (..),
    stateDef,
    Consts (..),
    defaultConsts,
    Character (..),
    lookupState,
    lookupAnim,
    validate,
    historyTicks,

    -- * Action helpers
    changeState,
    changeAnim,
    setCtrl,
    velSet,
    velSetX,
    velSetY,
    velAdd,
    posAdd,
    setVar,
    attack,
    throw,
    turn,
    spawnHelper,
    spawnProjectile,
    destroySelf,
  )
where

import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (isNothing, mapMaybe)
import Hugen.Anim
import Hugen.Command
import Hugen.Expr
import Hugen.Fixed

type StateId = Int

-- | What the engine does to the fighter's position every tick.
data Physics
  = -- | On the floor, with friction.
    PhysStand
  | -- | On the floor, crouching (more friction).
    PhysCrouch
  | -- | Gravity; landing sends the fighter to the character's land state.
    PhysAir
  | -- | Nothing: the state's controllers move the fighter themselves.
    PhysNone
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Where an attack lands, which decides how it can be guarded.
data HitLevel
  = -- | Misses a crouching fighter altogether, whatever its boxes say.
    -- Either guard stops it.
    HitHigh
  | -- | Either guard stops it.
    HitMid
  | -- | Only a crouching guard stops it.
    HitLow
  | -- | Only a standing guard stops it.
    HitOverhead
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | How an unguarded attack throws the opponent around.
data HitStrength
  = HitLight
  | HitHeavy
  | -- | The opponent falls, lies on the floor and gets up.
    HitKnockdown
  deriving (Eq, Ord, Show, Enum, Bounded)

data GuardHeight = GuardStanding | GuardCrouching
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Does this guard stop an attack of this level?
guardBlocks :: HitLevel -> GuardHeight -> Bool
guardBlocks HitLow GuardStanding = False
guardBlocks HitOverhead GuardCrouching = False
guardBlocks _ _ = True

-- | An attack, made active by the 'Attack' action (MUGEN's HitDef). It
-- connects at most once, when a hitbox of the current frame overlaps a
-- hurtbox of the opponent.
data HitDef = HitDef
  { hdDamage :: !Int,
    -- | Ticks the opponent stays in its hit state.
    hdHitStun :: !Int,
    -- | Ticks both fighters freeze on impact ("hit stop").
    hdHitPause :: !Int,
    -- | Speed the opponent is pushed away with.
    hdPushback :: !Fx,
    -- | Vertical speed given to an opponent hit in the air or knocked down
    -- (negative is up).
    hdAirLift :: !Fx,
    hdLevel :: !HitLevel,
    hdStrength :: !HitStrength,
    -- | Damage dealt through a guard.
    hdChip :: !Int,
    -- | Ticks the opponent stays in its guard state.
    hdGuardStun :: !Int
  }
  deriving (Eq, Show)

-- | A light attack; adjust with record update syntax.
hitDef :: HitDef
hitDef =
  HitDef
    { hdDamage = 30,
      hdHitStun = 12,
      hdHitPause = 8,
      hdPushback = 4,
      hdAirLift = -4,
      hdLevel = HitMid,
      hdStrength = HitLight,
      hdChip = 0,
      hdGuardStun = 10
    }

-- | A throw, made active by the 'Throw' action (MUGEN's HitDef with
-- @p1stateno@ and @p2stateno@, and @TargetBind@). Like an attack, it
-- connects when a hitbox of the current frame overlaps a hurtbox of the
-- opponent, but it cannot be guarded and only catches an opponent that is on
-- its feet and not being hit. The opponent is then held: it follows the
-- thrower at 'tdOffset' for 'tdHold' ticks, and can escape during the first
-- 'tdEscape' of them. If it does not, the throw lands: 'tdDamage' and a
-- knockdown.
data ThrowDef = ThrowDef
  { tdDamage :: !Int,
    -- | Thrower's state once the throw connects.
    tdState :: !StateId,
    -- | Ticks the opponent is held before the throw lands.
    tdHold :: !Int,
    -- | Ticks at the start of the hold in which the opponent can escape.
    tdEscape :: !Int,
    -- | Where the opponent is held, from the thrower's axis: x forward, y
    -- down.
    tdOffset :: !(Fx, Fx),
    -- | Velocity the opponent is thrown with: x forward from the thrower, y
    -- down (negative is up).
    tdLaunch :: !(Fx, Fx)
  }
  deriving (Eq, Show)

-- | A throw that goes on to the given state of the thrower; adjust with
-- record update syntax.
throwDef :: StateId -> ThrowDef
throwDef st =
  ThrowDef
    { tdDamage = 100,
      tdState = st,
      tdHold = 20,
      tdEscape = 10,
      tdOffset = (32, 0),
      tdLaunch = (3, -5)
    }

-- | What a state makes active: an attack or a throw.
data Armed
  = Strike !HitDef
  | Grab !ThrowDef
  deriving (Eq, Show)

-- | What an entity created by 'Spawn' is. Either kind runs its own states
-- (from its owner's character) and its attacks hit the owner's opponent.
data EntityKind
  = -- | Lives until it destroys itself with 'DestroySelf'.
    Helper
  | -- | Like a helper, but also destroyed when its attack connects (hit or
    -- guard), when its hitboxes touch those of an opposing projectile, and
    -- when it leaves the view.
    Projectile
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | An entity to create (MUGEN's Helper and Projectile controllers).
data EntityDef = EntityDef
  { edKind :: !EntityKind,
    -- | State the entity starts in, one of its owner's states.
    edState :: !StateId,
    -- | Where it appears, relative to the spawner's axis: x forward, y down.
    edOffset :: !(Fx, Fx)
  }
  deriving (Eq, Show)

data Action
  = ChangeState StateId
  | ChangeAnim AnimId
  | SetCtrl Bool
  | -- | Set velocity; 'Nothing' keeps that component.
    VelSet (Maybe (Expr Fx)) (Maybe (Expr Fx))
  | VelAdd (Expr Fx) (Expr Fx)
  | PosAdd (Expr Fx) (Expr Fx)
  | SetVar Int (Expr Int)
  | Attack HitDef
  | -- | Make a throw active. Only a fighter's throw connects: an entity's
    -- never does.
    Throw ThrowDef
  | -- | Face the other way.
    Turn
  | -- | Create an entity. It faces the way the spawner faces and starts
    -- acting on the next tick.
    Spawn EntityDef
  | -- | Remove the entity running this controller at the end of the tick.
    -- A fighter ignores it.
    DestroySelf
  deriving (Show)

data Controller = Controller
  { ctrlTrigger :: Expr Bool,
    ctrlAction :: Action
  }
  deriving (Show)

-- | @on trigger action@.
on :: Expr Bool -> Action -> Controller
on = Controller

data StateDef = StateDef
  { sdStance :: Stance,
    sdMoveType :: MoveType,
    sdPhysics :: Physics,
    -- | Animation to switch to on entry.
    sdAnim :: Maybe AnimId,
    -- | Control flag to set on entry.
    sdCtrl :: Maybe Bool,
    -- | Velocity to set on entry.
    sdVel :: Maybe (Fx, Fx),
    -- | State entered on touching the floor, instead of the character's
    -- land state.
    sdLandState :: Maybe StateId,
    sdControllers :: [Controller]
  }
  deriving (Show)

-- | A state with nothing set on entry and no controllers.
stateDef :: Stance -> MoveType -> Physics -> StateDef
stateDef st mt ph = StateDef st mt ph Nothing Nothing Nothing Nothing []

-- | Per-character numbers, like MUGEN's [Data], [Size] and [Velocity].
data Consts = Consts
  { cLife :: !Int,
    cWalkFwd :: !Fx,
    cWalkBack :: !Fx,
    -- | Initial vertical speed of a jump (negative is up).
    cJumpUp :: !Fx,
    cJumpFwd :: !Fx,
    cJumpBack :: !Fx,
    cGravity :: !Fx,
    -- | Velocity is multiplied by this every tick on the floor.
    cStandFriction :: !Fx,
    cCrouchFriction :: !Fx,
    -- | Half the body width, used to keep fighters from overlapping.
    cHalfWidth :: !Fx,
    -- | Body height, used to let a jump clear the opponent.
    cHeight :: !Fx,
    -- | State entered when an airborne fighter touches the floor.
    cLandState :: !StateId,
    -- | State entered when hit lightly, standing or crouching.
    cHitState :: !StateId,
    -- | State entered when hit hard, standing or crouching.
    cHitHeavyState :: !StateId,
    -- | State entered when hit in the air.
    cHitAirState :: !StateId,
    -- | State entered when knocked down, from the floor or from the air.
    cFallState :: !StateId,
    -- | State entered when guarding standing.
    cGuardStandState :: !StateId,
    -- | State entered when guarding crouching.
    cGuardCrouchState :: !StateId,
    -- | State entered at the start of a round.
    cStartState :: !StateId,
    -- | State the fighter is in while an opponent's throw holds it.
    cHeldState :: !StateId,
    -- | Command that escapes a throw, during the throw's escape window.
    cThrowEscape :: !String,
    -- | Hits a fighter can take before touching the floor again.
    cJuggleLimit :: !Int
  }
  deriving (Eq, Show)

-- | Numbers in the ballpark of a MUGEN character at 1 px = 1 unit, with
-- the common state numbers of "Hugen.Common".
defaultConsts :: Consts
defaultConsts =
  Consts
    { cLife = 1000,
      cWalkFwd = 2.4,
      cWalkBack = -2.2,
      cJumpUp = -8.4,
      cJumpFwd = 2.5,
      cJumpBack = -2.5,
      cGravity = 0.44,
      cStandFriction = 0.85,
      cCrouchFriction = 0.82,
      cHalfWidth = 16,
      cHeight = 60,
      cLandState = 52,
      cHitState = 5000,
      cHitHeavyState = 5001,
      cHitAirState = 5030,
      cFallState = 5050,
      cGuardStandState = 150,
      cGuardCrouchState = 152,
      cStartState = 0,
      cHeldState = 5300,
      cThrowEscape = "throw",
      cJuggleLimit = 3
    }

data Character = Character
  { charName :: String,
    charConsts :: Consts,
    charCommands :: [Command],
    -- | MUGEN's state -1: runs every tick before the current state.
    charGlobal :: [Controller],
    charStates :: IntMap.IntMap StateDef,
    charAnims :: IntMap.IntMap Anim
  }

lookupState :: Character -> StateId -> Maybe StateDef
lookupState c n = IntMap.lookup n (charStates c)

lookupAnim :: Character -> AnimId -> Maybe Anim
lookupAnim c n = IntMap.lookup n (charAnims c)

-- | Ticks of input history the character's commands need.
historyTicks :: Character -> Int
historyTicks c = 2 + maximum (0 : map need (charCommands c))
  where
    need k = cmdTime k + chordWindow + sum [n | Charge _ n <- cmdSteps k]

-- | Problems in a definition: references to missing states, animations or
-- commands, empty animations. An empty list means the character is well
-- formed.
validate :: Character -> [String]
validate c =
  [ "state " ++ show n ++ " is used but not defined"
  | n <- requiredStates ++ concatMap targets allControllers ++ landStates,
    isNothing (lookupState c n)
  ]
    ++ [ "animation " ++ show a ++ " is used but not defined"
       | a <- usedAnims,
         isNothing (lookupAnim c a)
       ]
    ++ [ "animation " ++ show a ++ " has no frames"
       | (a, an) <- IntMap.toList (charAnims c),
         null (animFrames an)
       ]
    ++ [ "command " ++ show (cThrowEscape k) ++ " escapes throws but is not defined"
       | cThrowEscape k `notElem` map cmdName (charCommands c)
       ]
  where
    k = charConsts c
    requiredStates =
      [ cLandState k,
        cHitState k,
        cHitHeavyState k,
        cHitAirState k,
        cFallState k,
        cGuardStandState k,
        cGuardCrouchState k,
        cStartState k,
        cHeldState k
      ]
    landStates = mapMaybe sdLandState (IntMap.elems (charStates c))
    allControllers = charGlobal c ++ concatMap sdControllers (IntMap.elems (charStates c))
    targets ctl = case ctrlAction ctl of
      ChangeState n -> [n]
      Spawn d -> [edState d]
      Throw t -> [tdState t]
      _ -> []
    usedAnims =
      mapMaybe sdAnim (IntMap.elems (charStates c))
        ++ [a | ctl <- allControllers, ChangeAnim a <- [ctrlAction ctl]]

changeState :: StateId -> Action
changeState = ChangeState

changeAnim :: AnimId -> Action
changeAnim = ChangeAnim

setCtrl :: Bool -> Action
setCtrl = SetCtrl

velSet :: Expr Fx -> Expr Fx -> Action
velSet x y = VelSet (Just x) (Just y)

velSetX, velSetY :: Expr Fx -> Action
velSetX x = VelSet (Just x) Nothing
velSetY y = VelSet Nothing (Just y)

velAdd :: Expr Fx -> Expr Fx -> Action
velAdd = VelAdd

posAdd :: Expr Fx -> Expr Fx -> Action
posAdd = PosAdd

setVar :: Int -> Expr Int -> Action
setVar = SetVar

attack :: HitDef -> Action
attack = Attack

throw :: ThrowDef -> Action
throw = Throw

turn :: Action
turn = Turn

-- | Create a helper in the given state, at an offset from the spawner's axis
-- (x forward, y down).
spawnHelper :: StateId -> Fx -> Fx -> Action
spawnHelper st dx dy = Spawn (EntityDef Helper st (dx, dy))

-- | Create a projectile in the given state, at an offset from the spawner's
-- axis (x forward, y down).
spawnProjectile :: StateId -> Fx -> Fx -> Action
spawnProjectile st dx dy = Spawn (EntityDef Projectile st (dx, dy))

destroySelf :: Action
destroySelf = DestroySelf
