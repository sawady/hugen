-- | A match between two fighters, advanced one tick at a time.
--
-- 'step' is the whole simulation: a pure function from the current 'Match'
-- and this tick's two 'Pad's to the next 'Match'. The shell calls it 60
-- times per second; tests and replays call it as fast as they like. The
-- same inputs always produce the same match.
--
-- Order of a tick:
--
-- 1. Input: each fighter records its pad and updates its command buffer.
-- 2. Controllers: each fighter runs its global controllers and its state.
--    Both read the opponent as it was before this phase, so neither player
--    gets to act first.
-- 3. Physics: velocity, friction, gravity, landing.
-- 4. Space: bodies push each other apart; fighters stay inside the stage
--    and the camera view.
-- 5. Hits: every active attack is tested against the opponent's hurtboxes
--    on the same snapshot, so two attacks can trade. This is also where a
--    guard is checked, against this tick's pad.
-- 6. Clocks: animation, state time and hit stun advance (or hit pause is
--    spent). Then the hits of step 5 are applied: damage and a hit state, or
--    chip damage and a guard state, and a hit pause that starts on the next
--    tick.
-- 7. Facing: an idle fighter on the floor turns to face the opponent.
-- 8. Camera and round end.
module Hugen.Match
  ( Side (..),
    Outcome (..),
    MatchDef (..),
    Match (..),
    newMatch,
    step,
    simulate,
    mirrorInputs,
  )
where

import Hugen.Box
import Hugen.Camera
import Hugen.Character
import Hugen.Expr (MoveType (..), Stance (..))
import Hugen.Fighter
import Hugen.Fixed
import Hugen.Input

data Side = P1 | P2
  deriving (Eq, Ord, Show, Enum, Bounded)

data Outcome = Winner Side | Draw
  deriving (Eq, Show)

-- | What does not change during a match.
data MatchDef = MatchDef
  { mdP1 :: Character,
    mdP2 :: Character,
    mdStage :: Stage
  }

data Match = Match
  { mTick :: !Int,
    mP1 :: !Fighter,
    mP2 :: !Fighter,
    mCamera :: !Camera,
    mOutcome :: !(Maybe Outcome)
  }
  deriving (Eq, Show)

newMatch :: MatchDef -> Match
newMatch md = Match 0 p1 p2 (cameraFor st (fX p1, fY p1) (fX p2, fY p2)) Nothing
  where
    st = mdStage md
    d = halve (stageStartDistance st)
    p1 = newFighter (mdP1 md) FacingRight (stageCenter st - d)
    p2 = newFighter (mdP2 md) FacingLeft (stageCenter st + d)

-- | Advance one tick. After the round is decided, input is ignored.
step :: MatchDef -> (Pad, Pad) -> Match -> Match
step md (pad1, pad2) m =
  Match
    { mTick = mTick m + 1,
      mP1 = p1f,
      mP2 = p2f,
      mCamera = camF,
      mOutcome = maybe (outcome p1f p2f) Just (mOutcome m)
    }
  where
    c1 = mdP1 md
    c2 = mdP2 md
    st = mdStage md
    over = mOutcome m /= Nothing
    (in1, in2) = if over then (noInput, noInput) else (pad1, pad2)
    -- 1. input
    a1 = feedInput c1 in1 (mP1 m)
    a2 = feedInput c2 in2 (mP2 m)
    -- 2. controllers, against the opponent's snapshot
    b1 = think c1 a2 a1
    b2 = think c2 a1 a2
    -- 3. physics
    ph1 = physics c1 b1
    ph2 = physics c2 b2
    -- 4. space
    (s1, s2) = separate st c1 c2 ph1 ph2
    -- 5. hits, both tested on the same snapshot
    h12 = if over then Nothing else connects c1 s1 c2 in2 s2
    h21 = if over then Nothing else connects c2 s2 c1 in1 s1
    -- 6. clocks; the hit is applied after them, so the tick of the impact
    -- still counts and the hit pause starts on the next one
    t1 = advance c1 s1
    t2 = advance c2 s2
    hit1 = maybe id (receive c1 (fFacing s2)) h21 . maybe id landHit h12 $ t1
    hit2 = maybe id (receive c2 (fFacing s1)) h12 . maybe id landHit h21 $ t2
    -- 7. facing
    p1f = autoTurn hit1 hit2
    p2f = autoTurn hit2 hit1
    -- 8. camera
    camF = cameraFor st (fX p1f, fY p1f) (fX p2f, fY p2f)

-- | Run a list of inputs.
simulate :: MatchDef -> [(Pad, Pad)] -> Match -> Match
simulate md inputs m0 = foldl' (flip (step md)) m0 inputs

-- | The inputs of the mirrored match: players swap sides and left and right
-- swap meaning. Used to test that the engine does not favor a side.
mirrorInputs :: [(Pad, Pad)] -> [(Pad, Pad)]
mirrorInputs = map (\(a, b) -> (mirrorPad b, mirrorPad a))

outcome :: Fighter -> Fighter -> Maybe Outcome
outcome p1 p2 = case (fLife p1 <= 0, fLife p2 <= 0) of
  (True, True) -> Just Draw
  (True, False) -> Just (Winner P2)
  (False, True) -> Just (Winner P1)
  _ -> Nothing

-- | An attack that reaches its target, and the guard that stops it, if any.
data Impact = Impact !HitDef !(Maybe GuardHeight)

-- | Does @a@'s active attack reach @d@ this tick? A fighter on the floor
-- cannot be hit, a high attack passes over a crouching one, and a fighter
-- that has taken its limit of hits in the air cannot be hit until it lands.
-- The pad is the defender's, for its guard.
connects :: Character -> Fighter -> Character -> Pad -> Fighter -> Maybe Impact
connects ca a cd p d = case fAttack a of
  Just h
    | not (fContact a)
        && fStance d /= Lying
        && not (hdLevel h == HitHigh && fStance d == Crouching)
        && not juggledOut
        && anyOverlap (hitboxes ca a) (hurtboxes cd d) ->
        Just (Impact h (guardFor h))
  _ -> Nothing
  where
    juggledOut =
      fStance d == Airborne
        && fMoveType d == BeingHit
        && fJuggle d >= cJuggleLimit (charConsts cd)
    guardFor h = case guardAgainst cd p (fX a) d of
      Just g | guardBlocks (hdLevel h) g -> Just g
      _ -> Nothing

-- | The attacker's side of a hit or a guard: the attack is spent and both
-- freeze.
landHit :: Impact -> Fighter -> Fighter
landHit (Impact h _) a = a {fContact = True, fPause = max (fPause a) (hdHitPause h)}

-- | The defender's side of an impact.
receive :: Character -> Facing -> Impact -> Fighter -> Fighter
receive c attackerFacing (Impact h guard) d = case guard of
  Just g -> guardHit c attackerFacing h g d
  Nothing -> getHit c attackerFacing h d

-- | Speed that pushes the defender away from the attacker, in the defender's
-- relative terms.
pushAway :: Facing -> HitDef -> Fighter -> Fx
pushAway attackerFacing h d = hdPushback h * facingSign attackerFacing * facingSign (fFacing d)

-- | A guarded attack: chip damage, guard stun, no reaction.
guardHit :: Character -> Facing -> HitDef -> GuardHeight -> Fighter -> Fighter
guardHit c attackerFacing h g d =
  entered
    { fLife = max 0 (fLife d - hdChip h),
      fHitStun = hdGuardStun h,
      fPause = max (fPause d) (hdHitPause h),
      fCtrl = False,
      fVX = pushAway attackerFacing h d,
      fVY = 0
    }
  where
    k = charConsts c
    entered = enterState c (case g of GuardStanding -> cGuardStandState k; GuardCrouching -> cGuardCrouchState k) d

-- | An attack that was not guarded.
getHit :: Character -> Facing -> HitDef -> Fighter -> Fighter
getHit c attackerFacing h d =
  entered
    { fLife = max 0 (fLife d - hdDamage h),
      fHitStun = hdHitStun h,
      fPause = max (fPause d) (hdHitPause h),
      fCtrl = False,
      fVX = pushAway attackerFacing h d,
      fVY = if airborneAfter then hdAirLift h else 0,
      fJuggle = if airborneAfter then fJuggle d + 1 else 0
    }
  where
    k = charConsts c
    inAir = fStance d == Airborne
    knocked = hdStrength h == HitKnockdown
    airborneAfter = inAir || knocked
    reaction
      | knocked = cFallState k
      | inAir = cHitAirState k
      | hdStrength h == HitHeavy = cHitHeavyState k
      | otherwise = cHitState k
    entered = enterState c reaction d

-- | Idle fighters on the floor turn to face the opponent. Attacking ones do
-- not: a move that crosses the opponent keeps its direction.
autoTurn :: Fighter -> Fighter -> Fighter
autoTurn f opp
  | fCtrl f && fMoveType f == Idle && fStance f /= Airborne && isBehind f opp =
      f {fFacing = flipFacing (fFacing f)}
  | otherwise = f

-- | Keep bodies from overlapping and fighters inside the view.
separate :: Stage -> Character -> Character -> Fighter -> Fighter -> (Fighter, Fighter)
separate st c1 c2 f1 f2 = fixCorner (clampX lo hi g1, clampX lo hi g2)
  where
    w1 = cHalfWidth (charConsts c1)
    w2 = cHalfWidth (charConsts c2)
    overlapping a b =
      fY a - cHeight (charConsts c1) < fY b
        && fY b - cHeight (charConsts c2) < fY a
    overlapBy a b = (w1 + w2) - abs (fX b - fX a)
    (g1, g2)
      | overlapping f1 f2 && overlapBy f1 f2 > 0 =
          let o = overlapBy f1 f2
              Fx raw = o
              each = Fx ((raw + 1) `quot` 2)
              dir = if fX f2 >= fX f1 then 1 else -1
           in (f1 {fX = fX f1 - each * dir}, f2 {fX = fX f2 + each * dir})
      | otherwise = (f1, f2)
    cam = cameraFor st (fX g1, fY g1) (fX g2, fY g2)
    (lo, hi) = viewBounds st cam
    -- Against a wall only the other fighter can move.
    fixCorner (a, b)
      | overlapping a b && overlapBy a b > 0 =
          let o = overlapBy a b
              dir = if fX b >= fX a then 1 else -1
              aStuck = fX a <= lo || fX a >= hi
              bStuck = fX b <= lo || fX b >= hi
           in case (aStuck, bStuck) of
                (True, False) -> (a, b {fX = fX b + o * dir})
                (False, True) -> (a {fX = fX a - o * dir}, b)
                _ -> (a, b)
      | otherwise = (a, b)

clampX :: Fx -> Fx -> Fighter -> Fighter
clampX lo hi f = f {fX = max lo (min hi (fX f))}
