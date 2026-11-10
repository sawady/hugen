-- | Guarding, hit levels, reactions and knockdowns: a scenario per rule and
-- the usual properties (determinism, symmetry, invariants) over fights at
-- close range.
module CombatSpec (spec) where

import qualified Data.IntMap.Strict as IntMap
import Data.List (find, findIndex)
import Gen
import Hugen.Anim (AnimPos (..), SpriteRef (..), anim, frame)
import Hugen.Box (box)
import Hugen.Camera
import Hugen.Character
import Hugen.Common
import Hugen.Example.Blocky
import Hugen.Example.Demo
import Hugen.Expr (MoveType (..), Stance (..))
import Hugen.Fighter
import Hugen.Input
import Hugen.Match
import Test.Hspec
import Test.QuickCheck

-- | The fighters start within reach of every normal.
closeDef :: MatchDef
closeDef = demoDef {mdStage = defaultStage {stageStartDistance = 48}}

-- | Blocky with one of his attacks replaced.
withMove :: StateId -> HitDef -> Character
withMove st h = blocky {charStates = IntMap.insert st (attackState Standing st h 1 stStand) (charStates blocky)}

-- | Player 1's kick does @h@. The kick reaches both standing and crouching
-- bodies, so what happens depends only on the rules under test.
kickDef :: HitDef -> MatchDef
kickDef h = closeDef {mdP1 = withMove stKick h}

-- | Every tick after the start of a match in which player 1 presses B once
-- and player 2 holds a pad.
kickAgainst :: MatchDef -> Pad -> [Match]
kickAgainst md p2 = run md (newMatch md) ((pad [KB], p2) : replicate 150 (noInput, p2))

run :: MatchDef -> Match -> [(Pad, Pad)] -> [Match]
run md m0 = drop 1 . scanl (flip (step md)) m0

-- | Player 2's posture, as the pad of the fighter on the right.
stand, back, downBack, down :: Pad
stand = noInput
back = pad [KRight]
downBack = pad [KDown, KRight]
down = pad [KDown]

firstContact :: [Match] -> Maybe Match
firstContact = find (fContact . mP1)

-- | Player 1 at its attack's first active tick, ready to hit on the next
-- 'step'.
armedKick :: MatchDef -> Match -> Match
armedKick md m = m {mP1 = (enterState (mdP1 md) stKick (mP1 m)) {fAnimPos = AnimPos 1 0 6}}

-- | Change player 2 before the first tick.
withP2 :: (Fighter -> Fighter) -> Match -> Match
withP2 f m = m {mP2 = f (mP2 m)}

-- | One tick of an armed kick against a prepared player 2.
impactOn :: HitDef -> Pad -> (Character -> Fighter -> Fighter) -> Match
impactOn h p2 prepare =
  step md (noInput, p2) (armedKick md (withP2 (prepare (mdP2 md)) (newMatch md)))
  where
    md = kickDef h

into :: StateId -> Character -> Fighter -> Fighter
into st c = enterState c st

-- | Is this a state in which the fighter is on the floor?
lying :: Fighter -> Bool
lying f = fStance f == Lying

spec :: Spec
spec = describe "combat" $ do
  describe "what guards what" $ do
    it "follows the table" $ do
      [(l, g) | l <- [minBound .. maxBound], g <- [minBound .. maxBound], guardBlocks l g]
        `shouldBe` [ (HitHigh, GuardStanding),
                  (HitHigh, GuardCrouching),
                  (HitMid, GuardStanding),
                  (HitMid, GuardCrouching),
                  (HitLow, GuardCrouching),
                  (HitOverhead, GuardStanding)
                ]

    it "a hit costs its damage and a guard only its chip, for any attack and posture" $
      property $
        forAll genHitDef $ \h ->
          forAll (elements [stand, back, downBack, down]) $ \p2 ->
            let crouching = p2 == downBack || p2 == down
                guard'
                  | p2 == back && guardBlocks (hdLevel h) GuardStanding = Just stGuardStand
                  | p2 == downBack && guardBlocks (hdLevel h) GuardCrouching = Just stGuardCrouch
                  | otherwise = Nothing
                reaction = case hdStrength h of
                  HitLight -> stHit
                  HitHeavy -> stHitHeavy
                  HitKnockdown -> stFall
             in case firstContact (kickAgainst (kickDef h) p2) of
                  Nothing -> counterexample "no contact" (hdLevel h == HitHigh && crouching)
                  Just m ->
                    (hdLevel h =/= HitHigh .||. not crouching)
                      .&&. fLife (mP2 m) === 1000 - maybe (hdDamage h) (const (hdChip h)) guard'
                      .&&. fState (mP2 m) === maybe reaction id guard'

  describe "guarding" $ do
    it "standing guard stops a mid attack, with chip damage and guard stun" $ do
      let m = impactOn hitDef {hdChip = 7, hdGuardStun = 9} back (const id)
      fLife (mP2 m) `shouldBe` 993
      fState (mP2 m) `shouldBe` stGuardStand
      fHitStun (mP2 m) `shouldBe` 9
      fCtrl (mP2 m) `shouldBe` False
      fMoveType (mP2 m) `shouldBe` BeingHit

    it "counts as contact for the attacker and freezes both fighters" $ do
      let m = impactOn hitDef {hdHitPause = 6} back (const id)
      fContact (mP1 m) `shouldBe` True
      fPause (mP1 m) `shouldBe` 6
      fPause (mP2 m) `shouldBe` 6

    it "pushes the guarding fighter away from the attacker" $ do
      let m = impactOn hitDef {hdPushback = 5} back (const id)
          after' = last (run (kickDef hitDef) m (replicate (hdHitPause hitDef + 4) (noInput, noInput)))
      fX (mP2 after') `shouldSatisfy` (> fX (mP2 m))

    it "lasts exactly the hit pause plus the guard stun" $
      property $ \(NonNegative pauseTicks) (Positive stun) ->
        let h = hitDef {hdHitPause = pauseTicks `mod` 20, hdGuardStun = stun `mod` 40 + 1}
            ms = kickAgainst (kickDef h) back
            frozen = hdHitPause h + hdGuardStun h
         in case findIndex (fContact . mP1) ms of
              Nothing -> property False
              Just i ->
                fCtrl (mP2 (ms !! (i + frozen))) === False
                  .&&. fCtrl (mP2 (ms !! (i + frozen + 1))) === True

    it "low attacks beat a standing guard, overheads a crouching one" $ do
      fLife (mP2 (impactOn hitDef {hdLevel = HitLow, hdDamage = 40} back (const id))) `shouldBe` 960
      fLife (mP2 (impactOn hitDef {hdLevel = HitOverhead, hdDamage = 40} downBack (into stCrouch))) `shouldBe` 960

    it "holding away does nothing without control" $ do
      let hit st = fState (mP2 (impactOn hitDef back (into st)))
      hit stJumpStart `shouldBe` stHit
      hit stLand `shouldBe` stHit
      hit stKick `shouldBe` stHit
      fState (mP2 (impactOn hitDef back (\c f -> (enterState c stHit f) {fHitStun = 10}))) `shouldBe` stHit

    it "does not work in the air" $ do
      let m = impactOn hitDef back (\c f -> enterState c stJumpAir f {fY = -20})
      fState (mP2 m) `shouldBe` stHitAir
      fLife (mP2 m) `shouldBe` 1000 - hdDamage hitDef

    it "stays up during guard stun and can switch height" $ do
      let inStun st c f = (enterState c st f) {fHitStun = 5}
          m = impactOn hitDef {hdLevel = HitLow, hdChip = 3} downBack (inStun stGuardStand)
      fState (mP2 m) `shouldBe` stGuardCrouch
      fLife (mP2 m) `shouldBe` 997

    it "is read against the attacker's position on a cross-up" $
      property $ \facing ->
        let md = kickDef hitDef
            s = facingSign facing
            setUp m =
              m
                { mP1 = (mP1 m) {fX = -24 * s, fFacing = facing},
                  mP2 = (mP2 m) {fX = 24 * s, fFacing = facing}
                }
            -- player 2 looks away from player 1, so "away" is its own forward
            away = pad [if facing == FacingRight then KRight else KLeft]
            toward = pad [if facing == FacingRight then KLeft else KRight]
            after' p2 = fState (mP2 (step md (noInput, p2) (armedKick md (setUp (newMatch md)))))
         in after' away === stGuardStand .&&. after' toward === stHit

    it "chip damage can end the round" $ do
      let md = (kickDef hitDef {hdChip = 20}) {mdP2 = blocky {charConsts = (charConsts blocky) {cLife = 10}}}
          m = step md (noInput, back) (armedKick md (newMatch md))
      fLife (mP2 m) `shouldBe` 0
      mOutcome m `shouldBe` Just (Winner P1)

    it "stops a knockdown: the guarding fighter stays on its feet" $ do
      let ms = kickAgainst (kickDef hitDef {hdStrength = HitKnockdown}) back
      any (lying . mP2) ms `shouldBe` False

    it "Blocky's low jab goes through a standing guard but not a crouching one" $ do
      let lowJab p2 =
            last
              ( run
                  closeDef
                  (newMatch closeDef)
                  ((pad [KDown], p2) : (pad [KDown, KA], p2) : replicate 30 (pad [KDown], p2))
              )
      fLife (mP2 (lowJab back)) `shouldBe` 975
      fLife (mP2 (lowJab downBack)) `shouldBe` 1000

  describe "hit levels" $ do
    it "a high attack misses a crouching fighter even when the boxes overlap" $ do
      let contact l = any (fContact . mP1) (kickAgainst (kickDef hitDef {hdLevel = l}) down)
      contact HitMid `shouldBe` True
      contact HitHigh `shouldBe` False

    it "a high attack hits a standing fighter" $
      fLife (mP2 (impactOn hitDef {hdLevel = HitHigh} stand (const id))) `shouldBe` 1000 - hdDamage hitDef

  describe "reactions" $ do
    it "light and heavy attacks use different states" $ do
      fState (mP2 (impactOn hitDef {hdStrength = HitLight} stand (const id))) `shouldBe` stHit
      fState (mP2 (impactOn hitDef {hdStrength = HitHeavy} stand (const id))) `shouldBe` stHitHeavy

    it "light and heavy attacks in the air use the air state" $
      fState (mP2 (impactOn hitDef {hdStrength = HitHeavy} stand (\c f -> enterState c stJumpAir f {fY = -20})))
        `shouldBe` stHitAir

    it "a heavy hit ends in the standing state once the stun runs out" $ do
      let ms = kickAgainst (kickDef hitDef {hdStrength = HitHeavy, hdHitStun = 5, hdHitPause = 2}) stand
      fState (mP2 (last ms)) `shouldBe` stStand

  describe "knockdown" $ do
    let knock = hitDef {hdStrength = HitKnockdown, hdAirLift = -5}
        states ms = dedupe (map (fState . mP2) ms)
        dedupe (a : b : rest)
          | a == b = dedupe (b : rest)
          | otherwise = a : dedupe (b : rest)
        dedupe xs = xs

    it "falls, lies, gets up and stands, in that order" $ do
      let ms = kickAgainst (kickDef knock) stand
      dropWhile (/= stFall) (states ms) `shouldBe` [stFall, stLie, stGetUp, stStand]

    it "lies and gets up for exactly the configured ticks" $ do
      let ms = kickAgainst (kickDef knock) stand
          count st = length (filter ((== st) . fState . mP2) ms)
      count stLie `shouldBe` lieTicks
      count stGetUp `shouldBe` getUpTicks

    it "is launched off the floor and comes back to it" $ do
      let ms = kickAgainst (kickDef knock) stand
      minimum (map (fY . mP2) ms) `shouldSatisfy` (< 0)
      fY (mP2 (last ms)) `shouldBe` 0

    it "from the air it also ends on the floor" $ do
      let md = kickDef knock
          m0 = armedKick md (withP2 (\f -> enterState (mdP2 md) stJumpAir f {fY = -20}) (newMatch md))
          ms = run md m0 (replicate 150 (noInput, noInput))
      dropWhile (/= stFall) (states ms) `shouldBe` [stFall, stLie, stGetUp, stStand]

    it "cannot be hit while lying or getting up" $ do
      -- hurtboxes on purpose: the rule must not depend on the animation data
      let body = anim [frame (SpriteRef 0 0) (-1) [box (-14) (-72) 14 0]]
          exposed = blocky {charAnims = IntMap.insert anLie body (IntMap.insert anGetUp body (charAnims blocky))}
          md = (kickDef hitDef) {mdP2 = exposed}
          hitWhile st = step md (noInput, noInput) (armedKick md (withP2 (enterState exposed st) (newMatch md)))
      fLife (mP2 (hitWhile stLie)) `shouldBe` 1000
      fContact (mP1 (hitWhile stLie)) `shouldBe` False
      fLife (mP2 (hitWhile stGetUp)) `shouldBe` 1000

    it "does not act while down, even when mashing" $ do
      let md = kickDef knock
          ms = run md (newMatch md) ((pad [KB], noInput) : replicate 150 (noInput, pad [KA]))
      all (\m -> not (lying (mP2 m)) || not (fCtrl (mP2 m))) ms `shouldBe` True

    it "Blocky's sweep is low and knocks down" $ do
      let sweep p2 =
            run
              closeDef
              (newMatch closeDef)
              ((pad [KDown], p2) : (pad [KDown, KB], p2) : replicate 120 (pad [KDown], p2))
          hasFallen = any ((== stFall) . fState . mP2)
      hasFallen (sweep stand) `shouldBe` True
      hasFallen (sweep back) `shouldBe` True
      hasFallen (sweep downBack) `shouldBe` False

  describe "juggle limit" $ do
    let juggled n c f = (enterState c stHitAir f {fY = -20}) {fJuggle = n}
        limit = cJuggleLimit (charConsts blocky)

    it "the last hit allowed connects and fills the count" $ do
      let m = impactOn hitDef stand (juggled (limit - 1))
      fContact (mP1 m) `shouldBe` True
      fJuggle (mP2 m) `shouldBe` limit
      fLife (mP2 m) `shouldBe` 1000 - hdDamage hitDef

    it "once it is full attacks go through: no damage, no contact, no pause" $ do
      let m = impactOn hitDef stand (juggled limit)
      fContact (mP1 m) `shouldBe` False
      fPause (mP1 m) `shouldBe` 0
      fLife (mP2 m) `shouldBe` 1000

    it "the first air hit on a jumping fighter counts as one" $
      fJuggle (mP2 (impactOn hitDef stand (\c f -> enterState c stJumpAir f {fY = -20}))) `shouldBe` 1

    it "a knockdown from the floor counts as one" $
      fJuggle (mP2 (impactOn hitDef {hdStrength = HitKnockdown} stand (const id))) `shouldBe` 1

    it "a hit on the floor does not count" $
      fJuggle (mP2 (impactOn hitDef {hdStrength = HitHeavy} stand (const id))) `shouldBe` 0

    it "a fighter held in the air takes exactly the limit, however long the attacks go on" $ do
      let h = hitDef {hdHitPause = 0, hdPushback = 0}
          md = kickDef h
          pin = withP2 (\f -> f {fY = -20, fVY = 0})
          swing x = step md (noInput, noInput) (pin (armedKick md x))
          begin = pin (withP2 (\f -> enterState (mdP2 md) stJumpAir f) (newMatch md))
          end = iterate swing begin !! 40
      fLife (mP2 end) `shouldBe` 1000 - limit * hdDamage h
      fJuggle (mP2 end) `shouldBe` limit

    it "the limit is per character" $ do
      let tough = blocky {charConsts = (charConsts blocky) {cJuggleLimit = 1}}
          md = (kickDef hitDef) {mdP2 = tough}
          m = step md (noInput, noInput) (armedKick md (withP2 (juggled 1 tough) (newMatch md)))
      fLife (mP2 m) `shouldBe` 1000

    it "touching the floor clears the count" $ do
      let md = kickDef hitDef
          m0 = withP2 (juggled limit (mdP2 md)) (newMatch md)
          ms = run md m0 (replicate 60 (noInput, noInput))
      fJuggle (mP2 (last ms)) `shouldBe` 0
      fStance (mP2 (last ms)) `shouldBe` Standing

  describe "validation" $ do
    it "reports a missing guard or knockdown state" $
      validate blocky {charStates = IntMap.delete stGuardCrouch (charStates blocky)}
        `shouldBe` ["state 152 is used but not defined"]

    it "reports a landing state that does not exist" $ do
      let broken = (stateDef Airborne Idle PhysAir) {sdLandState = Just 777}
      validate blocky {charStates = IntMap.insert 900 broken (charStates blocky)}
        `shouldBe` ["state 777 is used but not defined"]

  describe "properties at close range" $ do
    let start = newMatch closeDef
        brawl (Brawl xs) = simulate closeDef xs start
        ticks (Brawl xs) = scanl (flip (step closeDef)) start xs

    it "is deterministic" $
      property $ \b -> brawl b == brawl b

    it "is a fold" $
      property $ \(Brawl xs) (Brawl ys) ->
        simulate closeDef (xs ++ ys) start == simulate closeDef ys (simulate closeDef xs start)

    it "favors no side, and the fights do reach the new rules" $
      checkCoverage $ property $ \b@(Brawl xs) ->
        let a = simulate closeDef xs start
            m = simulate closeDef (mirrorInputs xs) start
            seen st = any (\t -> fState (mP1 t) == st || fState (mP2 t) == st) (ticks b)
            juggled = any (\t -> fJuggle (mP1 t) > 0 || fJuggle (mP2 t) > 0) (ticks b)
         in cover 8 (seen stGuardStand) "standing guard" $
              cover 8 (seen stGuardCrouch) "crouching guard" $
                cover 10 (seen stHitHeavy) "heavy reaction" $
                  cover 6 (seen stFall) "knockdown" $
                    cover 4 (seen stGetUp) "getting up" $
                      cover 4 juggled "hit while in the air" $
                      (mP1 m, mP2 m, camX (mCamera m))
                        === (mirrorFighter (mP2 a), mirrorFighter (mP1 a), negate (camX (mCamera a)))

    it "favors no side with any attack" $
      property $
        forAll genHitDef $ \h -> \(Brawl xs) ->
          let md = (kickDef h) {mdP2 = withMove stKick h}
              s0 = newMatch md
              a = simulate md xs s0
              m = simulate md (mirrorInputs xs) s0
           in (mP1 m, mP2 m, camX (mCamera m))
                === (mirrorFighter (mP2 a), mirrorFighter (mP1 a), negate (camX (mCamera a)))

    it "keeps life in range, fighters in the stage, lying fighters on the floor and juggles within the limit" $
      property $ \b ->
        let st = mdStage closeDef
            ok f =
              fLife f >= 0
                && fLife f <= 1000
                && fX f >= stageLeft st
                && fX f <= stageRight st
                && fY f <= 0
                && (not (lying f) || fY f == 0)
                && fJuggle f >= 0
                && fJuggle f <= cJuggleLimit (charConsts blocky)
                && (fJuggle f == 0 || (fStance f == Airborne && fMoveType f == BeingHit))
         in all (\t -> ok (mP1 t) && ok (mP2 t)) (ticks b)

    it "never hurts a fighter that stays on the floor" $
      property $ \b ->
        let unhurt before' after' =
              not (stillDown before' after') || fLife before' == fLife after'
            -- it is still down after this tick's hits were resolved
            stillDown before' after' =
              lying before'
                && lying after'
                && (fState before' /= stGetUp || fTime before' < getUpTicks)
            pairs = zip (ticks b) (drop 1 (ticks b))
         in all (\(x, y) -> unhurt (mP1 x) (mP1 y) && unhurt (mP2 x) (mP2 y)) pairs

    it "gets a knocked down fighter back up with no input" $
      property $
        forAll genHitDef $ \h0 ->
          let h = h0 {hdStrength = HitKnockdown, hdLevel = HitMid}
              ms = kickAgainst (kickDef h) stand
           in fState (mP2 (last ms)) === stStand
