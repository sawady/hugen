-- | Helpers and projectiles: a scenario per rule (creation, flight, hits,
-- guards, cancelling out, leaving the view, ids, the per-side cap) and the
-- usual properties (determinism, symmetry, invariants) over fights full of
-- fireballs.
module EntitySpec (spec) where

import qualified Data.IntMap.Strict as IntMap
import Data.List (find, findIndex, nub, sort)
import Gen
import Hugen.Anim (AnimPos (..), SpriteRef (..), anim)
import qualified Hugen.Anim as Anim
import Hugen.Box (box)
import Hugen.Camera
import Hugen.Character
import Hugen.Common
import Hugen.Example.Blocky
import Hugen.Example.Demo
import Hugen.Expr
import Hugen.Fighter
import Hugen.Fixed
import Hugen.Input
import Hugen.Match
import Test.Hspec
import Test.QuickCheck

start :: Match
start = newMatch demoDef

-- | The fighters start within reach of every normal.
closeDef :: MatchDef
closeDef = demoDef {mdStage = defaultStage {stageStartDistance = 48}}

run :: MatchDef -> Match -> [(Pad, Pad)] -> [Match]
run md m0 = drop 1 . scanl (flip (step md)) m0

-- | Player 1 throws a fireball at once; player 2 holds a pad.
throwAgainst :: Pad -> [Match]
throwAgainst p2 = run demoDef start (zip (fireballMotion KLeft ++ replicate 150 noInput) (repeat p2))

-- | Player 2 throws a fireball at once; player 1 holds a pad.
throwFromP2 :: Pad -> [Match]
throwFromP2 p1 = run demoDef start (zip (repeat p1) (fireballMotion KRight ++ replicate 150 noInput))

-- | One of Blocky's fireballs, as it is when created.
fireballAt :: Int -> Side -> Facing -> Fx -> Fx -> Entity
fireballAt n side facing x y =
  Entity n side Projectile (enterState blocky stFireball (newFighter blocky facing x) {fY = y})

-- | A match with these entities added.
withEntities :: [Entity] -> Match -> Match
withEntities es m = m {mEntities = es, mNextId = length es}

entityX :: Entity -> Fx
entityX = fX . eBody

-- | Player 1 at its kick's first active tick, ready to hit on the next 'step'.
armedKick :: MatchDef -> Match -> Match
armedKick md m = m {mP1 = (enterState (mdP1 md) stKick (mP1 m)) {fAnimPos = AnimPos 1 0 6}}

-- A character with a helper ------------------------------------------------

stHelper, stIdleHelper :: StateId
stHelper = 900
stIdleHelper = 901

helperHit :: HitDef
helperHit = hitDef {hdDamage = 15, hdHitPause = 6}

-- | How long the helper lives, in ticks of its own state.
helperLife :: Int
helperLife = 30

-- | Blocky whose kick also creates a helper 140 px ahead, on top of an
-- opponent at the starting distance. The helper hits once and destroys
-- itself after 'helperLife' ticks.
summoner :: Character
summoner =
  blocky
    { charStates =
        IntMap.insert stHelper helperState $
          IntMap.adjust (\sd -> sd {sdControllers = on (time .== 0) (spawnHelper stHelper 140 0) : sdControllers sd}) stKick (charStates blocky),
      charAnims = IntMap.insert stHelper helperAnim (charAnims blocky)
    }
  where
    helperState =
      (stateDef Standing Attacking PhysNone)
        { sdAnim = Just stHelper,
          sdControllers =
            [ on (time .== 0) (attack helperHit),
              on (time .>= lit helperLife) destroySelf
            ]
        }
    helperAnim = anim [Anim.Frame (SpriteRef 900 0) (-1) [] [box (-10) (-60) 10 0]]

-- | Blocky with a global controller that creates an idle helper every tick.
spammer :: Character
spammer =
  blocky
    { charGlobal = on true (spawnHelper stIdleHelper 0 0) : charGlobal blocky,
      charStates = IntMap.insert stIdleHelper (stateDef Standing Idle PhysNone) (charStates blocky)
    }

spec :: Spec
spec = describe "entities" $ do
  describe "Blocky's fireball" $ do
    it "appears in front of the thrower, on the throw's second frame, and flies forward" $ do
      let ms = throwAgainst noInput
      i <- maybe (expectationFailure "no fireball" >> pure 0) pure (findIndex (not . null . mEntities) ms)
      let m = ms !! i
          next = ms !! (i + 1)
      case (mEntities m, mEntities next) of
        ([e], [e']) -> do
          (eId e, eOwner e, eKind e) `shouldBe` (0, P1, Projectile)
          fState (eBody e) `shouldBe` stFireball
          fState (mP1 m) `shouldBe` stThrow
          fTime (mP1 m) `shouldBe` 11
          (entityX e, fY (eBody e)) `shouldBe` (fX (mP1 m) + fst fireballOffset, snd fireballOffset)
          fFacing (eBody e) `shouldBe` FacingRight
          entityX e' `shouldBe` entityX e + fireballSpeed
        other -> expectationFailure ("expected one fireball, got " ++ show (length (fst other)))

    it "thrown by player 2, it appears on its other side and flies the other way" $ do
      let ms = throwFromP2 noInput
      case [(m, e) | m <- ms, e <- mEntities m] of
        ((m, e) : (_, e') : _) -> do
          eOwner e `shouldBe` P2
          entityX e `shouldBe` fX (mP2 m) - fst fireballOffset
          fFacing (eBody e) `shouldBe` FacingLeft
          entityX e' `shouldBe` entityX e - fireballSpeed
        _ -> expectationFailure "no fireball"

    it "hits from a distance, once, and does not freeze the thrower" $ do
      let ms = throwAgainst noInput
      case find ((< 1000) . fLife . mP2) ms of
        Nothing -> expectationFailure "the fireball never hit"
        Just m -> do
          fLife (mP2 m) `shouldBe` 1000 - hdDamage fireballHit
          fState (mP2 m) `shouldBe` stHit
          fPause (mP2 m) `shouldBe` hdHitPause fireballHit
          fPause (mP1 m) `shouldBe` 0
          fContact (mP1 m) `shouldBe` False
          mEntities m `shouldBe` []
          fX (mP2 m) - fX (mP1 m) `shouldSatisfy` (> 100)
      fLife (mP2 (last ms)) `shouldBe` 1000 - hdDamage fireballHit

    it "guarded, it costs only its chip damage and is spent too" $ do
      let ms = throwAgainst (pad [KRight])
      case find ((== stGuardStand) . fState . mP2) ms of
        Nothing -> expectationFailure "the fireball was never guarded"
        Just m -> do
          fLife (mP2 m) `shouldBe` 1000 - hdChip fireballHit
          fHitStun (mP2 m) `shouldBe` hdGuardStun fireballHit
          mEntities m `shouldBe` []
      fLife (mP2 (last ms)) `shouldBe` 1000 - hdChip fireballHit

    it "is guarded away from the fireball, not from the thrower" $ do
      -- player 2's fireball comes at player 1 from behind
      let m0 = withEntities [fireballAt 0 P2 FacingRight (fX (mP1 start) - 40) (-46)] start
          reaction p1 = find (/= stWalk) (map (fState . mP1) (run demoDef m0 (replicate 20 (p1, noInput))))
      reaction (pad [KRight]) `shouldBe` Just stGuardStand
      reaction (pad [KLeft]) `shouldBe` Just stHit

  describe "leaving the view" $ do
    it "a projectile is removed once its axis passes the view plus the margin, not before" $ do
      let m0 = withEntities [fireballAt 0 P1 FacingLeft (-100) (-46)] start
          ms = m0 : run demoDef m0 (replicate 60 (noInput, noInput))
          st = mdStage demoDef
          viewLeft = camX (mCamera start) - halve (stageViewWidth st)
          lo = viewLeft - stageOffscreenMargin st
          xs = [entityX e | m <- ms, e <- mEntities m]
      minimum xs `shouldSatisfy` (< viewLeft)
      minimum xs `shouldSatisfy` (>= lo)
      minimum xs - fireballSpeed `shouldSatisfy` (< lo)
      mEntities (last ms) `shouldBe` []

    it "a helper is not removed for leaving the view" $ do
      let helper = (fireballAt 0 P1 FacingLeft (-100) (-46)) {eKind = Helper}
          m = simulate demoDef (replicate 60 (noInput, noInput)) (withEntities [helper] start)
      map eId (mEntities m) `shouldBe` [0]

  describe "projectile against projectile" $ do
    it "two fireballs thrown at once cancel out, and nobody is hurt" $ do
      let ms = run demoDef start (zip (fireballMotion KLeft ++ replicate 120 noInput) (fireballMotion KRight ++ replicate 120 noInput))
      any ((== 2) . length . mEntities) ms `shouldBe` True
      any ((== 1) . length . mEntities) ms `shouldBe` False
      mEntities (last ms) `shouldBe` []
      (fLife (mP1 (last ms)), fLife (mP2 (last ms))) `shouldBe` (1000, 1000)

    it "they cancel out wherever they meet, if both get thrown" $
      property $ \(NonNegative d) ->
        let delay = d `mod` 9
            ms = run demoDef start (zip (fireballMotion KLeft ++ replicate 120 noInput) (replicate delay noInput ++ fireballMotion KRight ++ replicate 120 noInput))
         in (fLife (mP1 (last ms)), fLife (mP2 (last ms)), mEntities (last ms)) === (1000, 1000, [])

    it "projectiles of the same side do not cancel out" $ do
      let m0 = withEntities [fireballAt 0 P1 FacingLeft (-100) (-46), fireballAt 1 P1 FacingLeft (-98) (-46)] start
      map eId (mEntities (step demoDef (noInput, noInput) m0)) `shouldBe` [0, 1]

    it "a projectile does not hit once the round is over" $ do
      let m0 = (withEntities [fireballAt 0 P1 FacingRight (fX (mP2 start)) (-46)] start) {mOutcome = Just (Winner P1)}
          m = simulate demoDef (replicate 5 (noInput, noInput)) m0
      fLife (mP2 m) `shouldBe` 1000
      map eId (mEntities m) `shouldBe` [0]

    it "a projectile does not hit its owner" $ do
      let m0 = withEntities [fireballAt 0 P1 FacingRight (fX (mP1 start)) (-46)] start
          m = step demoDef (noInput, noInput) m0
      fLife (mP1 m) `shouldBe` 1000
      map eId (mEntities m) `shouldBe` [0]

  describe "order and ids" $ do
    it "ids follow creation order and are never reused" $ do
      let throws = concat (replicate 3 (fireballMotion KLeft ++ replicate 40 noInput))
          ms = run demoDef start (zip throws (repeat noInput))
      nub [eId e | m <- ms, e <- mEntities m] `shouldBe` [0, 1, 2]
      mNextId (last ms) `shouldBe` 3

    it "on the same tick, player 1's entity is created first" $ do
      let ms = run demoDef start (zip (fireballMotion KLeft ++ replicate 20 noInput) (fireballMotion KRight ++ replicate 20 noInput))
      case find ((== 2) . length . mEntities) ms of
        Just m -> [(eId e, eOwner e) | e <- mEntities m] `shouldBe` [(0, P1), (1, P2)]
        Nothing -> expectationFailure "no two fireballs at once"

    it "entities keep moving while the fighters are frozen in hit pause" $ do
      let m0 = withEntities [fireballAt 0 P1 FacingLeft (-60) (-46)] (newMatch closeDef)
          ms = run closeDef m0 ((pad [KA], noInput) : replicate 20 (noInput, noInput))
      case findIndex ((> 1) . fPause . mP1) ms of
        Nothing -> expectationFailure "the jab never hit"
        Just i -> do
          let x k = map entityX (mEntities (ms !! k))
          x (i + 1) `shouldBe` map (subtract fireballSpeed) (x i)
          fX (mP1 (ms !! (i + 1))) `shouldBe` fX (mP1 (ms !! i))

    it "a side cannot have more than the maximum, and entities do not run global controllers" $ do
      let md = demoDef {mdP1 = spammer, mdP2 = spammer}
          ticksIn n = simulate md (replicate n (noInput, noInput)) (newMatch md)
          count s m = length (filter ((== s) . eOwner) (mEntities m))
      (count P1 (ticksIn 10), count P2 (ticksIn 10)) `shouldBe` (10, 10)
      (count P1 (ticksIn 100), count P2 (ticksIn 100)) `shouldBe` (maxEntitiesPerSide, maxEntitiesPerSide)
      mNextId (ticksIn 100) `shouldBe` 2 * maxEntitiesPerSide

  describe "helpers" $ do
    let md = demoDef {mdP1 = summoner}
        ms = run md (newMatch md) ((pad [KB], noInput) : replicate 80 (noInput, noInput))

    it "a helper's attack hits once, freezes the helper and not its owner, and the helper stays" $
      case findIndex ((< 1000) . fLife . mP2) ms of
        Nothing -> expectationFailure "the helper never hit"
        Just i -> do
          let m = ms !! i
          fLife (mP2 m) `shouldBe` 1000 - hdDamage helperHit
          map (fPause . eBody) (mEntities m) `shouldBe` [hdHitPause helperHit]
          map (fContact . eBody) (mEntities m) `shouldBe` [True]
          fPause (mP1 m) `shouldBe` 0
          fLife (mP2 (last ms)) `shouldBe` 1000 - hdDamage helperHit

    it "destroys itself when its state says so" $ do
      let alive = [e | m <- ms, e <- mEntities m]
      map (fTime . eBody) (take 1 (reverse alive)) `shouldBe` [helperLife]
      mEntities (last ms) `shouldBe` []

  describe "several hits on the same tick" $ do
    let md = closeDef
        juggledAt n m = m {mP2 = (enterState blocky stHitAir (mP2 m) {fY = -20, fVY = 0}) {fJuggle = n}}
        both n = step md (noInput, noInput) (armedKick md (withEntities [fireballAt 0 P1 FacingRight (fX (mP2 m0)) (-50)] (juggledAt n m0)))
        m0 = newMatch md
        kick = 70
        limit = cJuggleLimit (charConsts blocky)

    it "all land, each one counting for the juggle" $ do
      let m = both 0
      fLife (mP2 m) `shouldBe` 1000 - kick - hdDamage fireballHit
      fJuggle (mP2 m) `shouldBe` 2
      mEntities m `shouldBe` []

    it "but not past the juggle limit: the fighter's attack lands, the fireball goes through" $ do
      let m = both (limit - 1)
      fLife (mP2 m) `shouldBe` 1000 - kick
      fJuggle (mP2 m) `shouldBe` limit
      fContact (mP1 m) `shouldBe` True
      map eId (mEntities m) `shouldBe` [0]

  describe "validation" $
    it "reports an entity created in a state that does not exist" $ do
      let broken = (stateDef Standing Idle PhysStand) {sdControllers = [on true (spawnProjectile 777 0 0)]}
      validate blocky {charStates = IntMap.insert 902 broken (charStates blocky)}
        `shouldBe` ["state 777 is used but not defined"]

  describe "properties with fireballs" $ do
    let barrage (Barrage xs) = simulate demoDef xs start
        ticks (Barrage xs) = scanl (flip (step demoDef)) start xs
        st = mdStage demoDef

    it "is deterministic" $
      property $ \b -> barrage b == barrage b

    it "is a fold" $
      property $ \(Barrage xs) (Barrage ys) ->
        simulate demoDef (xs ++ ys) start == simulate demoDef ys (simulate demoDef xs start)

    it "favors no side, and the fights do reach every way a projectile ends" $
      checkCoverage $ property $ \b@(Barrage xs) ->
        let a = simulate demoDef xs start
            m = simulate demoDef (mirrorInputs xs) start
            pairs = zip (ticks b) (drop 1 (ticks b))
            gone x y = [e | e <- mEntities x, eId e `notElem` map eId (mEntities y)]
            fighter s = bySide s mP1 mP2
            hurt x y = any (\e -> fLife (fighter (otherSide (eOwner e)) y) < fLife (fighter (otherSide (eOwner e)) x)) (gone x y)
            guarded x y = any (\e -> let d = fighter (otherSide (eOwner e)) y in fState d `elem` [stGuardStand, stGuardCrouch] && fTime d == 0) (gone x y)
            clash x y = length (nub (map eOwner (gone x y))) == 2
            offscreen x y = any (\e -> not (insideView st (mCamera x) 0 (entityX e))) (gone x y)
            seen p = any (uncurry p) pairs
         in cover 30 (seen hurt) "a projectile hits" $
              cover 6 (seen guarded) "a projectile is guarded" $
                cover 10 (seen clash) "projectiles cancel out" $
                  cover 5 (seen offscreen) "a projectile leaves the view" $
                    m `isMirrorOf` a

    it "favors no side with a helper" $
      property $ \(Brawl xs) ->
        let md = closeDef {mdP1 = summoner, mdP2 = summoner}
            s0 = newMatch md
         in simulate md (mirrorInputs xs) s0 `isMirrorOf` simulate md xs s0

    it "keeps entities in creation order, under the cap and projectiles near the view" $
      property $ \b ->
        let ok m =
              let ids = map eId (mEntities m)
                  perSide s = length (filter ((== s) . eOwner) (mEntities m))
               in ids == sort (nub ids)
                    && all (< mNextId m) ids
                    && perSide P1 <= maxEntitiesPerSide
                    && perSide P2 <= maxEntitiesPerSide
                    && and [insideView st (mCamera m) (stageOffscreenMargin st) (entityX e) | e <- mEntities m, eKind e == Projectile]
         in all ok (ticks b)

    it "keeps life in range and fighters inside the stage" $
      property $ \b ->
        let ok f = fLife f >= 0 && fLife f <= 1000 && fX f >= stageLeft st && fX f <= stageRight st && fY f <= 0
         in all (\t -> ok (mP1 t) && ok (mP2 t)) (ticks b)
  where
    bySide s x y = case s of
      P1 -> x
      P2 -> y
