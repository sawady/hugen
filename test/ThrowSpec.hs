-- | Throws: a scenario per rule (what a throw catches, the hold, landing,
-- escaping, what happens to other attacks meanwhile) and the usual
-- properties (determinism, symmetry, invariants) over fights at throw range.
module ThrowSpec (spec) where

import qualified Data.IntMap.Strict as IntMap
import Data.List (findIndex)
import Data.Maybe (isNothing)
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

-- | The fighters start within reach of the throw.
closeDef :: MatchDef
closeDef = demoDef {mdStage = defaultStage {stageStartDistance = 48}}

run :: MatchDef -> Match -> [(Pad, Pad)] -> [Match]
run md m0 = drop 1 . scanl (flip (step md)) m0

-- | Player 1 presses throw once; player 2's pads follow, then nothing.
throwAgainst :: MatchDef -> [Pad] -> [Match]
throwAgainst md p2 = run md (newMatch md) (zip (pad [KC] : repeat noInput) (p2 ++ replicate 200 noInput))

-- | Player 1 presses throw once; player 2 holds a pad.
throwHolding :: Pad -> [Match]
throwHolding p2 = throwAgainst closeDef (replicate 200 p2)

-- | The tick the throw connects, if it does.
grabTick :: [Match] -> Maybe Int
grabTick = findIndex (inHold . mP2)

-- | Player 1 on the first active tick of its throw, ready to grab on the
-- next 'step'.
armedGrab :: MatchDef -> Match -> Match
armedGrab md m = m {mP1 = (enterState (mdP1 md) stGrab (mP1 m)) {fAnimPos = AnimPos 1 0 3}}

-- | Player 2 on the first active tick of its kick.
armedKickP2 :: MatchDef -> Match -> Match
armedKickP2 md m = m {mP2 = (enterState (mdP2 md) stKick (mP2 m)) {fAnimPos = AnimPos 1 0 6}}

-- | One tick of player 1's armed throw against a prepared player 2.
grabOn :: (Character -> Fighter -> Fighter) -> Match
grabOn prepare = step closeDef (noInput, noInput) (armedGrab closeDef (withP2 (prepare blocky) (newMatch closeDef)))

withP2 :: (Fighter -> Fighter) -> Match -> Match
withP2 f m = m {mP2 = f (mP2 m)}

-- | Blocky with another throw.
withThrow :: ThrowDef -> Character
withThrow t =
  blocky
    { charStates =
        IntMap.adjust (\sd -> sd {sdControllers = [on (animElemStarts 1) (throw t), on (animTime .>= 0) (changeState stStand)]}) stGrab $
          IntMap.adjust (\sd -> sd {sdControllers = [on (time .>= lit (tdHold t + 10)) (changeState stStand)]}) stGrabHold (charStates blocky)
    }

-- | Where the held fighter should be, before keeping it inside the view.
boundAt :: Fighter -> ThrowDef -> (Fx, Fx)
boundAt a t = (fX a + fst (tdOffset t) * facingSign (fFacing a), min 0 (fY a + snd (tdOffset t)))

-- | The tick a fighter changes from held to free, and how: by escaping
-- (unhurt) or by the throw landing.
released :: (Match -> Fighter) -> Match -> Match -> Bool
released side x y = inHold (side x) && not (inHold (side y))

spec :: Spec
spec = describe "throws" $ do
  describe "Blocky's throw" $ do
    let ms = throwHolding noInput
        t = blockyThrow

    it "holds the opponent in front of him, without hurting it yet" $
      case grabTick ms of
        Nothing -> expectationFailure "the throw never connected"
        Just i -> do
          let m = ms !! i
          i `shouldBe` 3
          fState (mP1 m) `shouldBe` stGrabHold
          fState (mP2 m) `shouldBe` stHeld
          fHeld (mP2 m) `shouldBe` Just (Held t 0)
          (fX (mP2 m), fY (mP2 m)) `shouldBe` boundAt (mP1 m) t
          fLife (mP2 m) `shouldBe` 1000
          (fPause (mP1 m), fPause (mP2 m)) `shouldBe` (0, 0)
          fCtrl (mP2 m) `shouldBe` False

    it "keeps it there for exactly the hold, then lands: damage and a knockdown" $
      case grabTick ms of
        Nothing -> expectationFailure "the throw never connected"
        Just i -> do
          let hold' = take (tdHold t) (drop i ms)
              landed = ms !! (i + tdHold t)
          map (fmap heldTime . fHeld . mP2) hold' `shouldBe` map Just [0 .. tdHold t - 1]
          all (\m -> (fX (mP2 m), fY (mP2 m)) == boundAt (mP1 m) t && fLife (mP2 m) == 1000) hold' `shouldBe` True
          fHeld (mP2 landed) `shouldBe` Nothing
          fState (mP2 landed) `shouldBe` stFall
          fLife (mP2 landed) `shouldBe` 1000 - tdDamage t
          fJuggle (mP2 landed) `shouldBe` 1
          fState (mP2 (last ms)) `shouldBe` stStand
          fState (mP1 (last ms)) `shouldBe` stStand

    it "tosses the opponent forward, from either side" $ do
      let fromP2 = run closeDef (newMatch closeDef) (zip (repeat noInput) (pad [KC] : replicate 200 noInput))
          furthest f = maximum . map (abs . fX . f)
      case (grabTick ms, findIndex (inHold . mP1) fromP2) of
        (Just i, Just j) -> do
          fVX (mP2 (ms !! (i + tdHold t))) `shouldBe` negate (fst (tdLaunch t))
          furthest mP2 ms `shouldSatisfy` (> abs (fX (mP2 (ms !! i))))
          furthest mP1 fromP2 `shouldSatisfy` (> abs (fX (mP1 (fromP2 !! j))))
          fLife (mP1 (last fromP2)) `shouldBe` 1000 - tdDamage t
        _ -> expectationFailure "a throw never connected"

    it "goes through a standing or crouching guard and catches a crouching opponent" $ do
      -- nearer, so that walking back does not leave the throw's reach
      let nearDef = closeDef {mdStage = defaultStage {stageStartDistance = 40}}
          heldAs p2 = let ts = throwAgainst nearDef (replicate 200 p2) in fmap (fState . mP2 . (ts !!)) (grabTick ts)
      mapM_ (\p2 -> heldAs p2 `shouldBe` Just stHeld) [pad [KRight], pad [KDown, KRight], pad [KDown]]

    it "whiffs out of reach, and the whiff costs its recovery" $ do
      let far = throwAgainst demoDef []
      any (inHold . mP2) far `shouldBe` False
      length (filter ((== stGrab) . fState . mP1) far) `shouldBe` 22
      fLife (mP2 (last far)) `shouldBe` 1000

  describe "what a throw catches" $ do
    it "an opponent on its feet, idle or in the middle of an attack" $ do
      inHold (mP2 (grabOn (const id))) `shouldBe` True
      inHold (mP2 (grabOn (\c -> enterState c stKick))) `shouldBe` True
      inHold (mP2 (grabOn (\c -> enterState c stCrouch))) `shouldBe` True

    it "not an opponent in the air, lying, getting up, in hit stun or in guard stun" $
      mapM_
        (\prepare -> inHold (mP2 (grabOn prepare)) `shouldBe` False)
        [ \c f -> enterState c stJumpAir f {fY = -10},
          \c -> enterState c stLie,
          \c -> enterState c stGetUp,
          \c f -> (enterState c stHit f) {fHitStun = 8},
          \c f -> (enterState c stGuardStand f) {fHitStun = 8}
        ]

    it "a throw that catches nothing does not connect" $ do
      let m = step demoDef (noInput, noInput) (armedGrab demoDef (newMatch demoDef))
      (fContact (mP1 m), inHold (mP2 m)) `shouldBe` (False, False)

  describe "on the same tick" $ do
    it "a throw beats an attack: the throw connects and the attack goes through" $ do
      let m = step closeDef (noInput, noInput) (armedKickP2 closeDef (armedGrab closeDef (newMatch closeDef)))
      inHold (mP2 m) `shouldBe` True
      fLife (mP1 m) `shouldBe` 1000
      fContact (mP2 m) `shouldBe` False
      fState (mP1 m) `shouldBe` stGrabHold

    it "two throws both miss" $ do
      let armedP2 md x = x {mP2 = (enterState (mdP2 md) stGrab (mP2 x)) {fAnimPos = AnimPos 1 0 3}}
          m = step closeDef (noInput, noInput) (armedP2 closeDef (armedGrab closeDef (newMatch closeDef)))
      (inHold (mP1 m), inHold (mP2 m)) `shouldBe` (False, False)
      (fState (mP1 m), fState (mP2 m)) `shouldBe` (stGrab, stGrab)

  describe "escaping" $ do
    let t = blockyThrow
        -- player 2 presses throw once, k ticks after the throw connects
        escapeAt k = throwAgainst closeDef (replicate (3 + k) noInput ++ [pad [KC]])

    it "pressing throw within the window frees both fighters, unhurt and pushed apart" $ do
      let ms = escapeAt 4
      case findIndex (\(x, y) -> released mP2 x y) (zip ms (drop 1 ms)) of
        Nothing -> expectationFailure "never released"
        Just i -> do
          let m = ms !! (i + 1)
          (fState (mP1 m), fState (mP2 m)) `shouldBe` (stHit, stHit)
          (fHitStun (mP1 m), fHitStun (mP2 m)) `shouldBe` (throwEscapeStun, throwEscapeStun)
          (fLife (mP1 m), fLife (mP2 m)) `shouldBe` (1000, 1000)
          (fVX (mP1 m), fVX (mP2 m)) `shouldBe` (negate throwEscapePush, negate throwEscapePush)
          fHeld (mP2 m) `shouldBe` Nothing
          heldTime <$> fHeld (mP2 (ms !! i)) `shouldBe` Just 3
          -- both get control back on the same tick, apart
          let back' = dropWhile (\x -> not (fCtrl (mP1 x) || fCtrl (mP2 x))) (drop (i + 1) ms)
          case back' of
            (x : _) -> do
              (fCtrl (mP1 x), fCtrl (mP2 x)) `shouldBe` (True, True)
              fX (mP2 x) - fX (mP1 x) `shouldSatisfy` (> fst (tdOffset t))
            [] -> expectationFailure "control never came back"
      fLife (mP2 (last ms)) `shouldBe` 1000

    it "works exactly within the window: from the grab tick (through the buffer) to its last tick" $
      property $
        forAll (choose (0, tdHold t - 1)) $ \k ->
          let ms = escapeAt k
              escaped = fLife (mP2 (last ms)) == 1000
           in counterexample ("pressed " ++ show k ++ " ticks after the grab") (escaped === (k <= tdEscape t))

    it "the held fighter cannot do anything else, even mashing" $ do
      let ms = throwAgainst closeDef (replicate 3 noInput ++ concat (replicate 20 [pad [KA], pad [KB], pad [KDown, KLeft]]))
      case grabTick ms of
        Nothing -> expectationFailure "the throw never connected"
        Just i -> do
          map (fState . mP2) (take (tdHold t) (drop i ms)) `shouldBe` replicate (tdHold t) stHeld
          fLife (mP2 (ms !! (i + tdHold t))) `shouldBe` 1000 - tdDamage t

    it "the held fighter runs no controllers, not even global ones that ignore control" $ do
      let restless = blocky {charGlobal = on (cmd "b") (changeState stKick) : charGlobal blocky}
          md = closeDef {mdP2 = restless}
          held = [m | m <- throwAgainst md (replicate 4 noInput ++ concat (replicate 12 [pad [KB], noInput])), inHold (mP2 m)]
      length held `shouldBe` tdHold blockyThrow
      all ((== stHeld) . fState . mP2) held `shouldBe` True

    it "a fighter held off the floor falls back to it after escaping" $ do
      let lifted = withThrow blockyThrow {tdOffset = (32, -20)}
          md = closeDef {mdP1 = lifted}
          ms = throwAgainst md (replicate 5 noInput ++ [pad [KC]] ++ replicate 60 noInput)
      case findIndex (\(x, y) -> released mP2 x y) (zip ms (drop 1 ms)) of
        Nothing -> expectationFailure "never released"
        Just i -> do
          fState (mP2 (ms !! (i + 1))) `shouldBe` stHitAir
          fState (mP1 (ms !! (i + 1))) `shouldBe` stHit
      (fY (mP2 (last ms)), fState (mP2 (last ms)), fLife (mP2 (last ms))) `shouldBe` (0, stStand, 1000)

  describe "during a hold" $ do
    let t = blockyThrow
        ms = throwHolding noInput
        i = maybe 0 id (grabTick ms)
        midHold = ms !! (i + 2)

    it "nothing hits either fighter, and projectiles go through without being spent" $ do
      let fireball side facing x = Entity 0 side Projectile (enterState blocky stFireball (newFighter blocky facing x) {fY = -46})
          es = [fireball P1 FacingRight (fX (mP2 midHold)), (fireball P2 FacingLeft (fX (mP1 midHold))) {eId = 1}]
          m = step closeDef (noInput, noInput) midHold {mEntities = es, mNextId = 2}
      (fLife (mP1 m), fLife (mP2 m)) `shouldBe` (1000, 1000)
      map eId (mEntities m) `shouldBe` [0, 1]
      inHold (mP2 m) `shouldBe` True

    it "the thrower's own attacks go through the held fighter" $ do
      let kicking = midHold {mP1 = (enterState blocky stKick (mP1 midHold)) {fAnimPos = AnimPos 1 0 6}}
          m = step closeDef (noInput, noInput) kicking
      fLife (mP2 m) `shouldBe` 1000
      fContact (mP1 m) `shouldBe` False

    it "the held fighter follows the thrower when it turns, and is thrown the way it faces" $ do
      let turner =
            blocky
              { charStates =
                  IntMap.adjust (\sd -> sd {sdControllers = on (time .== 5) turn : sdControllers sd}) stGrabHold (charStates blocky)
              }
          md = closeDef {mdP1 = turner}
          ts = throwAgainst md []
      case grabTick ts of
        Nothing -> expectationFailure "the throw never connected"
        Just j -> do
          let before' = ts !! j
              after' = ts !! (j + 8)
          fX (mP2 before') `shouldSatisfy` (> fX (mP1 before'))
          fX (mP2 after') `shouldBe` fX (mP1 after') - fst (tdOffset t)
          fX (mP2 (last ts)) `shouldSatisfy` (< fX (mP1 (last ts)))

    it "bodies do not push: the thrower stays put even when the held body overlaps it" $ do
      let md = closeDef {mdP1 = withThrow blockyThrow {tdOffset = (10, 0)}}
          ts = throwAgainst md []
          held = [m | m <- ts, inHold (mP2 m)]
      length held `shouldBe` tdHold blockyThrow
      map (fX . mP1) held `shouldBe` replicate (length held) (fX (mP1 (newMatch md)))
      map (\m -> fX (mP2 m) - fX (mP1 m)) held `shouldBe` replicate (length held) 10

    it "the held fighter does not move by itself, whatever its held state says" $ do
      let floaty = blocky {charStates = IntMap.insert stHeld (stateDef Airborne BeingHit PhysAir) {sdAnim = Just anHeld} (charStates blocky)}
          md = closeDef {mdP2 = floaty}
          held = [m | m <- throwAgainst md [], inHold (mP2 m)]
      length held `shouldBe` tdHold blockyThrow
      all (\m -> fState (mP2 m) == stHeld && fY (mP2 m) == 0) held `shouldBe` True

    it "the held fighter is never put under the floor" $ do
      let md = closeDef {mdP1 = withThrow blockyThrow {tdOffset = (32, 20)}}
          held = [m | m <- throwAgainst md [], inHold (mP2 m)]
      length held `shouldBe` tdHold blockyThrow
      map (fY . mP2) held `shouldBe` replicate (length held) 0

    it "the held fighter stays inside the view at a wall" $ do
      let turner =
            blocky
              { charStates =
                  IntMap.adjust (\sd -> sd {sdControllers = on (time .== 0) turn : sdControllers sd}) stGrabHold (charStates blocky)
              }
          md = closeDef {mdP1 = turner}
          st = mdStage md
          -- player 1 backed against the left wall
          m0 = (newMatch md) {mP1 = (mP1 (newMatch md)) {fX = stageLeft st + 16}, mP2 = (mP2 (newMatch md)) {fX = stageLeft st + 60}}
          ts = run md m0 ((pad [KC], noInput) : replicate 40 (noInput, noInput))
          held = [m | m <- ts, inHold (mP2 m)]
      length held `shouldSatisfy` (> 5)
      all (\m -> fX (mP2 m) >= stageLeft st && fX (mP2 m) <= fX (mP1 m)) (drop 2 held) `shouldBe` True

  describe "landing" $ do
    it "can end the round" $ do
      let weak = blocky {charConsts = (charConsts blocky) {cLife = 50}}
          ms = throwAgainst closeDef {mdP2 = weak} []
      fLife (mP2 (last ms)) `shouldBe` 0
      mOutcome (last ms) `shouldBe` Just (Winner P1)

    it "the thrown fighter falls, lies and gets up, like after any knockdown" $ do
      let states = dedupe (map (fState . mP2) (throwHolding noInput))
          dedupe (a : b : rest)
            | a == b = dedupe (b : rest)
            | otherwise = a : dedupe (b : rest)
          dedupe xs = xs
      states `shouldBe` [stStand, stHeld, stFall, stLie, stGetUp, stStand]

  describe "after the round" $
    it "a throw does not connect once the round is decided" $ do
      let m = step closeDef (noInput, noInput) (armedGrab closeDef (newMatch closeDef)) {mOutcome = Just Draw}
      inHold (mP2 m) `shouldBe` False

  describe "entities" $
    it "an entity's throw never connects" $ do
      let grabber =
            blocky
              { charStates = IntMap.insert 950 (stateDef Standing Attacking PhysNone) {sdAnim = Just 950, sdControllers = [on (time .== 0) (throw blockyThrow)]} (charStates blocky),
                charAnims = IntMap.insert 950 (anim [Anim.Frame (SpriteRef 950 0) (-1) [] [box (-30) (-80) 30 0]]) (charAnims blocky)
              }
          md = closeDef {mdP1 = grabber}
          m0 = newMatch md
          helper = Entity 0 P1 Helper (enterState grabber 950 (newFighter grabber FacingRight (fX (mP2 m0))))
          ms = run md m0 {mEntities = [helper], mNextId = 1} (replicate 10 (noInput, noInput))
      any (inHold . mP2) ms `shouldBe` False

  describe "validation" $ do
    it "reports a missing held state or throw state" $ do
      validate blocky {charStates = IntMap.delete stHeld (charStates blocky)}
        `shouldBe` ["state 5300 is used but not defined"]
      validate blocky {charStates = IntMap.delete stGrabHold (charStates blocky)}
        `shouldBe` ["state 810 is used but not defined"]

    it "reports an escape command that does not exist" $
      validate blocky {charConsts = (charConsts blocky) {cThrowEscape = "tech"}}
        `shouldBe` ["command \"tech\" escapes throws but is not defined"]

  describe "properties at throw range" $ do
    let start = newMatch closeDef
        ticks (Grapple xs) = scanl (flip (step closeDef)) start xs
        st = mdStage closeDef

    it "is deterministic" $
      property $ \(Grapple xs) -> simulate closeDef xs start == simulate closeDef xs start

    it "is a fold" $
      property $ \(Grapple xs) (Grapple ys) ->
        simulate closeDef (xs ++ ys) start == simulate closeDef ys (simulate closeDef xs start)

    it "favors no side, and the fights do reach every way a hold ends" $
      checkCoverage $ property $ \g@(Grapple xs) ->
        let a = simulate closeDef xs start
            m = simulate closeDef (mirrorInputs xs) start
            pairs = zip (ticks g) (drop 1 (ticks g))
            ends hurt' = any (\(x, y) -> or [released f x y && hurt' (f x) (f y) | f <- [mP1, mP2]]) pairs
            landedSeen = ends (\x y -> fLife y < fLife x || fState y == stFall)
            escapedSeen = ends (\x y -> fLife y == fLife x && fState y /= stFall)
         in cover 30 landedSeen "a throw lands" $
              cover 15 escapedSeen "a throw is escaped" $
                m `isMirrorOf` a

    it "favors no side with any throw, and keeps fighters inside the stage and above the floor" $
      property $
        forAll (genThrowDef stGrabHold) $ \t -> \(Grapple xs) ->
          let md = closeDef {mdP1 = withThrow t, mdP2 = withThrow t}
              s0 = newMatch md
              ok f = fY f <= 0 && fX f >= stageLeft st && fX f <= stageRight st
           in simulate md (mirrorInputs xs) s0 `isMirrorOf` simulate md xs s0
                .&&. all (\m -> ok (mP1 m) && ok (mP2 m)) (scanl (flip (step md)) s0 xs)

    it "keeps one fighter held at most, bound to the other, unhurt until the throw lands" $
      property $ \g ->
        let ok m = case (fHeld (mP1 m), fHeld (mP2 m)) of
              (Just _, Just _) -> False
              (Just h, Nothing) -> bound (mP2 m) (mP1 m) h
              (Nothing, Just h) -> bound (mP1 m) (mP2 m) h
              (Nothing, Nothing) -> True
            -- the thrower holds it at the offset, or nearer when the view
            -- is in the way, and the hold has not run out
            bound a d (Held t n) =
              let (bx, by) = boundAt a t
                  along = (fX d - fX a) * facingSign (fFacing a)
                  full = (bx - fX a) * facingSign (fFacing a)
               in fY d == by
                    && (fX d == bx || (signum along /= negate (signum full) && abs along <= abs full))
                    && n < tdHold t
                    && fState d == stHeld
                    && not (fCtrl d)
            unhurt x y = all (\f -> not (inHold (f x) && inHold (f y)) || fLife (f x) == fLife (f y)) [mP1, mP2]
            inStage f = fLife f >= 0 && fLife f <= 1000 && fX f >= stageLeft st && fX f <= stageRight st && fY f <= 0
         in all ok (ticks g)
              && and (zipWith unhurt (ticks g) (drop 1 (ticks g)))
              && all (\m -> inStage (mP1 m) && inStage (mP2 m)) (ticks g)

    it "always lets go: a hold ends by itself" $
      property $ \(Grapple xs) ->
        -- a throw armed by the last pads still has its startup to go
        let m = simulate closeDef (xs ++ replicate (tdHold blockyThrow + 20) (noInput, noInput)) start
         in isNothing (fHeld (mP1 m)) && isNothing (fHeld (mP2 m))
