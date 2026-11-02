-- | Common states every character can reuse: standing, walking, crouching,
-- jumping, landing and getting hit. The equivalent of MUGEN's
-- @common1.cns@, with the same state numbers.
--
-- A character supplies the animations these states use ('commonAnims'
-- lists them) and adds its own attacks on top.
module Hugen.Common
  ( -- * State numbers
    stStand,
    stCrouch,
    stWalk,
    stJumpStart,
    stJumpAir,
    stLand,
    stHit,
    stHitAir,

    -- * Animation numbers
    anStand,
    anCrouch,
    anWalk,
    anJumpStart,
    anJumpAir,
    anLand,
    anHit,
    anHitAir,
    commonAnims,

    -- * States
    commonStates,
    attackState,
    jumpStartTicks,
    landTicks,
  )
where

import Hugen.Anim (AnimId)
import Hugen.Character
import Hugen.Expr
import Hugen.Fixed (Fx)
import Hugen.Input (Dir (..))

stStand, stCrouch, stWalk, stJumpStart, stJumpAir, stLand, stHit, stHitAir :: StateId
stStand = 0
stCrouch = 11
stWalk = 20
stJumpStart = 40
stJumpAir = 50
stLand = 52
stHit = 5000
stHitAir = 5030

anStand, anCrouch, anWalk, anJumpStart, anJumpAir, anLand, anHit, anHitAir :: AnimId
anStand = 0
anCrouch = 11
anWalk = 20
anJumpStart = 40
anJumpAir = 41
anLand = 47
anHit = 5000
anHitAir = 5030

-- | Animations the common states need.
commonAnims :: [AnimId]
commonAnims = [anStand, anCrouch, anWalk, anJumpStart, anJumpAir, anLand, anHit, anHitAir]

-- | Ticks on the floor before a jump leaves it.
jumpStartTicks :: Int
jumpStartTicks = 3

-- | Ticks of landing recovery.
landTicks :: Int
landTicks = 2

lit' :: Fx -> Expr Fx
lit' = lit

-- | The common states for a character with these constants.
commonStates :: Consts -> [(StateId, StateDef)]
commonStates k =
  [ ( stStand,
      (stateDef Standing Idle PhysStand)
        { sdAnim = Just anStand,
          sdCtrl = Just True,
          sdControllers =
            [ on (ctrl .&& holding Up) (changeState stJumpStart),
              on (ctrl .&& holding Down) (changeState stCrouch),
              on (ctrl .&& (holding Fwd .|| holding Back)) (changeState stWalk)
            ]
        }
    ),
    ( stWalk,
      (stateDef Standing Idle PhysStand)
        { sdAnim = Just anWalk,
          sdCtrl = Just True,
          sdControllers =
            [ on (holding Up) (changeState stJumpStart),
              on (holding Down) (changeState stCrouch),
              on (holding Fwd) (velSetX (lit' (cWalkFwd k))),
              on (holding Back) (velSetX (lit' (cWalkBack k))),
              on (notE (holding Fwd .|| holding Back)) (changeState stStand)
            ]
        }
    ),
    ( stCrouch,
      (stateDef Crouching Idle PhysCrouch)
        { sdAnim = Just anCrouch,
          sdCtrl = Just True,
          sdControllers =
            [on (notE (holding Down)) (changeState stStand)]
        }
    ),
    ( stJumpStart,
      (stateDef Standing Idle PhysStand)
        { sdAnim = Just anJumpStart,
          sdCtrl = Just False,
          sdControllers =
            [ on
                (time .>= lit jumpStartTicks)
                ( velSet
                    (ifE (holding Fwd) (lit' (cJumpFwd k)) (ifE (holding Back) (lit' (cJumpBack k)) 0))
                    (lit' (cJumpUp k))
                ),
              on (time .>= lit jumpStartTicks) (changeState stJumpAir)
            ]
        }
    ),
    ( stJumpAir,
      (stateDef Airborne Idle PhysAir)
        { sdAnim = Just anJumpAir,
          sdCtrl = Just True
        }
    ),
    ( stLand,
      (stateDef Standing Idle PhysStand)
        { sdAnim = Just anLand,
          sdCtrl = Just False,
          sdVel = Just (0, 0),
          sdControllers = [on (time .>= lit landTicks) (changeState stStand)]
        }
    ),
    ( stHit,
      (stateDef Standing BeingHit PhysStand)
        { sdAnim = Just anHit,
          sdCtrl = Just False,
          sdControllers = [on hitOver (changeState stStand)]
        }
    ),
    ( stHitAir,
      (stateDef Airborne BeingHit PhysAir)
        { sdAnim = Just anHitAir,
          sdCtrl = Just False
        }
    )
  ]

-- | A grounded or airborne attack: the animation plays once, the hit
-- becomes active on the first tick of frame @activeElem@, and when the
-- animation ends the fighter goes back to @after@.
attackState :: Stance -> AnimId -> HitDef -> Int -> StateId -> StateDef
attackState st an h activeElem after =
  (stateDef st Attacking physics')
    { sdAnim = Just an,
      sdCtrl = Just False,
      sdVel = if st == Airborne then Nothing else Just (0, 0),
      sdControllers =
        [ on (animElemStarts activeElem) (attack h),
          on (animTime .>= 0) (changeState after)
        ]
    }
  where
    physics' = case st of
      Airborne -> PhysAir
      Crouching -> PhysCrouch
      _ -> PhysStand
