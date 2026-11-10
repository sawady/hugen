-- | Common states every character can reuse: standing, walking, crouching,
-- jumping, landing, guarding, getting hit and being knocked down. The equivalent of MUGEN's
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
    stGuardStand,
    stGuardCrouch,
    stHit,
    stHitHeavy,
    stHitAir,
    stFall,
    stLie,
    stGetUp,

    -- * Animation numbers
    anStand,
    anCrouch,
    anWalk,
    anJumpStart,
    anJumpAir,
    anLand,
    anGuardStand,
    anGuardCrouch,
    anHit,
    anHitHeavy,
    anHitAir,
    anFall,
    anLie,
    anGetUp,
    commonAnims,

    -- * States
    commonStates,
    attackState,
    jumpStartTicks,
    landTicks,
    lieTicks,
    getUpTicks,
  )
where

import Hugen.Anim (AnimId)
import Hugen.Character
import Hugen.Expr
import Hugen.Fixed (Fx)
import Hugen.Input (Dir (..))

stStand, stCrouch, stWalk, stJumpStart, stJumpAir, stLand, stGuardStand, stGuardCrouch :: StateId
stHit, stHitHeavy, stHitAir, stFall, stLie, stGetUp :: StateId
stStand = 0
stCrouch = 11
stWalk = 20
stJumpStart = 40
stJumpAir = 50
stLand = 52
stGuardStand = 150
stGuardCrouch = 152
stHit = 5000
stHitHeavy = 5001
stHitAir = 5030
stFall = 5050
stLie = 5110
stGetUp = 5120

anStand, anCrouch, anWalk, anJumpStart, anJumpAir, anLand, anGuardStand, anGuardCrouch :: AnimId
anHit, anHitHeavy, anHitAir, anFall, anLie, anGetUp :: AnimId
anStand = 0
anCrouch = 11
anWalk = 20
anJumpStart = 40
anJumpAir = 41
anLand = 47
anGuardStand = 150
anGuardCrouch = 152
anHit = 5000
anHitHeavy = 5001
anHitAir = 5030
anFall = 5050
anLie = 5110
anGetUp = 5120

-- | Animations the common states need.
commonAnims :: [AnimId]
commonAnims =
  [ anStand,
    anCrouch,
    anWalk,
    anJumpStart,
    anJumpAir,
    anLand,
    anGuardStand,
    anGuardCrouch,
    anHit,
    anHitHeavy,
    anHitAir,
    anFall,
    anLie,
    anGetUp
  ]

-- | Ticks on the floor before a jump leaves it.
jumpStartTicks :: Int
jumpStartTicks = 3

-- | Ticks of landing recovery.
landTicks :: Int
landTicks = 2

-- | Ticks lying on the floor after a knockdown, before getting up.
lieTicks :: Int
lieTicks = 30

-- | Ticks getting up. Still 'Lying': the fighter cannot be hit yet.
getUpTicks :: Int
getUpTicks = 20

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
    ( stGuardStand,
      (stateDef Standing BeingHit PhysStand)
        { sdAnim = Just anGuardStand,
          sdCtrl = Just False,
          sdControllers = [on hitOver (changeState stStand)]
        }
    ),
    ( stGuardCrouch,
      (stateDef Crouching BeingHit PhysCrouch)
        { sdAnim = Just anGuardCrouch,
          sdCtrl = Just False,
          sdControllers = [on hitOver (changeState stCrouch)]
        }
    ),
    ( stHit,
      (stateDef Standing BeingHit PhysStand)
        { sdAnim = Just anHit,
          sdCtrl = Just False,
          sdControllers = [on hitOver (changeState stStand)]
        }
    ),
    ( stHitHeavy,
      (stateDef Standing BeingHit PhysStand)
        { sdAnim = Just anHitHeavy,
          sdCtrl = Just False,
          sdControllers = [on hitOver (changeState stStand)]
        }
    ),
    ( stHitAir,
      (stateDef Airborne BeingHit PhysAir)
        { sdAnim = Just anHitAir,
          sdCtrl = Just False
        }
    ),
    ( stFall,
      (stateDef Airborne BeingHit PhysAir)
        { sdAnim = Just anFall,
          sdCtrl = Just False,
          sdLandState = Just stLie
        }
    ),
    ( stLie,
      (stateDef Lying BeingHit PhysStand)
        { sdAnim = Just anLie,
          sdCtrl = Just False,
          sdVel = Just (0, 0),
          sdControllers = [on (time .>= lit lieTicks) (changeState stGetUp)]
        }
    ),
    ( stGetUp,
      (stateDef Lying BeingHit PhysStand)
        { sdAnim = Just anGetUp,
          sdCtrl = Just False,
          sdControllers = [on (time .>= lit getUpTicks) (changeState stStand)]
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
