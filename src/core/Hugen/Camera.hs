-- | The stage and the camera.
--
-- The camera is part of the simulation, not of the renderer, because it
-- constrains gameplay: fighters cannot walk out of the visible area (MUGEN's
-- screen edges). The renderer only reads it.
module Hugen.Camera
  ( Stage (..),
    defaultStage,
    stageCenter,
    Camera (..),
    cameraFor,
    viewBounds,
    insideView,
  )
where

import Hugen.Fixed

data Stage = Stage
  { stageLeft :: !Fx,
    stageRight :: !Fx,
    -- | Width of the visible area in stage units.
    stageViewWidth :: !Fx,
    -- | How close to the edge of the view a fighter's axis may get.
    stageEdgeMargin :: !Fx,
    -- | Distance between the fighters' axes at the start of a round.
    stageStartDistance :: !Fx,
    -- | How far past the edge of the view a projectile's axis may go before
    -- the projectile is removed.
    stageOffscreenMargin :: !Fx
  }
  deriving (Eq, Show)

-- | A stage twice as wide as a 320-unit view, centered on 0.
defaultStage :: Stage
defaultStage =
  Stage
    { stageLeft = -320,
      stageRight = 320,
      stageViewWidth = 320,
      stageEdgeMargin = 16,
      stageStartDistance = 140,
      stageOffscreenMargin = 32
    }

stageCenter :: Stage -> Fx
stageCenter s = halve (stageLeft s + stageRight s)

-- | The point the view is centered on. @camY@ is 0 or negative (up).
data Camera = Camera
  { camX :: !Fx,
    camY :: !Fx
  }
  deriving (Eq, Show)

-- | Center on the midpoint of the fighters, without showing past the stage
-- edges. Vertically, follow half of their average height, so a single jump
-- moves the view a little and a long air combo moves it a lot.
cameraFor :: Stage -> (Fx, Fx) -> (Fx, Fx) -> Camera
cameraFor st (x1, y1) (x2, y2) = Camera x y
  where
    half = halve (stageViewWidth st)
    lo = stageLeft st + half
    hi = stageRight st - half
    x
      | lo > hi = stageCenter st
      | otherwise = max lo (min hi (halve (x1 + x2)))
    y = min 0 (halve (halve (y1 + y2)))

-- | Leftmost and rightmost x a fighter's axis may take with this camera.
viewBounds :: Stage -> Camera -> (Fx, Fx)
viewBounds st cam = (max (stageLeft st) l, min (stageRight st) r)
  where
    half = halve (stageViewWidth st)
    m = stageEdgeMargin st
    l = camX cam - half + m
    r = camX cam + half - m

-- | Is this x within the visible area widened by @margin@ on each side?
insideView :: Stage -> Camera -> Fx -> Fx -> Bool
insideView st cam margin x = x >= camX cam - half - margin && x <= camX cam + half + margin
  where
    half = halve (stageViewWidth st)
