-- | A scripted match between two Blockys, used by the headless mode of the
-- executable and by the tests. Inputs are written as runs: hold this pad
-- for that many ticks.
module Hugen.Example.Demo
  ( demoDef,
    demoScript,
    comboScript,
    runs,
  )
where

import Hugen.Camera (defaultStage)
import Hugen.Example.Blocky (blocky)
import Hugen.Input
import Hugen.Match

demoDef :: MatchDef
demoDef = MatchDef blocky blocky defaultStage

-- | Expand @(ticks, pad)@ runs into one pad per tick.
runs :: [(Int, Pad)] -> [Pad]
runs = concatMap (\(n, p) -> replicate n p)

-- | Player 1 walks in, jabs and cancels the jab into the rush (a
-- quarter circle forward + A typed during the hit pause).
comboScript :: [Pad]
comboScript =
  runs
    [ (30, pad [KRight]),
      (10, noInput),
      (1, pad [KA]),
      (3, noInput),
      (2, pad [KDown]),
      (2, pad [KDown, KRight]),
      (2, pad [KRight]),
      (1, pad [KRight, KA]),
      (60, noInput)
    ]

-- | The combo, then a jump-in air punch, an uppercut, a fireball thrown
-- after stepping back and a throw after walking in again; player 2 stands
-- still.
demoScript :: [(Pad, Pad)]
demoScript = zip p1 (repeat noInput)
  where
    p1 =
      comboScript
        ++ runs
          [ (12, pad [KLeft]),
            (4, pad [KUp, KRight]),
            (20, noInput),
            (1, pad [KA]),
            (50, noInput),
            (6, pad [KRight]),
            (12, noInput),
            (2, pad [KRight]),
            (2, pad [KDown]),
            (2, pad [KDown, KRight]),
            (1, pad [KB]),
            (90, noInput),
            (20, pad [KLeft]),
            (2, pad [KDown]),
            (2, pad [KDown, KLeft]),
            (1, pad [KLeft, KA]),
            (60, noInput),
            (30, pad [KRight]),
            (1, pad [KC]),
            (100, noInput)
          ]
