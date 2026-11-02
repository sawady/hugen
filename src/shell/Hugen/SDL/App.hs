{-# LANGUAGE OverloadedStrings #-}

-- | The SDL shell: window, fixed-rate loop, keyboard and drawing around the
-- pure core.
--
-- The loop measures real time only to decide how many ticks to run. The
-- core always advances in whole ticks of 1/60 s and never sees the clock,
-- so a slow frame runs several ticks and a fast one may run none.
module Hugen.SDL.App
  ( runTraining,
    renderSnapshot,
  )
where

import qualified Config
import Control.Lens ((^.))
import Control.Exception (catch)
import Control.Monad (unless)
import Hugen.Example.Blocky (blockyArt)
import Hugen.Example.Demo (demoDef)
import Hugen.Input (Pad, noInput)
import Hugen.Match
import Hugen.SDL.Draw
import Hugen.SDL.Keyboard
import Foreign.C.String (withCString)
import qualified SDL
import qualified SDL.Raw.Video as SDL.Raw
import SDL (V2 (..), ($=))

ticksPerSecond :: Double
ticksPerSecond = 60

-- | Never try to catch up more than this many seconds (after a breakpoint,
-- a dragged window...).
maxCatchUp :: Double
maxCatchUp = 0.25

data Loop = Loop
  { lMatch :: Match,
    lAccum :: Double,
    lLast :: Double,
    lBoxes :: Bool
  }

-- | Training room: player 1 on the keyboard against a standing dummy.
--
-- Keys: arrows, Z X C (A B C), A S D (X Y Z). F1 toggles collision boxes,
-- F5 resets, Escape quits.
runTraining :: IO ()
runTraining = do
  cfg <- Config.load
  SDL.initialize [SDL.InitVideo, SDL.InitEvents]
  window <-
    SDL.createWindow
      (cfg ^. Config.title)
      SDL.defaultWindow
        { SDL.windowInitialSize = V2 (cfg ^. Config.width) (cfg ^. Config.height),
          SDL.windowResizable = True
        }
  -- Prefer the GPU; fall back to software (no GPU, remote sessions, CI).
  renderer <-
    SDL.createRenderer window (-1) SDL.defaultRenderer
      `catch` \e -> do
        putStrLn ("hugen: no accelerated renderer (" ++ show (e :: SDL.SDLException) ++ "), using software")
        SDL.createRenderer window (-1) SDL.defaultRenderer {SDL.rendererType = SDL.SoftwareRenderer}
  SDL.rendererLogicalSize renderer $= Just (V2 screenWidth screenHeight)
  t0 <- SDL.time
  loop renderer (Loop (newMatch demoDef) 0 t0 True)
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

loop :: SDL.Renderer -> Loop -> IO ()
loop renderer st = do
  events <- map SDL.eventPayload <$> SDL.pollEvents
  let quit = any isQuit events
      boxes = if any (pressedKey SDL.KeycodeF1) events then not (lBoxes st) else lBoxes st
      reset = any (pressedKey SDL.KeycodeF5) events
  down <- SDL.getKeyboardState
  now <- SDL.time
  let p1 = padFrom player1Layout down
      accum = min maxCatchUp (lAccum st + now - lLast st)
      n = floor (accum * ticksPerSecond) :: Int
      m0 = if reset then newMatch demoDef else lMatch st
      m1 = runTicks n (p1, noInput) m0
      st' = Loop m1 (accum - fromIntegral n / ticksPerSecond) now boxes
  drawMatch renderer boxes demoDef (blockyArt, blockyArt) m1
  SDL.present renderer
  SDL.delay 1
  unless quit (loop renderer st')

runTicks :: Int -> (Pad, Pad) -> Match -> Match
runTicks n pads m = foldl' (\acc _ -> step demoDef pads acc) m [1 .. n]

isQuit :: SDL.EventPayload -> Bool
isQuit SDL.QuitEvent = True
isQuit e = pressedKey SDL.KeycodeEscape e

pressedKey :: SDL.Keycode -> SDL.EventPayload -> Bool
pressedKey k (SDL.KeyboardEvent e) =
  SDL.keyboardEventKeyMotion e == SDL.Pressed
    && not (SDL.keyboardEventRepeat e)
    && SDL.keysymKeycode (SDL.keyboardEventKeysym e) == k
pressedKey _ _ = False

-- | Draw a match into a BMP file with SDL's software renderer. Needs no
-- window, so it works headless (CI, WSL without a display).
renderSnapshot :: FilePath -> Bool -> Match -> IO ()
renderSnapshot path boxes m = do
  SDL.initialize [SDL.InitVideo]
  surface <- SDL.createRGBSurface (V2 screenWidth screenHeight) SDL.RGBA8888
  renderer <- SDL.createSoftwareRenderer surface
  drawMatch renderer boxes demoDef (blockyArt, blockyArt) m
  SDL.present renderer
  let SDL.Surface ptr _ = surface
  _ <- withCString path (SDL.Raw.saveBMP ptr)
  SDL.destroyRenderer renderer
  SDL.freeSurface surface
  SDL.quit
