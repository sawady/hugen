-- | Draw a match with rectangles: placeholder art, collision boxes and life
-- bars. Reads the core's state, never changes it.
module Hugen.SDL.Draw
  ( screenWidth,
    screenHeight,
    Art,
    drawMatch,
  )
where

import Control.Monad (forM_, when)
import Data.Word (Word8)
import Foreign.C (CInt)
import Hugen.Anim (SpriteRef)
import Hugen.Box
import Hugen.Camera
import Hugen.Character (Character (..), Consts (..))
import Hugen.Example.Blocky (Part (..), Role (..))
import Hugen.Fighter
import Hugen.Fixed
import Hugen.Match
import qualified SDL
import SDL (V2 (..), V4 (..), ($=))

-- | Logical resolution; SDL scales it to the window.
screenWidth, screenHeight :: CInt
screenWidth = 320
screenHeight = 180

-- | Screen row of the floor.
groundY :: CInt
groundY = 160

-- | How to draw a character's sprites with rectangles.
type Art = SpriteRef -> [Part]

type Color = V4 Word8

drawMatch :: SDL.Renderer -> Bool -> MatchDef -> (Art, Art) -> Match -> IO ()
drawMatch r showBoxes md (art1, art2) m = do
  fill r (V4 120 150 190 255) (0, 0, screenWidth, screenHeight)
  fill r (V4 70 90 60 255) (0, groundY, screenWidth, screenHeight - groundY)
  drawEdges r (mdStage md) cam
  drawFighter r cam art1 (mdP1 md) (palette P1) (mP1 m)
  drawFighter r cam art2 (mdP2 md) (palette P2) (mP2 m)
  forM_ (mEntities m) $ \e ->
    drawFighter r cam (artOf (eOwner e)) (charOf (eOwner e)) (palette (eOwner e)) (eBody e)
  when showBoxes $ do
    drawBoxes r cam (mdP1 md) (mP1 m)
    drawBoxes r cam (mdP2 md) (mP2 m)
    forM_ (mEntities m) $ \e -> drawBoxes r cam (charOf (eOwner e)) (eBody e)
  drawLife r P1 (mdP1 md) (mP1 m)
  drawLife r P2 (mdP2 md) (mP2 m)
  case mOutcome m of
    Nothing -> pure ()
    Just _ -> fill r (V4 240 220 80 255) (screenWidth `div` 2 - 20, 30, 40, 6)
  where
    cam = mCamera m
    artOf P1 = art1
    artOf P2 = art2
    charOf P1 = mdP1 md
    charOf P2 = mdP2 md

palette :: Side -> Role -> Color
palette side role = case (side, role) of
  (P1, Head) -> V4 240 200 170 255
  (P1, Torso) -> V4 60 110 200 255
  (P1, Limb) -> V4 40 70 140 255
  (P2, Head) -> V4 240 200 170 255
  (P2, Torso) -> V4 200 70 60 255
  (P2, Limb) -> V4 140 40 40 255

-- | Stage x to screen x.
toScreenX :: Camera -> Fx -> CInt
toScreenX cam x = fromIntegral (toPixels (x - camX cam)) + screenWidth `div` 2

toScreenY :: Camera -> Fx -> CInt
toScreenY cam y = groundY + fromIntegral (toPixels (y - camY cam))

boxRect :: Camera -> Box -> (CInt, CInt, CInt, CInt)
boxRect cam (Box l t r b) =
  let x = toScreenX cam l
      y = toScreenY cam t
   in (x, y, max 1 (toScreenX cam r - x), max 1 (toScreenY cam b - y))

drawFighter :: SDL.Renderer -> Camera -> Art -> Character -> (Role -> Color) -> Fighter -> IO ()
drawFighter r cam art c colors f =
  case currentSprite c f of
    Nothing -> pure ()
    Just s -> forM_ (art s) $ \(Part role bx) ->
      fill r (flash (colors role)) (boxRect cam (placeBox (fFacing f) (fX f, fY f) bx))
  where
    -- frozen fighters (hit pause) are drawn lighter
    flash col@(V4 cr cg cb ca)
      | fPause f > 0 = V4 (lighten cr) (lighten cg) (lighten cb) ca
      | otherwise = col
    lighten v = v + (255 - v) `div` 3

drawBoxes :: SDL.Renderer -> Camera -> Character -> Fighter -> IO ()
drawBoxes r cam c f = do
  forM_ (hurtboxes c f) $ outline r (V4 60 120 255 255) . boxRect cam
  forM_ (hitboxes c f) $ outline r (V4 255 40 40 255) . boxRect cam
  -- the axis
  let x = toScreenX cam (fX f)
      y = toScreenY cam (fY f)
  fill r (V4 255 255 255 255) (x - 1, y - 1, 3, 3)

drawEdges :: SDL.Renderer -> Stage -> Camera -> IO ()
drawEdges r st cam =
  forM_ [stageLeft st, stageRight st] $ \x ->
    fill r (V4 40 50 40 255) (toScreenX cam x - 1, 0, 2, screenHeight)

drawLife :: SDL.Renderer -> Side -> Character -> Fighter -> IO ()
drawLife r side c f = do
  let full = 120 :: CInt
      maxLife = max 1 (cLife (charConsts c))
      w = fromIntegral (max 0 (fLife f)) * full `div` fromIntegral maxLife
      x0 = case side of
        P1 -> 12
        P2 -> screenWidth - 12 - full
      xw = case side of
        P1 -> x0 + (full - w)
        P2 -> x0
  fill r (V4 30 30 30 255) (x0 - 1, 9, full + 2, 8)
  fill r (V4 230 200 40 255) (xw, 10, w, 6)

fill :: SDL.Renderer -> Color -> (CInt, CInt, CInt, CInt) -> IO ()
fill r c (x, y, w, h) = do
  SDL.rendererDrawColor r $= c
  SDL.fillRect r (Just (SDL.Rectangle (SDL.P (V2 x y)) (V2 w h)))

outline :: SDL.Renderer -> Color -> (CInt, CInt, CInt, CInt) -> IO ()
outline r c (x, y, w, h) = do
  SDL.rendererDrawColor r $= c
  SDL.drawRect r (Just (SDL.Rectangle (SDL.P (V2 x y)) (V2 w h)))
