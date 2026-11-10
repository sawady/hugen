module Main (main) where

import Hugen.Example.Demo (demoDef, demoScript)
import Hugen.Fighter (Fighter (..))
import Hugen.Match
import Hugen.SDL.App (renderSnapshot, runTraining)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

usage :: String
usage =
  unlines
    [ "usage: hugen                      training room (needs a display)",
      "       hugen --headless           run the scripted demo without SDL",
      "       hugen --snapshot FILE TICK draw tick TICK of the demo to a BMP"
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runTraining
    ["--headless"] -> headless
    ["--snapshot", file, tick] | Just n <- readMaybe tick -> do
      let m = simulate demoDef (take n demoScript) (newMatch demoDef)
      renderSnapshot file True m
      putStrLn (file ++ ": tick " ++ show (mTick m))
    _ -> putStr usage >> exitFailure

-- | Print a line every 30 ticks of the scripted demo.
headless :: IO ()
headless = go (newMatch demoDef) demoScript
  where
    go m inputs = do
      if mTick m `mod` 30 == 0 || null inputs then putStrLn (summary m) else pure ()
      case inputs of
        [] -> pure ()
        (i : rest) -> go (step demoDef i m) rest

summary :: Match -> String
summary m =
  "tick "
    ++ pad 4 (show (mTick m))
    ++ " | P1 "
    ++ fighter (mP1 m)
    ++ " | P2 "
    ++ fighter (mP2 m)
    ++ entities
    ++ maybe "" ((" | " ++) . show) (mOutcome m)
  where
    entities = case length (mEntities m) of
      0 -> ""
      n -> " | " ++ show n ++ (if n == 1 then " entity" else " entities")
    fighter f =
      "state "
        ++ pad 4 (show (fState f))
        ++ " x "
        ++ pad 7 (show (fX f))
        ++ " y "
        ++ pad 6 (show (fY f))
        ++ " life "
        ++ pad 4 (show (fLife f))
    pad n s = replicate (n - length s) ' ' ++ s
