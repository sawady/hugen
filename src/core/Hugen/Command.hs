-- | Motion commands (quarter circles, charges, dashes...) and the input
-- history that recognizes them.
--
-- Each fighter keeps the last few seconds of facing-relative input in its
-- state (never outside it: the history is gameplay state, it has to be
-- replayed and rolled back with everything else). Every tick the history is
-- matched against the character's command list, and the commands that just
-- completed enter the /command buffer/, where they stay active for a few
-- ticks so that a move typed slightly early still comes out.
--
-- Semantics, modeled on MUGEN's CMD files:
--
-- * A command is a sequence of 'Step's. The last step must happen on the
--   current tick, and the first one must have been happening at most
--   'cmdTime' ticks ago.
-- * Steps are matched as a subsequence: other input in between is allowed
--   (a sloppy @↓ → ↘ →@ still is a quarter circle).
-- * A held direction is one press, however long it is held: a direction
--   step matches its whole run of ticks. Holding down for a second before
--   rolling to down-forward costs nothing, and two 'Press' steps of the same
--   direction (a dash, @F, F@) need two separate runs.
module Hugen.Command
  ( -- * Defining commands
    Step (..),
    Command (..),
    command,
    defaultTime,
    defaultBuffer,
    chordWindow,

    -- * Input history
    History,
    emptyHistory,
    recordInput,
    inputAt,
    historyCapacity,

    -- * Recognition
    matches,
    recognize,
    CommandBuffer,
    emptyBuffer,
    updateBuffer,
    isActive,
  )
where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq, (<|))
import qualified Data.Sequence as Seq
import Hugen.Input

-- | One step of a command.
data Step
  = -- | The direction changes to exactly this one (MUGEN @D@ / @~D@).
    Press Dir
  | -- | The direction changes to one that includes this one (MUGEN @$D@):
    -- @PressLoose Down@ accepts down-back and down-forward too.
    PressLoose Dir
  | -- | The direction (loose) is held on the tick of the next step, or now
    -- if it is the last one (MUGEN @/D@). For "forward + A" style normals.
    Hold Dir
  | -- | The direction (loose) was held for at least @n@ ticks and is
    -- released now (MUGEN @~n$D@). Charge moves start with this.
    Charge Dir Int
  | -- | These buttons are pressed together: all held now, and each one
    -- pressed within the last 'chordWindow' ticks (MUGEN @a+b@).
    Buttons [Btn]
  | -- | The button is released now (MUGEN @~a@).
    Release Btn
  deriving (Eq, Show)

data Command = Command
  { cmdName :: String,
    cmdSteps :: [Step],
    -- | Maximum ticks between the first and the last step.
    cmdTime :: Int,
    -- | Ticks the command stays active in the buffer after it completes.
    cmdBuffer :: Int
  }
  deriving (Eq, Show)

-- | A command with the default timing.
command :: String -> [Step] -> Command
command name steps = Command name steps defaultTime defaultBuffer

-- | Same default as MUGEN's @command.time@.
defaultTime :: Int
defaultTime = 15

-- | Ticks a completed command stays active. MUGEN defaults to 1; modern
-- games are more lenient. An open design question, see docs/design.md.
defaultBuffer :: Int
defaultBuffer = 3

-- | How many ticks apart the buttons of a 'Buttons' chord may be pressed.
chordWindow :: Int
chordWindow = 2

-- | The last ticks of relative input, most recent first.
data History = History
  { historyCapacity :: !Int,
    historyInputs :: !(Seq RelInput)
  }
  deriving (Eq, Show)

-- | A history that remembers @n@ ticks. It must cover the longest
-- 'cmdTime' plus the longest 'Charge'.
emptyHistory :: Int -> History
emptyHistory n = History (max 2 n) Seq.empty

recordInput :: RelInput -> History -> History
recordInput i (History cap xs) = History cap (Seq.take cap (i <| xs))

-- | Input @age@ ticks ago (0 is the current tick). Ticks older than the
-- history are neutral.
inputAt :: History -> Int -> RelInput
inputAt (History _ xs) age = fromMaybe released (Seq.lookup age xs)
  where
    released = RelInput Neutral []

-- | Does the step's event happen at this age?
occursAt :: History -> Step -> Int -> Bool
occursAt h st age = case st of
  Press d -> dirAt age == d && dirAt (age + 1) /= d
  PressLoose d -> dirHas (dirAt age) d && not (dirHas (dirAt (age + 1)) d)
  Hold d -> dirHas (dirAt age) d
  Charge d n ->
    not (dirHas (dirAt age) d)
      && all (\k -> dirHas (dirAt k) d) [age + 1 .. age + n]
  Buttons bs ->
    not (null bs)
      && all (heldAt age) bs
      && any (pressedAt age) bs
      && all (\b -> any (`pressedAt` b) [age .. age + chordWindow - 1]) bs
  Release b -> not (heldAt age b) && heldAt (age + 1) b
  where
    dirAt = riDir . inputAt h
    heldAt a b = b `elem` riBtns (inputAt h a)
    pressedAt a b = heldAt a b && not (heldAt (a + 1) b)

-- | For direction steps: is the direction held at this age? Any tick of
-- the run counts as the step.
heldDir :: History -> Step -> Int -> Bool
heldDir h st age = case st of
  Press d -> riDir (inputAt h age) == d
  PressLoose d -> dirHas (riDir (inputAt h age)) d
  _ -> False

isRunStep :: Step -> Bool
isRunStep st = case st of
  Press _ -> True
  PressLoose _ -> True
  _ -> False

-- | Must the earlier step happen strictly before the later one? Two events
-- of the same kind cannot share a tick (one press cannot count twice in
-- @F, F@), but a direction and a button can (@↓ ↘ →+A@ pressed together).
strictlyBefore :: Step -> Step -> Bool
strictlyBefore earlier later = family earlier == family later
  where
    family :: Step -> Int
    family s = case s of
      Press _ -> 0
      PressLoose _ -> 0
      Hold _ -> 1
      Charge _ _ -> 2
      Buttons _ -> 3
      Release _ -> 4

-- | Where a step was found: the age that counts for timing (the most recent
-- tick of a run) and the age where it began (the press). Older steps must
-- be found before the beginning.
data Found = Found {foundAge :: !Int, foundStart :: !Int}

-- | Did the command complete on the current tick?
matches :: History -> Command -> Bool
matches h cmd = case reverse (cmdSteps cmd) of
  [] -> False
  (lastStep : older) -> occursAt h lastStep 0 && go lastStep (Found 0 0) older
  where
    limit = cmdTime cmd
    cap = historyCapacity h
    -- Walk the steps from the most recent to the oldest. Taking the most
    -- recent occurrence of each step is optimal (it leaves the most room for
    -- the older ones), except right before a 'Hold', whose tick is forced by
    -- the step after it; there every candidate is tried.
    go :: Step -> Found -> [Step] -> Bool
    go _ _ [] = True
    go later found (st : rest) = case st of
      Hold _ ->
        let a = foundAge found
         in occursAt h st a && go st (Found a a) rest
      _ ->
        let lo
              | strictlyBefore st later = foundStart found + 1
              | otherwise = foundStart found
            candidates
              | isRunStep st = runsFrom st lo
              | otherwise = [Found a a | a <- [lo .. limit], occursAt h st a]
            tryAll = case rest of
              (Hold _ : _) -> candidates
              _ -> take 1 candidates
         in any (\f -> go st f rest) tryAll
    -- Runs of a direction step, most recent first, whose most recent tick at
    -- or after @lo@ is within the command time.
    runsFrom st lo = case [a | a <- [lo .. limit], heldDir h st a] of
      [] -> []
      (a : _) ->
        let begin = last (takeWhile (heldDir h st) [a .. cap])
         in Found a begin : runsFrom st (begin + 1)

-- | Names of the commands that complete on the current tick.
recognize :: [Command] -> History -> [Command]
recognize cmds h = filter (matches h) cmds

-- | Active commands and the ticks they have left.
type CommandBuffer = Map.Map String Int

emptyBuffer :: CommandBuffer
emptyBuffer = Map.empty

-- | Age the buffer one tick (unless the fighter is frozen in hit pause, so
-- that input typed during the pause survives it) and add the commands
-- completed now.
updateBuffer :: Bool -> [Command] -> CommandBuffer -> CommandBuffer
updateBuffer frozen done buf = foldr add aged done
  where
    aged
      | frozen = buf
      | otherwise = Map.filter (> 0) (Map.map (subtract 1) buf)
    add c = Map.insertWith max (cmdName c) (cmdBuffer c)

isActive :: String -> CommandBuffer -> Bool
isActive name buf = maybe False (> 0) (Map.lookup name buf)
