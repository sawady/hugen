# Hugen design

This document describes how Hugen is built and why. It covers the architecture, the
tick, input and commands, the state/trigger language, collisions, the camera and how
characters are defined. Sections marked **(implemented)** describe code that exists
today in `src/core`; **(proposed)** marks what comes next. Open questions are listed
at the end.

Hugen is inspired by [MUGEN](https://en.wikipedia.org/wiki/Mugen_(game_engine)) and
looks at [Ikemen GO](https://github.com/ikemen-engine/Ikemen-GO) for how a modern
engine extends the same model. It reuses their *ideas* (numbered states, triggers,
command strings, collision boxes per animation frame) but not their code or their file
formats.

## Goals

- **A fighting game engine whose game logic is a pure function.** Same inputs, same
  match, on every machine.
- **Characters as states and triggers**, the MUGEN model, expressed as a typed
  embedded language in Haskell.
- **60 ticks per second, always.** Everything in gameplay is measured in ticks.
- **Testable without a window.** Every rule can be checked by a unit test or a
  property; the renderer is a thin layer on top.

Not goals: MUGEN compatibility (loading `.def`/`.cns`/`.air`/`.sff` files), which is
ruled out for good. Online play and 3D are also out, at least for now.

## 1. Architecture (implemented)

```
src/core/   hugen-core: the simulation. Pure, deterministic. Depends on base and containers only.
src/shell/  hugen: SDL2 window, keyboard, rendering, the real-time loop.
app/        the executable: training room, headless demo, snapshot.
test/       hspec + QuickCheck, against hugen-core only.
```

`hugen-core` is a separate (internal) library on purpose. It cannot import SDL, `IO`
helpers, the clock or a random generator, because they are not among its dependencies:
**the build enforces the boundary**, there is no convention to forget. The shell
imports the core, never the other way around.

Why a pure core pays off in a fighting game:

- **Frame data in tests.** "The jab is active on its fourth tick" is an assertion, not
  something you check by eye.
- **Replays for free.** A replay is the list of pads per tick; feeding it to `simulate`
  reproduces the match exactly. Recorded replays become a regression suite.
- **Rollback netcode later.** GGPO-style rollback needs a serializable state and a
  deterministic `step` to re-simulate. Both are already there.
- **No platform drift.** No floats (see below), no clock, no unordered iteration.

### Determinism rules

Inside `hugen-core`:

1. **No floating point in gameplay.** Positions, velocities and distances are `Fx`, a
   fixed-point number counting 1/256 of a pixel (`Hugen.Fixed`). Literals like `2.5`
   still work through `Num`/`Fractional` instances that round once, when the literal
   is converted to `Fx`.
2. **No clock.** Time is the tick number and per-state tick counters.
3. **No randomness**, until there is a seeded PRNG stored in the match state.
4. **No hidden state.** Everything that changes is in `Match`: positions, states,
   animation positions, life, the input history and the command buffer. `Match` has
   `Eq` and `Show`.
5. **Multiplication and division round toward zero**, so that negation commutes with
   them and the engine is exactly mirror-symmetric (a property test checks it).

### The real-time loop

The shell (`Hugen.SDL.App`) measures elapsed time only to decide how many ticks to run:

```
every frame: poll events, read keyboard -> accumulate elapsed time
             -> run floor(accumulated * 60) ticks of `step` -> draw the last state
```

The accumulator is capped at 0.25 s so a long pause does not trigger a burst of
catch-up ticks. The core never sees seconds. Rendering does not interpolate: a
fighting game is a sequence of discrete frames, and so is Hugen.

## 2. The tick (implemented)

`step :: MatchDef -> (Pad, Pad) -> Match -> Match` is the whole simulation. One tick:

1. **Input.** Each fighter appends this tick's pad, made facing-relative, to its input
   history; commands that complete now enter its command buffer.
2. **Controllers.** Each fighter runs its global controllers and then those of its
   current state (section 4). Both read the *opponent as it was before this phase*:
   player 1 is not processed "first".
3. **Physics.** Velocity is applied; then friction (standing, crouching) or gravity
   (airborne). An airborne fighter that reaches the floor while falling lands and
   enters its land state.
4. **Space.** Bodies push each other apart (each one half the overlap, rounded up so
   the push is symmetric); fighters are clamped to the stage and to the camera view;
   against a wall, only the other fighter moves.
5. **Hits.** Every active attack is tested against the opponent's hurtboxes, both on
   the same snapshot, so two attacks can **trade**.
6. **Clocks, then impact.** State time, animation and hit stun advance (or a tick of hit
   pause is spent). Then the hits found in step 5 are applied: damage, the defender's
   hit state, pushback, and a hit pause that starts on the *next* tick.
7. **Facing.** An idle fighter on the floor turns to face the opponent. A fighter in
   an attack does not: a move that crosses the opponent keeps its direction.
8. **Camera and round end.**

Two details in this order came from bugs found by tests:

- If the impact tick did not advance the clocks, a state whose attack is armed with
  `time == 0` would re-arm it after the hit pause and hit again. Applying the hit after
  the clocks (step 6) fixes it, and the test "every attack in the demo connects exactly
  once" keeps it fixed.
- Reading the opponent from a snapshot (step 2) and resolving hits on one snapshot
  (step 5) is what makes the mirror-symmetry property hold.

**Hit pause** ("hit stop") freezes both fighters: no controllers, physics or clocks.
Input is still recorded and recognized during the pause, and the command buffer does
not age, so a special typed during the pause comes out when it ends. That is how the
jab → rush cancel in the example works.

## 3. Input and commands (implemented)

### Pads

A `Pad` is the state of one controller on one tick: a bit per button (4 directions,
A B C X Y Z, start) in a `Word16`. It is **absolute**: "left", not "back". A replay is a
list of pad pairs.

Each tick the pad is made **relative** to the fighter's facing (`RelInput`: one of nine
directions plus the attack buttons). Opposite directions cancel (left + right is
neutral horizontally, up + down vertically), so a pad never means two contradictory
things. Move lists are written once, facing right, and work on both sides.

### History and command buffer

Each fighter keeps the last N ticks of relative input in its state (N is computed from
its command list). Keeping it in the state, and not in some input-device object, is
what keeps `step` pure and makes the history roll back with everything else.

A command is a list of steps plus timing:

```haskell
data Step
  = Press Dir          -- MUGEN "D":   the direction becomes exactly this one
  | PressLoose Dir     -- MUGEN "$D":  the direction becomes one that includes it
  | Hold Dir           -- MUGEN "/D":  held on the tick of the next step
  | Charge Dir Int     -- MUGEN "~30$B": held for n ticks, released now
  | Buttons [Btn]      -- MUGEN "a+b": pressed together
  | Release Btn        -- MUGEN "~a"

data Command = Command
  { cmdName :: String, cmdSteps :: [Step]
  , cmdTime :: Int     -- max ticks from the first step to the last (default 15)
  , cmdBuffer :: Int } -- ticks it stays active once complete (default 3)

qcfA = command "qcf_a" [Press Down, Press DownFwd, Press Fwd, Buttons [BtnA]]
dpB  = command "dp_b"  [Press Fwd, Press Down, Press DownFwd, Buttons [BtnB]]
dash = (command "ff" [Press Fwd, Press Fwd]) { cmdTime = 10 }
```

Matching rules:

- The **last step happens on the current tick**; the first one must have been
  happening at most `cmdTime` ticks ago.
- Steps match as a **subsequence**: noise in between is allowed (`↓ → ↘ → A` is still a
  quarter circle).
- **A held direction is one press.** A direction step matches its whole run of ticks,
  and timing counts from the most recent tick of the run. Holding down for a second
  before rolling to down-forward costs nothing; a dash (`F, F`) needs two separate runs
  of forward. (Measuring the window *between presses* rather than between ticks is a
  lesson from practice: otherwise holding a direction eats the tolerance and commands
  become stricter than players expect.)
- Two events of the same kind cannot share a tick; a direction and a button can
  (`→ + A` on the same tick completes `↓ ↘ → + A`).
- A `Buttons` chord completes on the tick its last button goes down, with all buttons
  pressed within `chordWindow` (2) ticks.
- The matcher is greedy from the most recent step backwards (optimal for subsequence
  matching) and only backtracks right before a `Hold`.

Completed commands go to the **command buffer** for `cmdBuffer` ticks, and the trigger
`cmd "qcf_a"` reads the buffer. The buffer is what lets a special typed a few ticks
before recovery ends still come out.

**Priority** between commands is not decided by the recognizer: several commands can
complete on the same tick (`qcf_a` and `a` do). The order of the global controllers
decides which one becomes a move, exactly like MUGEN's state -1: specials first, then
the normals they contain. Charge commands, being more specific, go before the motions
they overlap with.

## 4. States and triggers (implemented)

### The model

As in MUGEN's CNS:

- A character is always in exactly one **numbered state** (`StateDef`).
- A state has a **stance** (standing, crouching, airborne, lying), a **move type**
  (idle, attacking, being hit) and a **physics** mode (stand, crouch, air, none).
- Entering a state may set the **animation**, the **control flag** and the
  **velocity**.
- Every tick, the state's **controllers** run in order. A controller is a trigger and
  an action. If the trigger holds, the action runs.
- Before the current state, the character's **global controllers** run (MUGEN's state
  -1). That is where commands become moves.

```haskell
data StateDef = StateDef
  { sdStance :: Stance, sdMoveType :: MoveType, sdPhysics :: Physics
  , sdAnim :: Maybe AnimId, sdCtrl :: Maybe Bool, sdVel :: Maybe (Fx, Fx)
  , sdControllers :: [Controller] }

data Controller = Controller { ctrlTrigger :: Expr Bool, ctrlAction :: Action }

data Action
  = ChangeState StateId | ChangeAnim AnimId | SetCtrl Bool
  | VelSet (Maybe (Expr Fx)) (Maybe (Expr Fx)) | VelAdd (Expr Fx) (Expr Fx)
  | PosAdd (Expr Fx) (Expr Fx) | SetVar Int (Expr Int)
  | Attack HitDef | Turn
```

**`ChangeState` runs the new state on the same tick.** The rest of the current list is
skipped, the new state is entered (its time is 0) and its controllers run immediately,
so there is no tick of latency between a command and its move. At most 8 changes per
tick, so two states that jump to each other cannot hang the engine.

### Triggers: a typed deep embedding

Triggers are values of a GADT, not Haskell functions:

```haskell
data Expr a where
  Lit :: Show a => a -> Expr a
  Time, AnimTime, AnimElem, StateNo, Life :: Expr Int
  Ctrl, MoveContact, HitOver :: Expr Bool
  PosY, VelX, VelY, P2DistX :: Expr Fx
  CommandActive :: String -> Expr Bool
  Holding :: Dir -> Expr Bool
  Cmp :: (Ord b, Show b) => CmpOp -> Expr b -> Expr b -> Expr Bool
  And, Or :: Expr Bool -> Expr Bool -> Expr Bool
  Arith :: Num a => ArithOp -> Expr a -> Expr a -> Expr a
  If :: Expr Bool -> Expr a -> Expr a -> Expr a
  ...
```

with combinators that read like MUGEN triggers:

```haskell
on (cmd "qcf_a" .&& ctrl .&& grounded) (changeState 1000)
on (cmd "qcf_a" .&& stateNo .== 200 .&& moveContact) (changeState 1000)  -- cancel
on (animElemStarts 1) (attack hitDef { hdDamage = 70 })
on (animTime .>= 0) (changeState 0)
```

Why a deep embedding instead of `Fighter -> Bool`:

- **Types catch mistakes** that MUGEN only catches at runtime (comparing a position to a
  state number does not compile).
- **It is data.** It can be shown, inspected (which commands does a character read?
  which states can it reach? `validate` already checks state and animation
  references), serialized, and parsed from a text format later into the very same
  tree.
- **It cannot do IO or read anything outside the fighter and its opponent.** The
  evaluator (`Hugen.Fighter.eval`) is the only interpreter, and it is pure.

The cost is that every new query needs a constructor and an evaluator case. That is
also a feature: the set of things a character can observe is explicit.

### Common states

`Hugen.Common` provides MUGEN's common states with their usual numbers: stand (0),
crouch (11), walk (20), jump start (40), jump (50), land (52), hit (5000) and hit in
the air (5030), plus `attackState`, a template for "play the animation, arm the hit on
frame n, return to state m". A character reuses them and adds its moves.

## 5. Animation and collision (implemented)

An **animation** (MUGEN's AIR action) is a list of frames, each with a sprite reference,
a duration in ticks (-1 = forever), and two lists of boxes:

- **hurtboxes** (MUGEN's Clsn2): where the fighter can be hit;
- **hitboxes** (MUGEN's Clsn1): where its attack hits.

Boxes are relative to the fighter's axis (between the feet), facing right, y down, and
are mirrored when facing left. `animTime` follows MUGEN: negative while the animation
plays, 0 on the tick it ends.

**Overlap is strict**: boxes that only touch along an edge do not collide. The test is
symmetric and invariant under mirroring, so neither side of the screen nor either
player gets an advantage from rounding. (An engine whose overlap test counts a touching
edge only when the first box is on the left is an easy bug to write and a hard one to
see.)

An attack is a `HitDef` made active by the `Attack` action:

```haskell
data HitDef = HitDef
  { hdDamage :: Int, hdHitStun :: Int, hdHitPause :: Int
  , hdPushback :: Fx, hdAirLift :: Fx }
```

It connects at most once per activation (`fContact`), when any hitbox of the current
frame overlaps any hurtbox of the opponent. On impact the attacker gets `moveContact`
(for cancels) and both freeze for `hdHitPause` ticks; the defender takes damage, enters
its hit state (standing or air), is pushed away from the attacker and stays in hit stun
for `hdHitStun` ticks (`hitOver` tells when it ends).

**Proposed next:** guarding (holding back with control, with guard stun and chip
damage per attack), hit heights (high / low / overhead), attack priority and clashes,
projectiles as helper entities with their own states, throws, juggle limits.

## 6. Camera (implemented)

The camera is part of the simulation because it constrains gameplay: fighters cannot
leave the visible area (MUGEN's screen edges).

- **Horizontal:** centered on the midpoint of the fighters, clamped so the view never
  shows past the stage edges.
- **Vertical:** rises by half of the fighters' average height, so a single jump moves
  the view a little and a long air combo a lot.
- **Edges:** each fighter's axis stays at least `stageEdgeMargin` inside the view, which
  limits how far apart they can get.

Midpoints use `quot`, so the camera is exactly mirror-symmetric too.

**Proposed:** zoom, a dead zone before the camera follows, a one-tick lag (reading the
previous tick's positions) if the motion feels stiff in practice.

## 7. Defining characters (implemented in Haskell; JSON for animations planned)

Today a character is a Haskell value:

```haskell
data Character = Character
  { charName :: String
  , charConsts :: Consts                 -- life, speeds, gravity, size, special states
  , charCommands :: [Command]            -- the CMD
  , charGlobal :: [Controller]           -- state -1
  , charStates :: IntMap StateDef        -- the CNS
  , charAnims :: IntMap Anim }           -- the AIR
```

`Hugen.Example.Blocky` is the reference: the common states, three normals, an air
attack, a dash, a rush special and an uppercut, all drawn with rectangles generated by
code.

**The plan is layered** (see section 9 for what is decided):

| Layer | Format | Why |
|---|---|---|
| Logic (states, controllers, commands) | Haskell DSL | Types, functions to abstract repeated patterns (`attackState`), the compiler as validator. |
| Animations and boxes | JSON (not implemented yet) | Edited by tools (a box editor over the sprite sheet), regenerated, diffed. Nobody wants to type box coordinates in Haskell. |
| Sprites | PNG sheets plus an index | Standard tooling. |

## 8. Testing (implemented)

- **Unit tests** for each building block: fixed point, relative input, every command
  rule (sloppy input, timing, mirroring, charge, chords, buffer, hit pause), boxes,
  animation timing, `validate`.
- **Scenarios** with Blocky: walking, jumping and landing, whiffing, the jab → rush
  cancel, every attack connecting exactly once, hit pause, turning after a cross-up, KO.
- **Properties** over random inputs (QuickCheck):
  - determinism: the same inputs give the same match;
  - `step` is a fold: running a prefix and then the rest equals running everything (no
    state lives outside `Match`);
  - **no side is favored**: the mirrored match (players swapped, left and right
    swapped) is exactly the mirror image of the original;
  - invariants: life in range, fighters inside the stage and never below the floor.
- **Headless runs**: `hugen --headless` prints the scripted demo; `hugen --snapshot`
  draws a tick with SDL's software renderer (`SDL_VIDEODRIVER=dummy`), which CI uploads
  as an artifact.

**Proposed:** golden replays (recorded pad lists plus the expected final state) as a
regression suite that any behavior change must update on purpose.

## 9. Decisions and open questions

### Decided

- **Character format.** Character logic (states, controllers, commands) is written in the
  typed Haskell DSL. Animations and boxes will be loaded from JSON. Today Blocky's
  animations are still Haskell values; the JSON loader is future work.
- **MUGEN compatibility.** None, ever. Hugen is only *inspired by* MUGEN: it will not load
  `.def`/`.cns`/`.air`/`.sff` files, and there will be no importer.
- **Resolution and art style.** Logical 320x180, pixel art.
- **Command timing.** A completed command stays in the buffer for 3 ticks
  (`cmdBuffer`), and a command has a window of 15 ticks (`cmdTime`).

### Open

These need a decision from the maintainer; the code picks a default for each.

1. **Fixed-point resolution.** 1/256 px (default). Logical 320x180 makes a finer
   resolution unlikely to be needed.
2. **SOCD resolution.** Left + right = neutral (default) or last input wins?
3. **Facing and the input history.** When a fighter turns around, does input already in
   the history keep its old meaning (default: yes, it was stored relative) or is it
   reinterpreted?
4. **Hit pause and controllers.** MUGEN lets some controllers run during hit pause
   (`ignorehitpause`). Hugen freezes them all and relies on the buffer. Enough?
5. **Round flow.** Rounds, timer, intros and win poses as engine features or as states
   of a "system" character?
