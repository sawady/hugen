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
   animation positions, life, the input history, the command buffer and the entities.
   `Match` has `Eq` and `Show`.
5. **Multiplication and division round toward zero**, so that negation commutes with
   them and the engine is exactly mirror-symmetric (a property test checks it).
6. **No unordered collections in gameplay.** Entities are a list in creation order,
   not a `Map`; every phase of the tick visits them in that order.

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
   current state (section 4); each entity (section 5) runs its state. All of them read
   the *opponent as it was before this phase*: player 1 is not processed "first".
3. **Physics.** Velocity is applied; then friction (standing, crouching) or gravity
   (airborne). An airborne fighter that reaches the floor while falling lands and
   enters its land state.
4. **Space.** Bodies push each other apart (each one half the overlap, rounded up so
   the push is symmetric); fighters are clamped to the stage and to the camera view;
   against a wall, only the other fighter moves. Entities are not pushed or clamped, and
   a thrower and the fighter it holds do not push each other.
5. **Hits.** Throws first (section 5): a throw that connects, or a hold in progress,
   keeps every attack off both fighters this tick. Then every active attack, of a
   fighter or of an entity, is tested against the opposing fighter's hurtboxes, all on
   the same snapshot, so attacks can **trade**. The defender's guard is checked here,
   against this tick's pad. Opposing projectiles that touch cancel out, on the same
   snapshot.
6. **Clocks, then impact.** State time, animation and hit stun advance (or a tick of hit
   pause is spent). Then the hits found in step 5 are applied: damage and the defender's
   hit state, or chip damage and its guard state; pushback; and a hit pause that starts
   on the *next* tick. Then throws: one that connected puts the opponent in its held
   state; a hold in progress is escaped, lands or goes on; and the held fighter is put
   where the thrower holds it.
7. **Facing.** An idle fighter on the floor turns to face the opponent. A fighter in
   an attack does not: a move that crosses the opponent keeps its direction.
8. **Camera, entities and round end.** Entities that destroyed themselves, and
   projectiles that connected, cancelled out or left the view, are removed. The entities
   created in step 2 are added at the end of the list, to act from the next tick.

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
  , sdLandState :: Maybe StateId   -- instead of the character's land state
  , sdControllers :: [Controller] }

data Controller = Controller { ctrlTrigger :: Expr Bool, ctrlAction :: Action }

data Action
  = ChangeState StateId | ChangeAnim AnimId | SetCtrl Bool
  | VelSet (Maybe (Expr Fx)) (Maybe (Expr Fx)) | VelAdd (Expr Fx) (Expr Fx)
  | PosAdd (Expr Fx) (Expr Fx) | SetVar Int (Expr Int)
  | Attack HitDef | Turn
  | Spawn EntityDef | DestroySelf   -- section 5, helpers and projectiles
  | Throw ThrowDef                  -- section 5, throws
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
  NumProjectiles :: Expr Int       -- own side's projectiles (section 5)
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
- **It cannot do IO or read anything outside the fighter and what it sees of the
  match** (a `Sight`: the opponent and how many projectiles its side has, as they were
  before the controllers ran). The evaluator (`Hugen.Fighter.eval`) is the only
  interpreter, and it is pure.

The cost is that every new query needs a constructor and an evaluator case. That is
also a feature: the set of things a character can observe is explicit.

### Common states

`Hugen.Common` provides MUGEN's common states with their usual numbers: stand (0),
crouch (11), walk (20), jump start (40), jump (50), land (52), standing and crouching
guard (150, 152), hit (5000), heavy hit (5001), hit in the air (5030), fall (5050), lying
(5110) and getting up (5120), and the state of a fighter held by a throw (5300, a Hugen
number: MUGEN has no common state for it), plus `attackState`, a template for "play the animation, arm
the hit on frame n, return to state m". A character reuses them and adds its moves.

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
  , hdPushback :: Fx, hdAirLift :: Fx
  , hdLevel :: HitLevel, hdStrength :: HitStrength
  , hdChip :: Int, hdGuardStun :: Int }
```

It connects at most once per activation (`fContact`), when any hitbox of the current
frame overlaps any hurtbox of the opponent. On impact the attacker gets `moveContact`
(for cancels, on a hit or a guard alike) and both freeze for `hdHitPause` ticks; the
defender is pushed away from the attacker and takes the hit or guards it.

### Guarding

A fighter guards by holding away from the attacker on the tick of the impact, on the
floor, either idle with control or already in guard stun. Holding down as well makes it
a crouching guard. The direction is read from the absolute pad against the attacker's
position, not from the fighter's facing, so a cross-up has to be guarded the other way
even before the fighter has turned. There is no guard in the air.

A guarded attack costs `hdChip` life (which can end the round) and puts the defender in
its guard state for `hdGuardStun` ticks. Being in guard stun does not stop it from
guarding the next attack, or from changing height to do it. `hdStrength` has no effect
through a guard: nobody is knocked down while guarding.

Levels decide which guard works:

| `hdLevel` | Standing guard | Crouching guard | Against a crouching fighter |
|---|---|---|---|
| `HitHigh` | yes | yes | misses, whatever the boxes say |
| `HitMid` | yes | yes | hits |
| `HitLow` | no | yes | hits |
| `HitOverhead` | yes | no | hits |

### Reactions and knockdown

`hdStrength` picks the state an unguarded attack sends the defender to:

| `hdStrength` | On the floor | In the air |
|---|---|---|
| `HitLight` | hit (`hdHitStun` ticks) | hit in the air |
| `HitHeavy` | heavy hit (`hdHitStun` ticks) | hit in the air |
| `HitKnockdown` | fall | fall |

A knockdown launches the defender with `hdAirLift`. The fall state lands in the lying state
(`sdLandState`), which leads to getting up and then to standing. Both of those have the
stance `Lying`, and the engine never lets an attack reach a fighter in that stance.

### Juggle limit

A fighter counts the hits it takes in the air: a hit on an airborne fighter, or a knockdown
from the floor, adds one. Once the count reaches `cJuggleLimit` (3 by default, per
character), attacks go through a fighter that is airborne and being hit, without damage,
contact or hit pause. The count goes back to 0 on entering any state that is not an
airborne reaction, which in practice means touching the floor.

### Helpers and projectiles

Besides the two fighters, a `Match` holds a list of **entities** (MUGEN's helpers and
projectiles):

```haskell
data Entity = Entity { eId :: Int, eOwner :: Side, eKind :: EntityKind, eBody :: Fighter }
data EntityKind = Helper | Projectile
data EntityDef = EntityDef { edKind :: EntityKind, edState :: StateId, edOffset :: (Fx, Fx) }

on (animElemStarts 1) (spawnProjectile stFireball 30 (-46))   -- Blocky's throw
```

- **An entity is a fighter without input.** Its body is a `Fighter` running states of
  its owner's character: the same `StateDef`s, controllers, animations and boxes, so a
  fireball is written like any other state. It runs only its state's controllers, never
  the global ones (which turn commands into moves), and is not pushed by bodies or held
  inside the view.
- **Creation.** The `Spawn` action creates one at an offset from the spawner's axis
  (x forward, so it mirrors with the facing), facing the same way. It is added at the
  end of the tick and acts from the next one. A side can have at most
  `maxEntitiesPerSide` (32) entities; more spawns do nothing.
- **Order.** Entities live in a list ordered by `eId`, a counter in the match that is
  never reused. Every phase visits them in that order; when both sides create entities
  on the same tick, player 1's are created first. No rule depends on ids across sides,
  which is what keeps the mirror property exact (the mirrored match numbers the two
  sides' entities differently, and the test compares each side's list without ids).
- **Attacks.** An entity's active attack is tested against the owner's opponent, like a
  fighter's, with every rule of this section: guard (read against the entity's
  position, so a projectile from behind is guarded the other way), levels, reactions,
  juggle limit. It never hits its owner's side, and fighters' attacks do not hit
  entities. The defender freezes for the hit pause; the owner does not. A helper that
  connects freezes itself; entities are not frozen by the fighters' hit pause.
- **Projectiles** are helpers that are also removed when their attack connects (hit or
  guard), when their hitboxes touch those of a projectile of the other side (both are
  removed, there is no priority), and when their axis leaves the view by more than
  `stageOffscreenMargin`. A **helper** stays until it runs `DestroySelf`.
- **Several hits on one tick.** A fighter can now be reached by several attacks on the
  same tick: its opponent's first, then the opponent's entities in id order. All are
  decided on the snapshot, except the juggle count, which accumulates: once the earlier
  hits fill it, the later ones go through without contact (a projectile that goes
  through is not spent).

- **Counting.** The trigger `numProjectiles` (MUGEN's `NumProj`) is the number of
  projectiles of the fighter's side in the match at the start of the tick; for an entity,
  those of its owner. Helpers do not count. A projectile created this tick is counted
  from the next one, and one removed this tick until the end of it.

Blocky's fireball (↓ ↙ ← + A) is the example: the throw creates the projectile on its
second frame, and the projectile's state sets its speed and arms a mid attack with chip
damage. The command only starts a throw while `numProjectiles .== 0`, so Blocky has one
fireball on screen at a time.

### Throws

A throw is armed by the `Throw` action (MUGEN uses a `HitDef` with `p1stateno`,
`p2stateno` and `TargetBind` for this):

```haskell
data ThrowDef = ThrowDef
  { tdDamage :: Int
  , tdState :: StateId     -- the thrower's state once it connects
  , tdHold :: Int          -- ticks the opponent is held before the throw lands
  , tdEscape :: Int        -- ticks at the start of the hold in which it can escape
  , tdOffset :: (Fx, Fx)   -- where the opponent is held, from the thrower's axis
  , tdLaunch :: (Fx, Fx) } -- velocity it is thrown with, x forward from the thrower

on (animElemStarts 1) (throw blockyThrow)   -- Blocky's throw, C at close range
```

- **What it catches.** Like an attack, a throw connects at most once, when a hitbox of
  the current frame overlaps a hurtbox of the opponent. It cannot be guarded, and it
  only catches an opponent on its feet (standing or crouching) that is not being hit:
  not one in the air, lying or getting up, in hit stun or guard stun, or already held.
  An opponent in the middle of an attack can be thrown.
- **The hold.** The thrower goes to `tdState`. The opponent goes to its own held state
  (`cHeldState`, common state 5300) and is held: it runs no controllers and no physics,
  and at the end of every tick the engine puts it at `tdOffset` from the thrower (x
  forward, so it follows the thrower's facing, also when the thrower turns), never under
  the floor and inside the view. The hold depends only on its own clock: the thrower's
  state is free to animate, move or turn, but it does not decide when the throw lands.
  MUGEN runs the thrown character in the thrower's states ("custom states"); Hugen keeps
  it in a state of its own character, so that both characters stay self-contained.
- **Nothing hits them.** On the tick a throw connects and while the hold lasts, no
  attack reaches either fighter: not the thrower's, not one the held fighter armed
  before, not an entity's. They go through without contact, and a projectile is not
  spent. So a throw beats an attack that would connect on the same tick, and two throws
  that connect on the same tick both miss.
- **Escaping.** The held fighter escapes if its escape command (`cThrowEscape`, a
  command of its own character) is active on any of the first `tdEscape` ticks of the
  hold. The command buffer counts, so an escape completed on the tick the throw connects
  works too. Both fighters then enter their hit state (the air one if above the floor)
  for `throwEscapeStun` (12) ticks, pushed apart at `throwEscapePush` (4) along the
  thrower's facing, unhurt, and get control back on the same tick.
- **Landing.** Otherwise, after `tdHold` ticks the throw lands: `tdDamage`, which can end
  the round, and a knockdown. The opponent enters its fall state with `tdLaunch` (x along
  the way the thrower faces then), lies and gets up as after any knockdown. It counts as
  one hit for the juggle limit, like a knockdown from the floor.
- **Only fighters throw.** An entity can run `Throw`, but its throw never connects.

Blocky's throw is C at close range: active on the fourth tick for three ticks, 16 ticks
of recovery when it whiffs. It holds the opponent for 20 ticks, body against body, with
an escape window of 10 (pressing C again), and deals 120.

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

`Hugen.Example.Blocky` is the reference: the common states, four normals, an air
attack, a dash, a rush special, an uppercut, a fireball and a throw, all drawn with rectangles
generated by code.

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
  cancel, every attack connecting exactly once, hit pause, turning after a cross-up, KO;
  and one per combat rule: the guard table, guard stun to the tick, chip damage, cross-up
  guards, reactions, the knockdown sequence, no hits on a fighter that is down, the
  juggle limit; and one per entity rule: where a projectile appears and how it flies,
  hitting and guarding from a distance, cancelling out, leaving the view, ids, the cap,
  helpers that hit and destroy themselves, several hits on one tick; and one per throw
  rule: what a throw catches and what it does not, the hold to the tick, landing,
  the escape window to the tick, nothing hitting during a hold, a throw against an
  attack or another throw on the same tick, turning while holding, walls.
- **Properties** over random inputs (QuickCheck):
  - determinism: the same inputs give the same match;
  - `step` is a fold: running a prefix and then the rest equals running everything (no
    state lives outside `Match`);
  - **no side is favored**: the mirrored match (players swapped, left and right
    swapped) is exactly the mirror image of the original;
  - invariants: life in range, fighters inside the stage and never below the floor;
  - the same properties over fights at close range, with coverage checks that make sure
    those fights reach guards and knockdowns, and over fights full of fireballs, with
    coverage of each way a projectile ends (hit, guard, cancel, leaving the view), and
    over fights at throw range, with coverage of throws that land and throws that are
    escaped, also with random throws;
  - entity invariants: creation order, ids under the counter, the cap, projectiles near
    the view;
  - throw invariants: at most one fighter held, held where the thrower holds it, unhurt
    until the throw lands, and every hold ends by itself.
- **Mutation checks.** Each rule was broken on purpose once (a projectile that is not
  spent, entities that hit their own side, reused ids, offsets that ignore facing, a
  throw that can be guarded, an escape window one tick off...) to confirm that some
  test fails.
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
- **Clashes.** None. Two attacks that connect on the same tick both land (a trade).
- **High attacks.** `HitHigh` misses a crouching fighter even if the boxes overlap.
- **Guard details.** Chip damage can end the round, a guarded attack uses the same hit
  pause and pushback as a hit, and there is no guarding in the air.

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
6. **Juggle model.** Resolved: every air hit costs one and the limit is 3 per character.
   Attacks have no individual cost.
7. **Entities.** Resolved (2026-11-11), as described in section 5: fighters cannot hit
   entities; projectiles cancel out one for one with no priority or durability; a
   projectile that cancels out on the tick it reaches the opponent still hits (same
   snapshot); entities keep moving while the fighters are in hit pause; "leaving the
   view" is only horizontal; at most 32 entities per side; `numProjectiles` counts one's
   own projectiles, and Blocky uses it to throw one at a time.
8. **Throws.** Defaults taken for the first version (section 5): a throw cannot be
   guarded and only catches an opponent on its feet that is not being hit; it beats an
   attack on the same tick, and two throws on the same tick both miss; nothing hits
   either fighter during the hold; the escape is the held fighter's own command during
   the first `tdEscape` ticks, buffer included, and leaves both fighters in their hit
   state for 12 ticks, unhurt; the hold lasts a fixed time, whatever the thrower's state
   does; a landed throw is a knockdown and counts for the juggle; only fighters throw,
   and there are no air throws (a throw cannot catch an airborne opponent).
