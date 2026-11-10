# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [Unreleased]

### Added

- Pure simulation core (`hugen-core`), an internal library that depends only on
  `base` and `containers`: fixed-point math, facing-relative input, motion and charge
  commands with an input buffer, states and triggers as a typed embedded language,
  animations with hitboxes and hurtboxes, hit pause, camera and common states.
- Training room with Blocky, a placeholder character drawn with rectangles.
- Headless demo (`--headless`) and software-rendered snapshots (`--snapshot`), both
  usable without a display.
- Test suite (hspec + QuickCheck): unit tests for every rule, scenarios with Blocky and
  properties for determinism, `step` as a fold, mirror symmetry and invariants.
- Design document (`docs/design.md`).
- CI on GitHub Actions: build, tests, headless demo and a snapshot as an artifact.
- Window settings (title and size) read from `resources/config.ini`.
- `CONTRIBUTING.md`.
- Guarding: hold away from the attacker, standing or crouching, with guard stun and chip
  damage per attack.
- Hit levels (high, mid, low, overhead) that decide which guard stops an attack.
- Reactions by strength (light, heavy, knockdown); falling, lying and getting up, with
  no hits on a fighter that is down.
- Blocky's sweep (crouching B), a low attack that knocks down.

### Changed

- Build on Stackage LTS 24 (GHC 9.10); imports fixed for mtl 2.3.
- The SDL code moved to `src/shell` and is driven by a fixed-rate loop (60 ticks per
  second) over the core.
- README rewritten in English with status, usage and roadmap; license holder filled in.
- Copyright years updated to 2024–2026.

### Removed

- The early sprite demo (`Game`, `Scene`) and its third-party sprite.
- `resources/bg.png`, an unused background image of unknown origin.
- Both images were also removed from the whole git history (2026-11-04); the repository
  contains no third-party art.

[Unreleased]: https://github.com/sawady/hugen/commits/main
