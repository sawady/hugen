# Changelog for `hugen`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

- Pure simulation core (`hugen-core`): fixed-point math, facing-relative input, motion
  and charge commands with an input buffer, states and triggers as a typed embedded
  language, animations with hitboxes and hurtboxes, hit pause, camera.
- Training room with Blocky, a placeholder character drawn with rectangles.
- Headless demo (`--headless`) and software-rendered snapshots (`--snapshot`).
- Test suite (hspec + QuickCheck): determinism, mirror symmetry, invariants.
- Build on Stackage LTS 24 (GHC 9.10); CI on GitHub Actions.
- Removed the old sprite demo and its third-party sprite.

