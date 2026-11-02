# Contributing to Hugen

Thanks for your interest! Hugen is a work in progress, so bug reports, questions and ideas
are as welcome as code.

## Issues

GitHub issues are welcome: bugs, things that feel wrong when you play the training room,
questions about the design, ideas for the roadmap. If you want to work on something, open
an issue first so we can agree on the approach.

## Running and testing

You need [Stack](https://docs.haskellstack.org/) and the SDL2 development libraries
(see the [README](README.md#running-it)).

```sh
stack build
stack test                                   # unit tests and QuickCheck properties
stack exec hugen                             # training room (needs a display)
stack exec hugen -- --headless               # scripted demo in the terminal
SDL_VIDEODRIVER=dummy stack exec hugen -- --snapshot demo.bmp 44   # no display needed
```

CI runs the same build, tests, headless demo and snapshot on every push and pull request.

## Style and rules

- Read [docs/design.md](docs/design.md) first: it explains the architecture and the
  order of a tick.
- `hugen-core` (`src/core`) stays pure: it depends only on `base` and `containers`, uses
  no floats in gameplay and never touches SDL, the clock or `IO`. The build enforces it.
- Every gameplay rule comes with a test, and the properties (determinism, mirror
  symmetry) must keep passing.
- No new warnings with the GHC flags in `package.yaml` (`-Wall` and friends).
- Code, comments and docs in English.
- No third-party art, sounds or character data in the repository unless their license
  allows it and the source is credited.

## License

By contributing you agree that your contributions are licensed under the
[BSD-3-Clause license](LICENSE).
