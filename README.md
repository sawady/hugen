# Hugen

This is a game engine called `Hugen`.

It's implemented in Haskell, and it's inspired on the classic fighting game engine called [MUGEN](https://es.wikipedia.org/wiki/MUGEN).

The game engine works with ticks at a constante rate, so you can implement behaviour and animations around time in that unit.

Also, modeling states and triggers like in MUGEN involves a state system for characters (and other actors) and their behavior during gameplay. Each state represents a specific action or behavior (e.g., idle, attack, jump), and triggers dictate transitions between these states based on conditions like inputs, time, or collisions. This is implemented as a form of domain specific language in Haskell.

This is a W.I.P., so it's not stable right now.