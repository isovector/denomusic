# denomusic

It's a Haskell DSL for composing music. Inspired by
[diagrams](https://hackage.haskell.org/package/diagrams), combining [Tiled
Polymorphic Temporal Media](https://dl.acm.org/doi/10.1145/2633638.2633649)
with [Dmitri Tymoczko's work on the geometry of
harmony.](https://dmitri.mycpanel.princeton.edu/whatmakesmusicsoundgood.html)

The library is capable of playing music via MIDI, and of engraving scores via
[lilypond](https://lilypond.org/).


## The Core Idea

Stop thinking about pitches. Instead refine counterpoint by moving around in
voice-leading space.

Also, stop thinking about music notation. Compose music hierarchically, and
link it together monoidally.
