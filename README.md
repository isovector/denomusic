# denomusic

[This readme was written by the clankers. The code is almost entirely human.]

A Haskell library attempting to discover the *high-level "programming language"
of music*.

## Overview

A musical score is a projection. So is a waveform. Each is a low-dimensional
shadow of a much richer object: the piece of music as the composer actually
conceives of it — a web of harmonic function, voice-leading, metric hierarchy,
motivic transformation and register, all evolving through time.

Writing music directly as notes-on-a-staff, or as samples-in-a-buffer, is
therefore working at the wrong level of abstraction. It is the musical
equivalent of writing machine code: every decision is flattened into its final
encoding, and the structure that *generated* those decisions is lost. Change
the harmony underneath a melody and you must rewrite every pitch by hand.
Change the meter and you must re-beam every bar.

denomusic is an attempt to recover that lost structure — to describe music in
terms of the things that actually vary in a composer's head, and to let the
computer perform the projection down to scores (via
[LilyPond](https://lilypond.org/)) and sound (via MIDI, through
[Euterpea](https://hackage.haskell.org/package/Euterpea)).

The library stands on two pillars:

- **Dmitri Tymoczko's geometry of pitch and hierarchy.** Following *A Geometry
  of Music* and *Tonality: An Owner's Manual*, pitches are never absolute.
  Notes are scale degrees *relative to* a scale, and scales are themselves
  defined relative to larger scales (a triad inside a diatonic scale inside the
  chromatic scale). Harmonic motion is expressed as voice-leadings — movement
  through voice-leading space — using Tymoczko's chordal (`t`) and scalar (`T`)
  transpositions.
- **Conal Elliott's Functional Reactive Programming.** Time is continuous and
  denotational. Music is built from signal functions (`SF`) — composable,
  arrow-based transformers of time-varying values and discrete events. This
  provides a temporal substrate in which independent musical layers (harmony,
  rhythm, motive, register) can be written separately and combined.

## Core concepts

### Hierarchical pitch (`Music.Types`, `Music.Harmony`)

- `Note n` — a degree in an `n`-note scale, plus accidentals. `Note 12` is
  chromatic, `Note 7` diatonic, `Note 3` triadic. The scale size lives in the
  type.
- `Scale c s` — a map from a `c`-note scale into an `s`-note scale. Scales
  compose, so a melody written in `Note 3` (chord tones) can be realized
  through `triad :: Scale 3 7` and then `diatonic :: Scale 7 12`.
  `invertScaleAbove` / `invertScaleBelow` / `invertScaleNearest` pull notes
  back up the hierarchy.
- `VoiceLeading f s` — an invertible movement of a chord. Includes
  `chordalSteps` (Tymoczko's `t`), `scaleSteps` (`T`), and `expand` /
  `contract` for spreading or collapsing pairs of voices.
- `Vertical v a` — a length-indexed vector of simultaneous voices.

### Time (`FRP`)

- `SF a b` — a signal function; an instance of `Category`, `Arrow`,
  `ArrowLoop` and `ArrowApply`, so music can be written with `proc` notation.
- Like writing music out by hand, `SF` is acausal, meaning you can look into the
  future and see what notes *will* play.

### Rhythm and meter (`Music.Beat`, `Music.TimeSig`, `Music.Types.Rhythm`)

- `Meter` — a tree describing metric hierarchy, with priorities for strong and
  weak beats. Predefined meters include `time4'4`, `time3'4`, `time6'8` and
  others, plus `subdivide` and `swing`.
- `Horizontal` — a monadic builder for sequential material (`hit`, `fill`,
  `rest`), which runs as an `SF` via `horizontal`.

### Output (`DenoMusic.Play`, `DenoMusic.Notation`)

- `play` — renders anything with a `ToMusic1` instance to MIDI. This includes
  `SF`s, `Signal`s, `Horizontal`s, `Vertical`s, `Note`s and `Span`s.
- `toPdf` / `toPdf'` — engraves the same values with LilyPond. Notes are split
  at barlines and beat groups according to the meter.

## Example

A Tintinnabuli-style harmonization (after Arvo Pärt). Each melody note is
paired with a nearby triad tone. This is a single function over the scale
hierarchy rather than a list of pitches:

```haskell
tintinnabuli :: (KnownNat s, 1 <= c) => Scale c s -> Note c -> Note s -> Note s
tintinnabuli sc c =
  applyScale sc . (+ c) . dropAccidental @s . applyScale (invertScaleAbove sc)

melody :: Horizontal r i (Note 7) ()
melody = foldMap (hit 0.25) $ fmap fromInteger [-12 .. 12]

harmonized :: Horizontal r i (Vertical 2 (Note 7)) ()
harmonized = flip mapOut melody $ \m -> m :> tintinnabuli triad 2 m :> Nil

main :: IO ()
main = do
  toPdf (horizontal harmonized)
  play  (horizontal harmonized)
```

Harmony, meter and motive can also be written as independent layers and
combined in time:

```haskell
song :: SF () (Event (Span (Note 7)))
song = proc _ -> do
  vls <- msumE <<< leading' @3 <<< harmony -< ()   -- harmonic rhythm
  let sc = applyT vls triad                        -- current chord as a scale
  bs  <- beatsOf time4'4 -< ()                     -- metric grid
  bs' <- replace (0 : cycle motive) -< bs          -- motive in chord tones
  returnA -< fmap (\(b, n) -> Span (bduration b) (sc n)) bs'
```

See `src/Ex/` for some (very early stage) examples.

## Building

The project uses [Stack](https://haskellstack.org) (GHC 9.8.4, LTS 23.26):

```sh
stack build
stack test
```

System dependencies:

- ALSA / PortMidi and a MIDI synth (for example, FluidSynth or TiMidity) for
  `play`.
- `lilypond` and a PDF viewer for `toPdf`.

With Nix, Stack's Nix integration provides these.

