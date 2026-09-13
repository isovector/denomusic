{-# LANGUAGE OverloadedLists #-}

module Test where

import Data.Functor
import Data.Coerce
import Debug.Trace
import DenoMusic.Harmony
import DenoMusic.Modes
import DenoMusic.Notation
import DenoMusic.Types
import DenoMusic.Play qualified as Play
import FRP

type Music = SF (Reg PitchClass)

regular :: Time -> SF m x (Event Time)
regular t = every t t



chords :: SF m x (Event (MetaScale 4))
chords =
  discrete
    [ (0, add11 triad)
    , (1, coerce sus2)
    , (1.5, coerce triad)
    ]


note :: SF (Reg PitchClass) (Event (Time, T [7, 12])) (Event ())
note
  = arr
      (fmap $ fmap $ \t ->
        elim
          (MSCons harmonicMinor spelledSharp)
          (Reg 4 Fs)
          $ t <> mixolydian harmonicMinor)
  >>> play

chordTone :: SF (Reg PitchClass) (MetaScale n, Event (Time, T [n, 7, 12])) (Event ())
chordTone = proc (ms, e) ->
  note -< e <&> fmap (kill ms)


motif1 :: Music (MetaScale n, Event x) ()
motif1 = proc (ms, e0) -> do
  e1 <- offset (1/16) -< e0
  e2 <- chordTone -< (ms, (1/16, [0, 0, 0]) <$ e1)
  e3 <- chordTone -< (ms, (1/16, [-1, 0, 0]) <$ e2)
  chordTone -< (ms, (1/16, [-2, 0, 0]) <$ e3)
  returnA -< ()

motif2 :: Music (MetaScale n, Event x) ()
motif2 = proc (ms, e0) -> do
  e1 <- offset (1/16) -< e0
  e2 <- chordTone -< (ms, (1/16, [-2, 0, 0]) <$ e1)
  e3 <- chordTone -< (ms, (1/16, [-1, 0, 0]) <$ e2)
  chordTone -< (ms, (1/16, [0, 0, 0]) <$ e3)
  returnA -< ()


song :: SF (Reg PitchClass) () ()
song = proc _ -> do
  ch <- hold (error "ch") <<< chords -< ()

  b <- regular 1 -< ()
  q <- regular (1/4) -< ()
  sq <- offset (- 1/4) <<< onlyEvery 2 -< q
  wq <- offset (1/4) <<< regular (1/2) -< q

  motif1 -< (ch, sq)
  motif2 -< (ch, wq)

  x <- offset (-(1/4)) <<< chordTone -< (ch, (1/4, [-2, 0, -12]) <$ b)
  chordTone -< (ch, (1/4, [-2, 4, -12]) <$ x)

  returnA -< ()


main :: IO ()
main = Play.play $ traceShowId $ export (0, 2) song

