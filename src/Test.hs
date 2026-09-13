{-# LANGUAGE OverloadedLists #-}

module Test where

import Data.Set qualified as S
import Data.Set (Set)
import Data.Functor
import Data.Coerce
import DenoMusic.Harmony
import DenoMusic.Modes
import DenoMusic.Notation
import DenoMusic.Types
import DenoMusic.Play qualified as Play
import FRP

type Music = SF (Reg PitchClass)

regular :: Time -> SF m x (Event Time)
regular t = every t t

type Chord = (MetaScale 4, T [4, 7, 12])

chords :: SF m x (Event Chord)
chords =
  discrete
    [ (0,   (add11 triad,  [0,  0, 0]))
    , (1,   (coerce triad, [0, -1, 0]))
    , (1.5, (add11 triad,  [0, -1, 0]))
    , (2,   (add11 triad,  [0 , 0, 0]))
    , (3,   (coerce triad, [0, -1, 0]))
    , (3.5, (add11 triad,  [0, -1, 0]))
    , (4,   (coerce triad, [0, -2, 0]))
    , (4.5, (add7 triad,   [0, -2, 0]))
    , (6,   (coerce triad,   [-1, 1, 0]))
    , (7,   (add7 triad,   [-1, 2, 0]))
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

chordTone :: SF (Reg PitchClass) (Chord, Event (Time, T [4, 7, 12])) (Event ())
chordTone = proc ((ms, t), e) ->
  note -< e <&> fmap (\t0 -> kill ms (t <> t0))


motif1 :: Music (Chord, Event x) ()
motif1 = proc (ms, e0) -> do
  e1 <- offset (1/16) -< e0
  e2 <- chordTone -< (ms, (1/16, [0, 0, 0]) <$ e1)
  e3 <- chordTone -< (ms, (1/16, [-1, 0, 0]) <$ e2)
  e4 <- chordTone -< (ms, (1/16, [-2, 0, 0]) <$ e3)
  e5 <- rest -< (1/16) <$ e4
  e7 <- chordTone -< (ms, (1/16, [-2, 0, 0]) <$ e5)
  e8 <- chordTone -< (ms, (1/16, [-1, 0, 0]) <$ e7)
  chordTone -< (ms, (1/16, [0, 0, 0]) <$ e8)
  returnA -< ()


song :: SF (Reg PitchClass) () ()
song = proc _ -> do
  ch <- hold (add7 triad, mempty) <<< chords -< ()

  b <- regular 1 -< ()
  q <- regular (1/4) -< ()
  sq <- offset (- 1/4) <<< onlyEvery 2 -< q
  wq <- offset (1/4) <<< regular (1/2) -< q

  motif1 -< (ch, sq)

  x <- chordTone -< (ch, (1/4, [-2, 0, -12]) <$ sq)
  chordTone -< (ch, (1/4, [-2, 4, -12]) <$ x)

  returnA -< ()


main :: IO ()
main = do
  let ns = export (0, 8) song
  toPdf $ makeScore $ pure $ fmap (\(i, s) -> (i, Right (mempty, S.findMin s))) ns
  Play.play ns

