{-# OPTIONS_GHC -fno-warn-orphans #-}

module Example where

import Data.Ratio
import Debug.Trace
import Euterpea
import Euterpea.IO.MIDI.Play
import IntervalMap
import Euterpea (PitchClass (..), Octave, Pitch(..))
import Euterpea qualified as E
import Legacy (maj7, min7, dom7)


renormalize :: Rational -> [(Rational, Durated a)] -> [(Rational, Durated a)]
renormalize r ds =
  let shortest = minimum $ fmap (getDuration . snd) ds
      mult = r / shortest
   in fmap ((* mult) *** mapDuration (* mult)) ds


foldMusic :: [(Rational, Durated a)] -> Music a
foldMusic =
  flip foldr (rest 0) $
    uncurry $ \offset (Durated d a) m ->
      (rest offset :+: note d a) :=: m


main :: IO ()
main =
  playDev @Pitch 2 $
    foldMusic song

song :: [(Rational, Durated Pitch)]
song = renormalize (1 % 4) $
  foldInterval $ do
    x <-
      liftA2 (,)
        (Interval [pure G, pure A, pure B])
        (Interval [pure 4, pure 5])
    Interval [pure x, Empty]

