{-# OPTIONS_GHC -fno-warn-orphans #-}

module Example where

import Data.Ratio
import Debug.Trace
import Euterpea
import Euterpea.IO.MIDI.Play
import IntervalMap
import Euterpea (PitchClass (..), Octave, Pitch(..))
import Euterpea qualified as E
import Legacy hiding (main)
import Data.Semigroup


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
    bind (\d a -> chord $ fmap (note d) a) $
    foldMusic $
      renormalize (1 % 4) $
        foldInterval song

im :: [a] -> IntervalMap a
im = Interval . fmap pure



song :: IntervalMap [Pitch]
song =
  mconcat
    -- [ do
    --     c <- im [E]
    --     oct <- im [0, 1]
    --     stimes 2 $ do
    --       f <- im [minor, maj]
    --       hand <- im [2, 3]
    --       pure $ f c (oct + hand)
    [ do
      c <- im [F]
      oct <- im [0]
      f <- im [minor]
      hand <- im [2, 3]
      pure $ f c (oct + hand)
    ]

    -- x <-
    --   liftA2 (,)
    --     (Interval [pure G, pure A, pure B])
    --     (Interval [pure 4, pure 5])
    -- Interval [pure x, Empty]

