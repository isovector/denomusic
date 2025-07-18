{-# LANGUAGE LambdaCase           #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Example where

import Control.Arrow
import Data.Ratio
import Debug.Trace
import Euterpea.IO.MIDI.Play
import IntervalMap
import Euterpea (PitchClass (..), Octave, Pitch(..), Music (..), note, rest)
import Euterpea qualified as E
import Legacy hiding (main)
import Data.Semigroup


renormalize :: Rational -> [(Rational, Durated a)] -> [(Rational, Durated a)]
renormalize r ds =
  let shortest = minimum $ fmap (getDuration . snd) ds
      mult = r / shortest
   in fmap ((* mult) *** mapDuration (* mult)) ds


foldMusic :: [(Rational, Durated a)] -> E.Music a
foldMusic =
  flip foldr (rest 0) $
    uncurry $ \offset (Durated d a) m ->
      (rest offset :+: note d a) :=: m


main :: IO ()
main =
  playDev @Pitch 2 $
    foldMusic $
      renormalize (1 % 8) $
        foldInterval song

im :: [a] -> IntervalMap a
im = Interval . fmap pure


chord :: [a] -> IntervalMap a
chord [] = mempty
chord xs = foldr1 Par $ fmap pure xs

bar1to4 = do
  c <- im [E]
  oct <- im [0, 1]
  stimes 2 $ do
    f <- im [minor, maj]
    hand <- im [2, 3]
    chord $ f c (oct + hand)

bar5 = do
  stimes 4 $ do
    c <- im [F]
    oct <- im [0]
    f <- im [minor]
    ch <- chord $ f c (oct + 2)
    im [ch, E.trans 12 ch]

section10gen
  :: IntervalMap [Pitch]
  -> IntervalMap [Pitch]
  -> IntervalMap Pitch
section10gen top bot =
  (chord =<<) $
    Par
      (overlay (const id) (im $ replicate 6 ()) top)
      ( do
          n <- overlay (const id) (im $ replicate 4 ()) bot
          im [take 2 n, drop 2 n]
      )

invert :: [Pitch] -> [Pitch]
invert [] = []
invert ((pc, o) : xs) = xs <> pure (pc, o + 1)

song :: IntervalMap Pitch
song =
  let
    b1 = section10gen (pure $ minor F 4) (pure $ minor F 3)
    b2 = section10gen
          (Interval
            [ pure $ maj Cs 4
            , im
              [ [(F, 4), (Af, 4), (Ef, 5)]
              , [(F, 4), (Af, 4), (Ef, 5)]
              , invert $ maj Cs 4
              ]
            ])
          (Interval
            [ pure $ invert $ maj Cs 3
            , Interval
                [ pure [(F, 3), (Af, 3), (Ef, 4)]
                , pure $ invert $ maj Cs 3

                ]
            ])
  in Interval
    [ b1
    , b2
    , section10gen
        (pure $ invert $ invert $ maj Af 3)
        (pure $ invert $ invert $ maj Af 2)
    , section10gen
        (im
          [ invert $ maj C 4
          , [(C, 4), (E, 4), (Bf, 4)]
          ]
          )
        (pure $ invert $ maj C 3)
    , b1
    , b2
    , section10gen
        (pure $ invert $ maj Ef 4)
        (pure $ invert $ maj Ef 2)
    , section10gen
        (im
          [ invert $ invert $ maj C 4
          , [(G, 4), (Bf, 4), (E, 5)]
          ]
          )
        (pure $ invert $ invert $ maj C 2)
    ]

