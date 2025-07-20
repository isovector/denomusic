{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-x-partial #-}

module Etude17 where

import Control.Arrow
import Data.Ratio
import Euterpea (Music (..), Pitch, PitchClass (..), note, rest)
import Euterpea qualified as E
import Euterpea.IO.MIDI.Play
import Legacy hiding (main)
import Rhythm

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

bar1to4 :: Rhythm Pitch
bar1to4 = do
  c <- im [E]
  oct <- im [0, 1]
  rtimes 2 $ do
    f <- im [minor, maj]
    hand <- im [2, 3]
    chord $ f c (oct + hand)

bar5 :: Rhythm Pitch
bar5 = do
  rtimes 4 $ do
    c <- im [F]
    oct <- im [0]
    f <- im [minor]
    ch <- chord $ f c (oct + 2)
    im [ch, E.trans 12 ch]

section10gen
  :: Rhythm [Pitch]
  -> Rhythm [Pitch]
  -> Rhythm Pitch
section10gen top bot =
  (chord =<<) $
    Par
      (overlay (const) top (im $ replicate 6 ()))
      ( do
          n <- overlay (const id) (im $ replicate 4 ()) bot
          im [take 2 n, drop 2 n]
      )

invert :: [Pitch] -> [Pitch]
invert [] = []
invert ((pc, o) : xs) = xs <> pure (pc, o + 1)

noFifth :: [Pitch] -> [Pitch]
noFifth (x : y : _ : z) = x : y : z
noFifth xs = xs

data Variant = Variant1 | Variant2 | Variant3

song' :: Rhythm Pitch
song' = tuplet $ do
  (ch, v) <-
    [ (minor F, Variant1)
      , (maj Df, Variant2)
      , (maj Af, Variant1)
      , (maj C, Variant3)
      , (minor F, Variant1)
      , (maj Df, Variant2)
      , (maj Ef, Variant1)
      , (maj C, Variant3)
      , (minor F, Variant1)
      , (maj Df, Variant2)
      , (maj Ef, Variant1)
      , (maj C, Variant3)
      ]
  pure $ case v of
    Variant1 -> section10gen (pure $ ch 4) (pure $ invert $ ch 2)
    Variant2 ->
      section10gen
        ( tuplet
            [ pure $ ch 4
            , im
                [ invert $ ch 4
                , invert $ invert $ ch 4
                , ch 4
                ]
            ]
        )
        (pure $ ch 3)
    Variant3 ->
      section10gen
        ( im
            [ invert $ ch 4
            , dim (fst $ head $ drop 1 $ ch 4) 4
            ]
        )
        (pure $ ch 2)

song :: Rhythm Pitch
song = do
  let
    b1 = section10gen (pure $ minor F 4) (pure $ minor F 3)
  tuplet
    [ -- section 10
      b1
    , section10gen
        ( tuplet
            [ pure $ maj Cs 4
            , im
                [ noFifth $ min7 F 4
                , noFifth $ min7 F 4
                , invert $ maj Cs 4
                ]
            ]
        )
        ( tuplet
            [ pure $ invert $ maj Cs 3
            , im
                [ noFifth $ min7 F 3
                , invert $ maj Cs 3
                ]
            ]
        )
    , section10gen
        (pure $ invert $ invert $ maj Af 3)
        (pure $ invert $ invert $ maj Af 2)
    , section10gen
        ( im
            [ invert $ maj C 4
            , dim E 4
            ]
        )
        (pure $ invert $ maj C 3)
    , -- section 11
      b1
    , section10gen
        ( tuplet
            [ pure $ maj Cs 4
            , im
                [ noFifth $ min7 F 4
                , noFifth $ min7 F 4
                , invert $ maj Cs 4
                ]
            ]
        )
        (pure $ maj Df 3)
    , section10gen
        (pure $ invert $ maj Ef 4)
        (pure $ invert $ maj Ef 2)
    , section10gen
        ( im
            [ invert $ invert $ maj C 4
            , invert $ dim E 4
            ]
        )
        (pure $ invert $ invert $ maj C 2)
    , -- section 12
      section10gen
        (pure $ invert $ minor F 4)
        (pure $ invert $ minor F 2)
    , section10gen
        ( tuplet
            [ pure $ invert $ invert $ maj Df 4
            -- NOTE second bar of section 12 has a nonstandard pattern here
            -- which we can't express via section10gen
            ]
        )
        (pure $ invert $ invert $ maj Df 2)
    , section10gen
        (pure $ invert $ maj Ef 4)
        (pure $ invert $ maj Ef 2)
    , section10gen
        ( im
            [ invert $ invert $ maj C 4
            , invert $ dim E 4
            ]
        )
        (pure $ invert $ maj C 2)
    , -- section 13 line 1
      section10gen
        (pure $ minor F 4)
        (pure $ power F 2)
    , section10gen
        (pure [(E, 4), (Af, 4), (C, 5)])
        (pure [(E, 2), (C, 3), (E, 3)])
    , section10gen
        (pure $ invert $ maj Df 4)
        (pure $ power Df 2)
    , section10gen
        ( im
            [ [(E, 4), (G, 4), (Ef, 5)]
            , [(E, 4), (G, 4), (Df, 5)]
            ]
        )
        (pure $ power C 2)
    , -- section 13 line 2
      section10gen
        (pure $ minor F 4)
        (pure $ power F 2)
    , section10gen
        (pure [(E, 4), (Af, 4), (C, 5)])
        (pure [(E, 2), (C, 3), (E, 3)])
    , -- first go around
      section10gen
        (pure $ invert $ maj Ef 4)
        (pure $ invert $ invert $ maj Ef 2)
    , section10gen
        ( im
            [ invert $ invert $ maj C 4
            , invert $ dim E 4
            ]
        )
        (pure $ invert $ maj C 2)
    ]
