{-# LANGUAGE LambdaCase                           #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fno-warn-type-defaults           #-}

module Glass.Etude16 where

import Data.Semigroup
import Data.Ratio
import Euterpea (Pitch, PitchClass(..))
import Score
import Theory.Chords


bassRhythm :: Score a -> Score a -> Score a
bassRhythm a b = mconcat
  [ a, b, b
  , a, b, a, b
  ]

bassTemplate :: Pitch -> Pitch -> Pitch -> Score Pitch
bassTemplate a b c =
  scale (1 % 7) $
    bassRhythm
      (chord [a, b])
      (tile 1 c)

bassTwiddle :: Score Pitch
bassTwiddle =
  scale (2 % 7) $ scale (1 % 4) $
      im
        [ (G,  2)
        , (A,  2)
        , (G,  2)
        , (Fs, 2)
        ]

bassTwiddleG :: Score Pitch
bassTwiddleG =
  co bassTwiddle <> tile (10 % 7) (G, 2)

octaveChord :: PitchClass -> Int -> Score Pitch
octaveChord pc o =
  chord
      [ (pc, o)
      , (pc, o + 1)
      ]

strike1 :: Score Pitch
strike1 = mconcat
  [ co (scale (1 % 7) $ octaveChord Fs 5)
  , scale (10 % 7) $ octaveChord G 5
  ]

strike2 :: Score Pitch
strike2 = mconcat
  [ co (scale (1 % 7) $ octaveChord Ef 5)
  , octaveChord D 5
  ]

bass1, bass1Twice :: Score Pitch
bass1 =
  let [a, b, c] = inversion 2 $ minor G 2
   in bassTemplate a b c
bass1Twice = stimes 2 bass1

sec1 :: Score Pitch
sec1 = mconcat
  [ bass1Twice
  , fork bassTwiddleG bass1Twice
  ]

sec2 :: Score Pitch
sec2 = mconcat
  [ fork strike1 bass1Twice
  , fork bassTwiddleG bass1Twice
  ]

secBuilder :: (Int -> Simul Pitch) -> Score Pitch
secBuilder f = foldMap (\(i, t) -> fork (getSimul $ f i) t) $
  zip [0..]
    [ bass1
    , bassTemplate (F,  3) (G,  3) (Bf, 3)
    , bassTemplate (Ef, 3) (G,  3) (Bf, 3)
    , bassTemplate (Ef, 3) (G,  3) (A,  3)
    , bassTemplate (C,  3) (F,  3) (A,  3)
    , bassTemplate (D,  3) (F,  3) (A,  3)
    , bassTemplate (Ds, 3) (Fs, 3) (A,  3)
    , bassTemplate (D,  3) (Fs, 3) (A,  3)
    ]

trebleTwiddle1 :: Score Pitch
trebleTwiddle1 =
  scale (2 % 7) $ scale (1 % 4) $
      im
        [ (D,  5)
        , (Ef, 5)
        , (D,  5)
        , (Cs, 5)
        ]

trebleTwiddle2 :: Score Pitch
trebleTwiddle2 =
  scale (2 % 7) $ scale (1 % 4) $
      im
        [ (D,  5)
        , (Ef, 5)
        , (D,  5)
        , (C,  5)
        ]

twiddleD :: Score Pitch
twiddleD = co trebleTwiddle1 <> octaveChord D 5


sec3 :: Score Pitch
sec3 = secBuilder $ Simul . \case
  0 -> strike2
  4 -> twiddleD
  _ -> mempty

sec3'2 :: Score Pitch
sec3'2 = secBuilder $ Simul . \case
  0 -> twiddleD
  4 -> twiddleD
  _ -> mempty

ascTriplets :: (Pitch, Pitch) -> (Pitch, Pitch) -> (Pitch, Pitch) -> Score Pitch
ascTriplets (l1, h1) (l2, h2) (l3, h3) =
  mconcat
    [ co $ scale (4 % 7) $ scale (1 % 2) $ mconcat
      [ chord [l1, h1]
      , chord [l2, h2]
      ]
    , chord [l3, h3]
    ]

triple1, triple3, triple5, triple7 :: Score Pitch
triple1 = ascTriplets ((Ef, 4), (G,  4)) ((F,  4), (A, 4)) ((G,  4), (Bf, 4))
triple3 = ascTriplets ((G,  4), (Bf, 4)) ((A,  4), (C, 5)) ((Bf, 4), (D,  5))
triple5 = ascTriplets ((D,  4), (F,  4)) ((Ef, 4), (G, 4)) ((F,  4), (A,  4))
triple7 = ascTriplets ((Bf, 3), (D,  4)) ((C,  4), (E, 4)) ((D,  4), (Fs, 4))


sec5Bits :: Int -> Simul Pitch
sec5Bits =
  Simul . \case
    0 -> co trebleTwiddle2 <> octaveChord Bf 4
    1 -> triple1
    3 -> triple3
    5 -> triple5
    7 -> triple7
    _ -> mempty

sec5 :: Score Pitch
sec5 = secBuilder $ sec5Bits <> Simul . \case
  2 -> twiddleD
  4 -> bassTwiddleG
  _ -> mempty



strike3 :: Score Pitch
strike3 = mconcat
  [ co (scale (1 % 7) $ octaveChord D 5)
  , octaveChord Ef 5
  ]

strike4 :: Score Pitch
strike4 = mconcat
  [ co (scale (1 % 7) $ octaveChord Ef 5)
  , octaveChord F 5
  ]

sec7 :: Score Pitch
sec7 = secBuilder $ sec5Bits <> Simul . \case
  2 -> strike2
  4 -> strike3
  6 -> strike4
  _ -> mempty

song :: Score Pitch
song = mconcat
  [ sec1
  , sec2
  , sec3
  , sec3'2
  , stimes 2 sec5
  , stimes 2 sec7
  , stimes 2 sec9
  ]


lh9Template :: Score a -> Score a -> Score a
lh9Template a b = mconcat
  [ scaleTo (3 % 7) a
  , stimes 2 $ scaleTo (2 % 7) b
  ]


rh9Template :: Score a -> Score a
rh9Template a = stimes 7 $ scaleTo (1 % 7) a

sec9 :: Score Pitch
sec9 = mconcat
  [ bar  (chord $ minor G 4)
         (octaveChord G 1)
         (chord $ minor G 3)
  , bar' (chord [(F,  4), (Bf, 4), (D,  5)])
         (chord [(F,  3), (Bf, 3), (D,  4)])
  , bar' (chord [(G,  4), (Bf, 4), (Ef, 5)])
         (chord $ maj Ef 3)
  , bar' (chord [(A,  4), (C,  5), (Ef, 5)])
         (chord [(Ef, 3), (A,  3), (C,  4)])
  , bar  (chord $ maj F 4)
         (octaveChord F 1)
         (chord $ inversion 2 $ maj F 2)
  , bar' (chord [(F,  4), (A,  4), (D,  5)])
         (chord $ inversion 2 $ maj F 2)
  , bar  (chord [(Fs, 4), (A,  4), (Ds, 5)])
         (octaveChord D 1)
         (chord [(Fs, 3), (A,  3), (D,  4)])
  , do
      let lhs = chord [(Fs, 3), (A, 3), (C, 4)]
      fork (lh9Template lhs lhs)
        $ scaleTo 1
        $ mconcat
            [ stimes 5 $ chord [(Fs, 4), (A, 4), (D, 5)]
            , stimes 2 $ chord [(Fs, 4), (A, 4), (C, 5)]
            ]

  ]
  where
    bar  a b c = fork (rh9Template a) $ lh9Template b c
    bar' a b = bar a b b


score :: Score Pitch
score = mconcat
  [ song
  ]

main :: IO ()
main =
  playScore score

