{-# LANGUAGE LambdaCase      #-}

module Etude16 where

import Data.Semigroup
import Data.Ratio
import Euterpea (Pitch, PitchClass(..))
import Tile

bassRhythm :: Tile a -> Tile a -> Tile a
bassRhythm a b = mconcat
  [ a, b, b
  , a, b, a, b
  ]

chord :: [a] -> Tile a
chord = simul . fmap (tile 1)

bassTemplate :: Pitch -> Pitch -> Pitch -> Tile Pitch
bassTemplate a b c =
  scale (1 % 7) $
    bassRhythm
      (chord [a, b])
      (tile 1 c)

im :: [a] -> Tile a
im = foldMap (tile 1)

bassTwiddle :: Tile Pitch
bassTwiddle =
  scale (2 % 7) $ scale (1 % 4) $
      im
        [ (G, 2)
        , (A, 2)
        , (G, 2)
        , (Fs, 2)
        ]

bassTwiddleG :: Tile Pitch
bassTwiddleG =
  co bassTwiddle <> tile (10 % 7) (G, 2)

octaveChord :: PitchClass -> Int -> Tile Pitch
octaveChord pc o =
  chord
      [ (pc, o)
      , (pc, o + 1)
      ]

strike1 :: Tile Pitch
strike1 = mconcat
  [ co (scale (1 % 7) $ octaveChord Fs 5)
  , scale (10 % 7) $ octaveChord G 5
  ]

strike2 :: Tile Pitch
strike2 = mconcat
  [ co (scale (1 % 7) $ octaveChord Ef 5)
  , octaveChord D 5
  ]

bass1, bass1Twice :: Tile Pitch
bass1 = bassTemplate (D, 3) (G, 3) (Bf, 3)
bass1Twice = stimes 2 bass1

sec1 :: Tile Pitch
sec1 = mconcat
  [ bass1Twice
  , fork bassTwiddleG bass1Twice
  ]

sec2 :: Tile Pitch
sec2 = mconcat
  [ fork strike1 bass1Twice
  , fork bassTwiddleG bass1Twice
  ]

secBuilder :: (Int -> Tile Pitch) -> Tile Pitch
secBuilder f = foldMap (\(i, t) -> fork (f i) t) $
  zip [0..]
    [ bass1
    , bassTemplate (F, 3) (G, 3) (Bf, 3)
    , bassTemplate (Ef, 3) (G, 3) (Bf, 3)
    , bassTemplate (Ef, 3) (G, 3) (A, 3)
    , bassTemplate (C, 3) (F, 3) (A, 3)
    , bassTemplate (D, 3) (F, 3) (A, 3)
    , bassTemplate (Ds, 3) (Fs, 3) (A, 3)
    , bassTemplate (D, 3) (Fs, 3) (A, 3)
    ]

trebleTwiddle1 :: Tile Pitch
trebleTwiddle1 =
  scale (2 % 7) $ scale (1 % 4) $
      im
        [ (D, 5)
        , (Ef, 5)
        , (D, 5)
        , (Cs, 5)
        ]

trebleTwiddle2 :: Tile Pitch
trebleTwiddle2 =
  scale (2 % 7) $ scale (1 % 4) $
      im
        [ (D, 5)
        , (Ef, 5)
        , (D, 5)
        , (C, 5)
        ]


sec3 :: Tile Pitch
sec3 = secBuilder $ \case
  0 -> strike2
  4 -> co trebleTwiddle1 <> octaveChord D 5
  _ -> mempty

sec3'2 :: Tile Pitch
sec3'2 = secBuilder $ \case
  0 -> co trebleTwiddle1 <> octaveChord D 5
  4 -> co trebleTwiddle1 <> octaveChord D 5
  _ -> mempty

ascTriplets :: (Pitch, Pitch) -> (Pitch, Pitch) -> (Pitch, Pitch) -> Tile Pitch
ascTriplets (l1, h1) (l2, h2) (l3, h3) =
  mconcat
    [ co $ scale (4 % 7) $ scale (1 % 2) $ mconcat
      [ chord [l1, h1]
      , chord [l2, h2]
      ]
    , chord [l3, h3]
    ]

triple1, triple3, triple5, triple7 :: Tile Pitch
triple1 = ascTriplets ((Ef, 4), (G, 4)) ((F, 4), (A, 4)) ((G, 4), (Bf, 4))
triple3 = ascTriplets ((G, 4), (Bf, 4)) ((A, 4), (C, 5)) ((Bf, 4), (D, 5))
triple5 = ascTriplets ((D, 4), (F, 4)) ((Ef, 4), (G, 4)) ((F, 4), (A, 4))
triple7 = ascTriplets ((Bf, 3), (D, 4)) ((C, 4), (E, 4)) ((D, 4), (Fs, 4))

sec5 :: Tile Pitch
sec5 = secBuilder $ \case
  0 -> co trebleTwiddle2 <> octaveChord Bf 4
  1 -> triple1
  3 -> triple3
  5 -> triple5
  7 -> triple7
  _ -> mempty


strike3 :: Tile Pitch
strike3 = mconcat
  [ co (scale (1 % 7) $ octaveChord D 5)
  , octaveChord Ef 5
  ]

strike4 :: Tile Pitch
strike4 = mconcat
  [ co (scale (1 % 7) $ octaveChord Ef 5)
  , octaveChord F 5
  ]

sec7 :: Tile Pitch
sec7 = secBuilder $ \case
  0 -> co trebleTwiddle2 <> octaveChord Bf 4
  1 -> triple1
  2 -> strike2
  3 -> triple3
  4 -> strike3
  5 -> triple5
  6 -> strike4
  7 -> triple7
  _ -> mempty

score :: Tile Pitch
score = scale (2 / 3) $ mconcat
  [ sec1
  , sec2
  , sec3
  , sec3'2
  , stimes 2 sec5
  , stimes 2 sec7
  ]

main :: IO ()
main = playTile score


