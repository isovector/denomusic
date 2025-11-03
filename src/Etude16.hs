{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Etude16 where

import Data.Semigroup
import Data.Ord
import Debug.Trace
import Data.Function
import Data.List (maximumBy, minimumBy)
import Control.Monad
import Data.Maybe
import Data.Ratio
import Control.Applicative
import Rhythm
import Euterpea (Pitch, PitchClass(..))
import Euterpea qualified as E
import Tile
import Data.Functor.Foldable
import Data.Functor.Foldable.TH



bar :: Rhythm a -> Tile a
bar = asBars 1


asBars :: Rational -> Rhythm a -> Tile a
asBars r
  = (%% delay r)
  . foldr fork mempty
  . fmap (\(i, a) -> delay (getOffset i * r) <> tile (getDuration i * r) a)
  . intervals

bassRhythm :: Rhythm a -> Rhythm a -> Rhythm a
bassRhythm a b = tuplet
  [ a, b, b
  , a, b, a, b
  ]

bassTemplate :: Pitch -> Pitch -> Pitch -> Rhythm Pitch
bassTemplate a b c =
  bassRhythm
    (chord [a, b])
    (pure c)

asLastBeat :: Rhythm a -> Rhythm a
asLastBeat a = weightedTuplet
  [ (5 % 7, empty)
  , (2 % 7, a)
  ]


twiddle :: Tile Pitch
twiddle =
  co $ asBars (2 % 7) $
      im
        [ (G, 2)
        , (A, 2)
        , (G, 2)
        , (Fs, 2)
        ]

twiddleG :: Tile Pitch
twiddleG = mconcat
  [ twiddle
  , asBars (10 % 7) $ pure (G, 2)
  ]


octaves :: [Int] -> Pitch -> Tile Pitch
octaves os (pc, o) = simul  $ fmap (tile 1 . (pc, ) . (+ o)) os

hammer :: Ord b => (Int -> b) -> Tile Pitch -> Tile Pitch
hammer f t = fromMaybe mempty $ do
  (Events x y, _) <- uncons t
  guard $ y == 0
  let top = maximumBy (on compare $ f . E.absPitch . snd) x
  pure $ octaves [2, 3] $ snd top

approachU :: Tile Pitch -> Tile Pitch
approachU = fmap (E.pitch . subtract 1 . E.absPitch)

approachD :: Tile Pitch -> Tile Pitch
approachD = fmap (E.pitch . (+ 1) . E.absPitch)

striking :: Rational -> Tile Pitch -> Tile Pitch
striking r t = traceShowWith toMusic $
  let ham = hammer id t
   in co (scale r $ approachU ham) <> ham

strikingD :: Rational -> Tile Pitch -> Tile Pitch
strikingD r t = traceShowWith toMusic $
  let ham = hammer Down t
   in co (scale r $ approachD ham) <> ham


strike :: PitchClass -> PitchClass -> Tile Pitch
strike pc1 pc2 = mconcat
  [ co $ asBars (1 % 7) $ chord
      [ (pc1, 5)
      , (pc1, 6)
      ]
  , asBars (10 % 7) $ chord
      [ (pc2, 5)
      , (pc2, 6)
      ]
  ]

-- parL :: Tile a -> Tile a -> Tile a
-- parL a b = _

b = bar $ bassTemplate (D, 3) (G, 3) (Bf, 3)

score :: Tile Pitch
score = mconcat
  -- [ b
  -- , b
  -- , fork twiddleG b
  -- , b
  -- , fork (strike Fs G) b
  -- , b
  -- , fork twiddleG b
  [ b2
  , fork twiddleG b2
  , fork (striking (1 / 7) b) b2
  , fork twiddleG b2
  , fork (strikingD (1 / 7) b) b
  , b'
  , b''
  , b'''
  -- , fork (fmap (E.trans 24) $ twiddle) b''''
  -- , b'''''
  -- , b''''''
  -- , b'''''''
  ]
  where
    b2 = stimes 2 b
    b' = bar $ bassTemplate
          (F, 3)
          (G, 3)
          (Bf, 3)
    b'' = bar $ bassTemplate
          (Ef, 3)
          (G, 3)
          (Bf, 3)
    b''' = bar $ bassTemplate
          (Ef, 3)
          (G, 3)
          (A, 3)
    b'''' = bar $ bassTemplate
          (C, 3)
          (F, 3)
          (A, 3)
    b''''' =
      bar $ bassTemplate
          (D, 3)
          (F, 3)
          (A, 3)
    b'''''' =
      bar $ bassTemplate
          (Ds, 3)
          (Fs, 3)
          (A, 3)
    b''''''' =
      bar $ bassTemplate
          (D, 3)
          (Fs, 3)
          (A, 3)


main :: IO ()
main = playTile score




