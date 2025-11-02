{-# LANGUAGE DerivingStrategies   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Score
  ( Score
  , playScore

  -- * Construction
  , bar
  , asBars

  -- * Combinators
  , strut
  , delay
  , par
  , parL
  ) where

import Data.Bifunctor
import Data.List (unfoldr)
import Euterpea.IO.MIDI.Play (playDev)
import Rhythm
import Euterpea (Music (..), Pitch, rest)
import Data.IntervalMap.FingerTree (IntervalMap)
import Data.IntervalMap.FingerTree qualified as IM


data Score a = Score
  { envelope :: IM.Interval Rational
  , unScore :: IntervalMap Rational (Rhythm a)
  }
  deriving stock (Show, Functor)

toIntervals :: Ord v => IntervalMap v a -> [(IM.Interval v, a)]
toIntervals = unfoldr IM.leastView

fromIntervals :: Ord v => [(IM.Interval v, a)] -> IntervalMap v a
fromIntervals = foldr (uncurry IM.insert) mempty

deriving stock instance Functor IM.Interval

duration :: Score a -> Rational
duration s =
  let i = envelope s
   in IM.high i - IM.low i

start :: Score a -> Rational
start = IM.low . envelope


offset :: (Num v, Ord v) => v -> IntervalMap v a -> IntervalMap v a
offset t = fromIntervals . fmap (first $ fmap (+ t)) . toIntervals

-- abut :: Num v => IM.Interval v -> IM.Interval v -> IM.Interval v
-- abut (IM.Interval a b) (IM.Interval c d) = IM.Interval a

instance Semigroup (Score a) where
  a <> b = Score (IM.Interval (start a) $ duration a + duration b + start b) $
    unScore a <> offset (duration a + start b) (unScore b)

instance Monoid (Score a) where
  mempty = Score (IM.point 0) mempty


par :: Score a -> Score a -> Score a
par (Score d1 a) (Score d2 b) = Score (max d1 d2) $ a <> b


parL :: Score a -> Score a -> Score a
parL (Score d a) (Score o b) = Score d $ a <> offset (IM.low o) b


bar :: Rhythm a -> Score a
bar = asBars 1


asBars :: Rational -> Rhythm a -> Score a
asBars a = Score (IM.Interval 0 a) . IM.singleton (IM.Interval 0 a)


strut :: Rational -> Score a
strut n = Score (IM.Interval 0 n) mempty


delay :: Rational -> Score a -> Score a
delay r (Score a b) = Score (fmap (+ r) a) b


playScore :: Score Pitch -> IO ()
playScore
  = playDev 2
  . foldr (:=:) (rest 0)
  . fmap (\(iv, a) -> rest (IM.low iv) :+: foldMusic (fmap (first $ fmap $ (* (IM.high iv - IM.low iv))) $ intervals a))
  . toIntervals
  . unScore

