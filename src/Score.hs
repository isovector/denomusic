{-# LANGUAGE DerivingStrategies   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Score
  ( Score
  , offset
  , simultaneously
  , bar
  , playScore
  ) where

import Data.Bifunctor
import Data.List (unfoldr)
import Euterpea.IO.MIDI.Play (playDev)
import Rhythm
import Euterpea (Music (..), Pitch, rest)
import Data.IntervalMap.FingerTree (IntervalMap)
import Data.IntervalMap.FingerTree qualified as IM


data Score a = Score
  { duration :: Rational
  , unScore :: IntervalMap Rational (Rhythm a)
  }
  deriving stock (Show, Functor)

toIntervals :: Ord v => IntervalMap v a -> [(IM.Interval v, a)]
toIntervals = unfoldr IM.leastView

fromIntervals :: Ord v => [(IM.Interval v, a)] -> IntervalMap v a
fromIntervals = foldr (uncurry IM.insert) mempty

deriving stock instance Functor IM.Interval


offset :: (Num v, Ord v) => v -> IntervalMap v a -> IntervalMap v a
offset t = fromIntervals . fmap (first $ fmap (+ t)) . toIntervals

instance Semigroup (Score a) where
  a <> b = Score (duration a + duration b) $
    unScore a <> offset (duration a) (unScore b)

instance Monoid (Score a) where
  mempty = Score 0 mempty


simultaneously :: Score a -> Score a -> Score a
simultaneously (Score d1 a) (Score d2 b) = Score (max d1 d2) $ a <> b


bar :: Rhythm a -> Score a
bar = Score 1 . IM.singleton (IM.Interval 0 1)

playScore :: Score Pitch -> IO ()
playScore
  = playDev 2
  . foldr (:=:) (rest 0)
  . fmap (\(iv, a) -> rest (IM.low iv) :+: foldMusic (intervals a))
  . toIntervals
  . unScore

