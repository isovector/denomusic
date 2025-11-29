{-# LANGUAGE OverloadedStrings #-}

module Pieces.Pitchy where

import Data.Bool
import Control.Lens
import APitch hiding (APitch(..))
import APitch2
import Data.Semigroup
import Combinators
import Score
import Data.Music.Lilypond.Pitch

motif :: Score (APitch Int)
motif = mconcat
  [ tile 0.25 $ pure 0
  , tile (1/8) $ pure 1
  , tile (1/8) $ pure 2
  , tile 0.25 $ pure 3
  , tile 0.25 $ pure 4
  ]


motifed :: Score (APitch a) -> (APitch a -> APitch b) -> Score (APitch b)
motifed z f = unflatten . (unsafePartsOf (traverse . _1) %~ fmap f) $ flatten z

c4 :: Pitch
c4 = Pitch (C, 0, 4)

iterateN :: Int -> (a -> a) -> a -> a
iterateN 0 _ a = a
iterateN n f a = iterateN (n - 1) f (f a)

score :: Score (APitch Pitch)
score = mconcat
  [ motifed motif $ \z -> do
      i <- z
      Degree (pure c4) major i
  , delay 1
  , motifed motif $ \z -> do
      i <- z
      Degree (pure c4) major $ negate i
  , delay 1
  , motifed motif $ \z -> do
      i <- z
      Degree (bool Flat Sharp (mod i 2 == 0) $ pure c4) major i
  , delay 1
  , motifed motif $ \z -> do
      i <- z
      let int = IntervalOn $ fromSemis i
      int $ int $ pure c4
  , delay 1
  , motifed motif $ \z -> do
      i <- z
      iterateN i (IntervalOn Maj3) $ pure c4
  , delay 1
  , motifed motif $ \z -> do
      i <- z
      Degree (iterateN i (IntervalOn Maj3) $ pure c4) hungarianMajor (-i)
  , delay 1
  , motifed motif $ \z -> do
      i <- z
      iterateN i (\z -> Degree z hungarianMinor 2) $ pure c4
  ]



main :: IO ()
main = do
  -- toPdf s
  playScore $ fmap eval score


