{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

module Combinators where

import Euterpea (Pitch, pitch, absPitch)
import Score
import Control.Monad.State

retro :: Score a -> Score a
retro s
  = flip mappend (delay $ duration s)
  . unflatten
  . flip evalState Nothing
  . traverse (traverse go)
  . reverse
  $ flatten s
  where
    go :: Envelope Rational -> State (Maybe Rational) (Envelope Rational)
    go (Envelope dur off v) =
      get >>= \case
        Nothing -> do
          put $ Just $ off + dur
          pure $ Envelope dur 0 v
        Just end -> do
          pure $ Envelope dur (end - off - dur) v


negativeHarmony :: Num a => a -> Score a -> Score a
negativeHarmony relTo = fmap $ \p -> negate (p - relTo) + relTo

negativeHarmony' :: Pitch -> Score Pitch -> Score Pitch
negativeHarmony' c = fmap pitch . negativeHarmony (absPitch c) . fmap absPitch
