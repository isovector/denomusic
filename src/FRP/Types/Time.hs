{-# OPTIONS_GHC -Wno-orphans #-}

module FRP.Types.Time where

import Data.Group

type Time = Rational

instance Semigroup Time where
  (<>) = (+)

instance Monoid Time where
  mempty = 0

instance Group Time where
  invert = negate

