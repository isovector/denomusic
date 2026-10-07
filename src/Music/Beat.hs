{-# LANGUAGE TemplateHaskell #-}

module Music.Beat where

import GHC.Exts
import FRP.Types.Time
import Data.Functor.Foldable.TH
import FRP


data Beat = Beat
  { bduration :: Time
  , stress :: Priority
  }
  deriving stock (Eq, Ord, Show)


newtype Priority = P Int
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum)


data Meter a
  = Pulse a
  | Group [Meter a]
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Applicative Meter where
  pure = Pulse
  liftA2 f (Pulse a) (Pulse b) = Pulse $ f a b
  liftA2 f (Group a) (Pulse b) = Group $ fmap (fmap $ flip f b) a
  liftA2 f (Pulse a) (Group b) = Group $ fmap (fmap $ f a) b
  liftA2 f (Group a) (Group b) = Group $ liftA2 (liftA2 f) a b

instance Monad Meter where
  Pulse a >>= f = f a
  Group as >>= f = Group $ fmap (>>= f) as

instance IsList (Meter a) where
  type Item (Meter a) = Meter a
  toList (Group ms) = ms
  toList (Pulse a) = [Pulse a]
  fromList = Group


makeBaseFunctor ''Meter


partitionBeats :: Priority -> SF (Event Beat) (Event Beat, Event Beat)
partitionBeats p = proc b -> do
  strong <- filterE ((<= p) . stress) -< b
  weak   <- filterE ((> p) . stress)  -< b
  returnA -< (strong, weak)


setDuration :: Time -> Beat -> Beat
setDuration d (Beat _ p) = Beat d p

