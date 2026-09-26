{-# OPTIONS_GHC -Wno-x-partial #-}

module FRP.Types
  ( module FRP.Types
  , module FRP.Types.Signal
  , module FRP.Event
  , Time
  , Interval(..)
  ) where

import Control.Applicative (WrappedArrow(..))
import Control.Arrow
import Control.Category
import Data.Function (on)
import Data.Functor
import Data.IntervalMap.FingerTree (Interval(..))
import Data.List (groupBy, sortBy)
import Data.Monoid
import Data.Ratio
import Data.Set (Set)
import Data.Set qualified as S
import FRP.Event
import FRP.Time
import FRP.Types.Signal
import Prelude hiding (id, (.))

newtype Notes a = Notes
  { getNotes :: Set (Time, a)
  }
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid)


export
    :: Ord a
    => (Time, Time)
    -> SF () (Event (Notes a))
    -> [(Interval Time, Set a)]
export (lo, hi)
  = concatMap (\o -> do
      let t = fst o - lo
      xs <- groupBy (on (==) fst) $ sortBy (on compare fst) $ S.toList $ getNotes $ snd o
      let d = fst $ head xs
      pure (Interval t (t + d), S.fromList $ fmap snd xs)
        )
  . takeWhile ((< hi) . fst)
  . dropWhile ((< lo) . fst)
  . observe


newtype SF a b = SF
  { runSF :: Signal a -> Signal b
  }
  deriving (Functor, Applicative) via WrappedArrow SF a
  deriving (Semigroup, Monoid) via Ap (SF a) b

instance Category SF where
  id = SF id
  SF g . SF f = SF (g . f)

instance Arrow SF where
  arr = SF . fmap
  SF f *** SF g = SF $ \sg ->
    liftA2 (,) (f $ fmap fst sg) (g $ fmap snd sg)


discrete :: [(Time, a)] -> SF x (Event a)
discrete = SF . const . mkDiscrete


steps :: a -> [(Time, a)] -> SF x a
steps a = SF . const . mkSteps a


ev2ev :: ([(Time, a)] -> [(Time, b)]) -> SF (Event a) (Event b)
ev2ev f = SF $ mkDiscrete . f . events


observe :: SF () (Event a) -> [(Time, a)]
observe sf = events $ runSF sf (pure ())


hold :: a -> SF (Event a) a
hold a0 = SF $ mkSteps a0 . events


fhold :: a -> SF (Event a) a
fhold a0 = SF $ \s -> do
  case events s of
    [] -> Signal (const a0) mempty
    ((t, a) : as) ->
      mkSteps a $ zip (t : fmap fst as) $ fmap snd as <> [a]

