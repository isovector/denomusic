{-# OPTIONS_GHC -Wno-x-partial #-}

module FRP.Primitives where

import GHC.Generics
import Data.Set qualified as S
import Data.Set (Set)
import Control.Lens (view)
import FRP.Types
import Data.Maybe (listToMaybe)


discrete :: [(Time, a)] -> SF x (Event a)
discrete = SF . const . mkDiscrete


steps :: a -> [(Time, a)] -> SF x a
steps a = SF . const . mkSteps a


ev2ev :: ([(Time, a)] -> [(Time, b)]) -> SF (Event a) (Event b)
ev2ev f = SF $ mkDiscrete . f . events


evByEv :: ((Time, a) -> (Time, a) -> [(Time, b)]) -> SF (Event a) (Event b)
evByEv f = SF $ \s -> do
  let es = events s
  mkDiscrete $ concat $ zipWith f es (tail es)


observe :: SF () (Event a) -> [(Time, a)]
observe sf = events $ runSF sf (pure ())


switch :: SF a (b, Event c) -> (c -> SF a b) -> SF a b
switch (SF f) k = SF $ \sig -> do
  let sig' = f sig
      sig'1 = fmap fst sig'
  case listToMaybe $ eventsTerminating $ fmap snd sig' of
    Just (t0, a) ->
      spliceAt sig'1 t0 $ runSF (k a) sig
    Nothing -> sig'1


newtype Notes a = Notes
  { getNotes :: Set (Time, a)
  }
  deriving stock Generic
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid)


instance Ord a => HasDuration (Notes a) where
  duration f (Notes n) = fmap (Notes . S.fromList) $ traverse (\(t, a) -> fmap (, a) $ f t) $ S.toList n


export
    :: (HasDuration a)
    => (Time, Time)
    -> SF () (Event a)
    -> [(Interval Time, a)]
export (lo, hi)
  = fmap (\(time, o) -> do
      let t = time - lo
      (Interval t $ t + view duration o, o)
        )
  . takeWhile ((< hi) . fst)
  . dropWhile ((< lo) . fst)
  . observe

