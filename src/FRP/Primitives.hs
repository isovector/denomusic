{-# OPTIONS_GHC -Wno-x-partial #-}

module FRP.Primitives where

import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.Set (Set)
import Data.Set qualified as S
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

