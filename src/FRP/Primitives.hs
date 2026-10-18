module FRP.Primitives where

import FRP.Types
import Data.Maybe (listToMaybe, mapMaybe)


discrete :: [(Time, a)] -> SF x (Event a)
discrete as = SF $ \input ->
  unionClock
    input
    (mkDiscrete as)


steps :: a -> [(Time, a)] -> SF x a
steps a = SF . const . mkSteps a


ev2ev :: ([(Time, a)] -> [(Time, b)]) -> SF (Event a) (Event b)
ev2ev f = SF $ \input ->
  unionClock input $ mkDiscrete $ f $ events input


evStream :: ([(Time, Event a)] -> [(Time, Event b)]) -> SF (Event a) (Event b)
evStream f = SF $ mkDiscrete . mapMaybe (traverse eventToMaybe) . f . values


evByEv :: ((Time, a) -> (Time, a) -> [(Time, b)]) -> SF (Event a) (Event b)
evByEv f = SF $ \s -> do
  let es = events s
  mkDiscrete $ concat $ zipWith f es $ drop 1 es


observe :: SF () (Event a) -> [(Time, a)]
observe sf = events $ runSF sf (pure ())

observeV :: SF () (a) -> [(Time, a)]
observeV sf = values $ runSF sf (pure ())

-- | Generate an event by sampling at local time 0.
dynamicE :: SF (Time, a) (a, Event ())
dynamicE = SF $ \s -> do
  let (t, a) = sample s 0
  fmap (a, ) $ mkDiscrete $ pure (t, ())


switch :: SF a (b, Event c) -> (c -> SF a b) -> SF a b
switch (SF f) k = SF $ \sig -> do
  let sig' = f sig
      sig'1 = fmap fst sig'
  case listToMaybe $ eventsTerminating $ fmap snd sig' of
    Just (t0, a) ->
      spliceAt sig'1 t0 $ runSF (k a) $
        -- Make sure we sample the new signal relative to t0!
        invmapTime (subtract t0) (+ t0) sig
    Nothing -> sig'1


export
    :: (Time, Time)
    -> SF () (Event (Timed a))
    -> [(Interval Time, a)]
export (lo, hi)
  = fmap (\(time, Timed d o) -> do
      let t = time - lo
      (Interval t $ t + d, o)
        )
  . takeWhile ((< hi) . fst)
  . dropWhile ((< lo) . fst)
  . observe

