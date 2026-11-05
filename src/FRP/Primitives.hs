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


scanSteps :: (s -> Event a -> s) -> s -> Signal (Event a) -> [(Time, s)]
scanSteps f s0
  = drop 1
  . scanl (\(_, s) (t, ea) -> (t, f s ea)) (undefined, s0)
  . values

scanE :: (s -> a -> (s, b)) -> s -> SF (Event a) (Event b)
scanE f s0
  = SF
  $ \input -> unionClock input
  $ mkDiscrete'
  $ fmap (fmap snd)
  $ (scanSteps
      (\(s, _) ea -> case ea of
                       Event a -> fmap Event $ f s a
                       NoEvent -> (s, NoEvent)
                       )
      (s0, NoEvent)) input


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

constS :: Signal a -> SF x a
constS = SF . const

switchS :: Signal (a, Event b) -> (b -> Signal a) -> Signal a
switchS s k = runSF (switch (constS s) $ constS . k) $ pure ()

export
    :: (Time, Time)
    -> SF () (Event (Span a))
    -> [(Interval Time, a)]
export (lo, hi)
  = fmap (\(time, Span d o) -> do
      let t = time - lo
      (Interval t $ t + d, o)
        )
  . takeWhile ((< hi) . fst)
  . dropWhile ((< lo) . fst)
  . observe

