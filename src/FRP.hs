{-# OPTIONS_GHC -Wno-orphans #-}

module FRP
  ( module Control.Arrow
  , Alternative (..)
  , module FRP
  , module FRP.Types
  , Interval(..)
  ) where

import Control.Monad
import Control.Monad.Cont
import Data.Void
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Exception (evaluate)
import Data.Bool
import Data.Maybe
import Data.Monoid
import Data.Ratio
import FRP.Types
import Prelude hiding (id, (.))
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)


-- | The 'Time's must be monotonically increasing.
discrete :: [(Time, a)] -> SF x (Event a)
discrete = SF . const . Discrete (const Event) (const NoEvent)

every :: Time -> a -> SF x (Event a)
every dur a = discrete $ zip (iterate (+ dur) 0) $ repeat a

at :: Time -> a -> SF x (Event a)
at t' a = discrete $ pure (t', a)

invmapTime
    :: (Time -> Time)  -- ^ co
    -> (Time -> Time)  -- ^ contra
    -> SF a a
invmapTime co contra = SF $ \case
  Discrete k a0 as ->
    Discrete (k . contra) (a0 . contra) $
      fmap (first co) as
  Stepwise k a as ->
    Stepwise (k . contra) a $
      fmap (first co) as
  Hybrid a as ->
    Hybrid (runSF (invmapTime co contra) a) $
      fmap (co *** runSF (invmapTime co contra)) as

-- | Stretch time by the given amount.
stretch :: Rational -> SF a a
stretch r = invmapTime (* r) (/ r)

now :: a -> SF x (Event a)
now = at 0

-- | Observe whether a computation would diverge, and if so, return 'Nothing'
-- instead. This can be used to guard otherwise-sketchy combinators which need
-- to fold over infinite event streams.
--
-- This is impolemented by terminating after 10ms of trying.
terminating :: a -> Maybe a
terminating a = unsafePerformIO $! timeout 10_000 $! evaluate a

switch :: SF a (b, Event c) -> (c -> SF a b) -> SF a b
switch (SF f) k = SF $ \sig -> do
  let sig' = f sig
      sig'1 = fmap fst sig'
  case sig' of
    Discrete ka _ as -> do
      case terminating $! listToMaybe $! mapMaybe (\(t, a) -> sequenceA (t, eventToMaybe $ snd $ ka t a)) as of
        Just (Just (t0, a)) ->
          mkHybrid sig'1 $ pure (t0, runSF (offset t0 <<< k a <<< offset (- t0)) sig)
        _ -> sig'1
    Stepwise{} -> sig'1


-- | Hold the value of the most recent value of an 'Event'.
hold :: a -> SF (Event a) a
hold a0 = SF $ \case
  Discrete k _ as -> Stepwise (const id) a0 $ mapMaybe (\(t, a) -> sequenceA (t, eventToMaybe $ k t a)) as
  Stepwise{} -> error "hold on stepwise"
  Hybrid{} -> error "hold on hybrid"

-- -- | Hold the value of the next (not yet occurred!) value of an 'Event'.
fhold :: a -> SF (Event a) a
fhold a0 = SF $ \case
  Discrete k _ as -> do
    let as' = mapMaybe (\(t, a) -> sequenceA (t, eventToMaybe $ k t a)) as
    case terminating $! as' of
      Just ((_, a) : as'') ->
        Stepwise (const id) a $ zip (fmap fst as) (fmap snd as'' <> [a0])
      _ -> pure a0
  Stepwise{} -> error "fhold on stepwise"
  Hybrid{} -> error "hold on hybrid"

offset :: Time -> SF a a
offset dt = invmapTime (+ dt) (subtract dt)

localTime :: SF x Time
localTime = SF $ const $ Discrete @Void (const absurd) id mempty

replace :: [a] -> SF (Event b) (Event (b, a))
replace as = ev2ev $ \bs -> zipWith (\(t, b) a -> (t, (b, a))) bs as

partitionEvents :: (a -> Either b c) -> SF (Event a) (Event b, Event c)
partitionEvents f = proc eva -> do
  evb <- mapMaybeE (either Just (const Nothing) . f) -< eva
  evc <- mapMaybeE (either (const Nothing) Just . f) -< eva
  returnA -< (evb, evc)


filterE :: (a -> Bool) -> SF (Event a) (Event a)
filterE f = ev2ev $ filter (f . snd)

filterTimeE :: (Time -> Bool) -> SF (Event a) (Event a)
filterTimeE f = ev2ev $ filter (f . fst)

mapMaybeE :: (a -> Maybe b) -> SF (Event a) (Event b)
mapMaybeE = ev2ev . mapMaybe . traverse


gate :: Bool -> Event a -> Event a
gate False _ = NoEvent
gate True e = e

notYet :: SF (Event a) (Event a)
notYet = filterTimeE (> 0)

once :: SF (Event a) (Event a)
once = takeE 1

takeE :: Int -> SF (Event a) (Event a)
takeE = ev2ev . take

dropE :: Int -> SF (Event a) (Event a)
dropE = ev2ev . drop

accum :: a -> SF (Event (a -> a)) (Event a)
accum a0 = ev2ev $ drop 1 . scanl (\(_, a) (t', f) -> (t', f a)) (undefined, a0)

onlyEvery :: Int -> SF (Event a) (Event a)
onlyEvery n = proc ev -> do
  x <- hold 0 <<< accum 0 -< (+1) <$ ev
  returnA -< bool NoEvent ev $ mod x n == 0

subdiv :: Int -> SF (Event Beat) (Event Beat)
subdiv n = ev2ev $ \bs -> do
  (t, Beat d s) <- bs
  let d' = d / fromIntegral n
  take n $ zip (iterate (+ d') t) $ Beat d' s : repeat (Beat d' $ succ s)

newtype Seq i o a = Seq
  { unSeq :: Cont (SF i o) a
  }
  deriving newtype (Functor, Applicative, Monad)


toSeq :: SF i (Event o, Event a) -> Seq i (Event o) a
toSeq = Seq . cont . switch

switchSeq :: Seq i o a -> (a -> SF i o) -> SF i o
switchSeq = runCont . unSeq

getSeq :: Seq i (Event o) a -> SF i (Event o)
getSeq = flip switchSeq $ const $ arr $ const NoEvent

rest :: Time -> Seq i (Event a) ()
rest t = toSeq $ proc i -> do
  e <- at t () -< i
  returnA -< (NoEvent, e)

hit :: Time -> a -> Seq i (Event a) ()
hit t a = toSeq $ proc i -> do
  n <- now a -< i
  e <- at t () -< i
  returnA -< (n, e)

beat :: Time -> Priority -> Seq i (Event Beat) ()
beat t p = hit t $ Beat t p


-- test :: SF i (Event Char)
-- test =
--   switch
--     (liftA2 (,) (getSeq $ replicateM 4 $ hit 0.25 'a') (at 0.55 ()))
--     $ const $ getSeq $ replicateM 4 $ hit 0.05 'b'

