{-# OPTIONS_GHC -Wno-orphans #-}

module FRP
  ( module Control.Arrow
  , Alternative (..)
  , module FRP
  , module FRP.Types
  , Interval(..)
  ) where

import Control.Applicative hiding (Const)
import Control.Arrow
import Control.Category
import Control.Exception (evaluate)
import Control.Monad (join)
import Control.Monad.Writer (runWriter, tell, mapWriter)
import Data.Align
import Data.Bifunctor (bimap)
import Data.Bool
import Data.Coerce
import Data.Either (partitionEithers)
import Data.Functor
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Monoid
import Data.Ratio
import Data.Semigroup qualified as S
import Data.Set (Set)
import Data.Set qualified as S
import Data.These
import FRP.Types
import Prelude hiding (id, (.))
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)



-- sf :: Clock -> (Time -> a -> b) -> SF a b
-- sf clk' f = SF $ \(Signal clk s) ->
--   Signal (clk <> clk') $ \t -> f t (s t)


-- -- downbeat :: SF (Event Beat) (Event ())
-- -- downbeat = fmap void $ filterE (== 0)

-- -- upbeat :: SF (Event Beat) (Event ())
-- -- upbeat = proc ev -> do
-- --   y <- fhold (-1) -< ev
-- --   returnA -< void $ ev >> bool NoEvent (Event ()) (y == 0)

-- | The 'Time's must be monotonically increasing.
discrete :: [(Time, a)] -> SF x (Event a)
discrete = SF . const . Discrete (const Event) (const NoEvent)

every :: Time -> a -> SF x (Event a)
every dur a = discrete $ zip (iterate (+ dur) 0) $ repeat a

at :: Time -> a -> SF x (Event a)
at t' a = discrete $ pure (t', a)

-- TODO(sandy): do we need to update the continuations?
invmapTime
    :: (Time -> Time)  -- ^ co
    -> (Time -> Time)  -- ^ contra
    -> SF a a
invmapTime co contra = SF $ \case
  Const a -> Const a
  Continuous f -> Continuous $ f . contra
  Discrete k a0 as -> Discrete k a0 $ fmap (first co) as
  Stepwise k a as ->  Stepwise k a $ fmap (first co) as

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

-- -- TODO(sandy): What would fswitch do? Run the first SF until the event in the
-- -- second would trigger?
-- switchBy :: (b -> b -> b) -> SF a (b, Event c) -> (c -> SF a b) -> SF a b
-- switchBy comb (SF f) k = SF $ \sig -> do
--   let sig' = f sig
--   case sig' of
--     Const (_, Event c) -> runSF (k c) sig
--     Discrete ka as ->
--       case terminating $! listToMaybe as of
--         Just (Just (t0, a)) -> do
--           let sig2 = runSF (offset t0 <<< k c <<< offset (- t0)) sig0

--           -- Signal (clock sig1 <> clock sig2) $ \t ->
--           --   flip sample t $ bool sig1b sig2 $ t >= t0
--         _ -> fmap fst sig'
--     _ -> fmap fst sig'

--   -- SF $ \sig0@Signal{} -> do
--   -- let sig1 = f sig0
--   --     sig1b = fmap fst sig1
--   -- case listToMaybe $ signalEvs $ fmap snd sig1 of
--   --   Nothing -> sig1b
--   --   Just (t0, c) -> do


-- | Hold the value of the most recent value of an 'Event'.
hold :: a -> SF (Event a) a
hold a0 = SF $ \case
  Discrete k _ as -> Stepwise (const id) a0 $ mapMaybe (\(t, a) -> sequenceA (t, eventToMaybe $ k t a)) as
  Const NoEvent -> Const a0
  Const (Event a) -> Const a
  Continuous{} -> error "hold on continuous"
  Stepwise{} -> error "hold on stepwise"

-- -- | Hold the value of the next (not yet occurred!) value of an 'Event'.
fhold :: a -> SF (Event a) a
fhold a0 = SF $ \case
  Discrete k _ as -> do
    let as' = mapMaybe (\(t, a) -> sequenceA (t, eventToMaybe $ k t a)) as
    case terminating $! as' of
      Just ((_, a) : as') ->
        Stepwise (const id) a $ zip (fmap fst as) (fmap snd as' <> [a0])
      _ -> Const a0
    -- Stepwise (const id) a0 $
  Const NoEvent -> Const a0
  Const (Event a) -> Const a
  Continuous{} -> error "fhold on continuous"
  Stepwise{} -> error "fhold on stepwise"

offset :: Time -> SF a a
offset dt = invmapTime (+ dt) (subtract dt)

localTime :: SF x Time
localTime = SF $ const $ Continuous id

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

