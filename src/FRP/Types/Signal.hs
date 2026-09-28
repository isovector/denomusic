module FRP.Types.Signal
  ( Step(..)
  , Signal(..)
  , values
  , events
  , eventsTerminating
  , mkDiscrete
  , mkSteps
  , invmapTime
  , spliceAt
  ) where

import Control.Applicative
import Control.Arrow
import Control.Exception (evaluate)
import Control.Lens (set, ix, _1)
import Data.These
import Data.Maybe
import FRP.Event
import FRP.Time
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)


-- | An interval of the real line. When 'stepVal' is 'Just', this interval has
-- a closed left side. The right side is always open.
data Step a = Step
  { stepVal :: Maybe a
  , step :: Time -> a
  }
  deriving stock Functor


-- | Like 'liftA2' for 'Step', when we have a concrete time to sample at.
combine :: Time -> (a -> b -> c) -> Step a -> Step b -> Step c
combine t f (Step a fa) (Step b fb) =
  Step
    (asum
      [ liftA2 f a b
      , fmap (f (fa t)) b
      , fmap (flip f (fb t)) a
      ])
    (liftA2 f fa fb)


-- | A piecewise function, built out of 'Step' intervals.
data Signal a = Signal (Time -> a) [(Time, Step a)]
  deriving stock Functor

instance Applicative Signal where
  pure a = Signal (pure a) mempty
  liftA2 f (Signal a as) (Signal b bs) =
    Signal (liftA2 f a b)
      $ fmap (uncurry $ \t -> (t,) . uncurry (combine t f))
      $ joining (Step Nothing a) (Step Nothing b) as bs


-- | Specialized 'Data.Align.align' for sorted pairs.
merge :: Ord a => [(a, b)] -> [(a, c)] -> [(a, These b c)]
merge [] ys       = fmap (fmap That) ys
merge (x : xs) [] = fmap (fmap This) (x : xs)
merge xx@((tx, x) : xs) yy@((ty, y) : ys) =
  case compare tx ty of
    LT -> (tx, This x) : merge xs yy
    GT -> (ty, That y) : merge xx ys
    EQ -> (tx, These x y) : merge xs ys


-- | Overlay two lists of sorted pairs.
joining :: Ord a => Step b -> Step c -> [(a, Step b)] -> [(a, Step c)] -> [(a, (Step b, Step c))]
joining a0 b0 as bs =
  drop 1 $ scanl
    (\(_, (a, b)) (t, th) ->
      case th of
        This a' -> (t, (a', open b))
        That b' -> (t, (open a, b'))
        These a' b' -> (t, (a', b'))
    ) (undefined, (a0, b0)) $ merge as bs

open :: Step a -> Step a
open (Step _ a) = Step Nothing a

mapMaybeTerminating :: (a -> Maybe b) -> [a] -> [b]
mapMaybeTerminating f = go
  where
    go as =
      case terminating (findNext as) of
        Just (Just (b, rest)) -> b : go rest
        _ -> []

    findNext [] = Nothing
    findNext (a : as') =
      case f a of
        Just b  -> Just (b, as')
        Nothing -> findNext as'

-- | Get the discrete (closed-endpoint) values of a 'Signal'.
values :: Signal a -> [(Time, a)]
values (Signal _ as) = mapMaybe (traverse stepVal) as


-- | Get the eventful values out of a 'Signal'.
events :: Signal (Event a) -> [(Time, a)]
events = mapMaybe (traverse eventToMaybe) . values


-- | Like 'events', but catches divergence and terminates with an empty list.
eventsTerminating :: Signal (Event a) -> [(Time, a)]
eventsTerminating = mapMaybeTerminating (traverse eventToMaybe) . values


-- | Build a 'Signal' out of discrete values.
mkDiscrete :: [(Time, a)] -> Signal (Event a)
mkDiscrete = Signal (const NoEvent) . fmap (fmap $ \a -> Step (Just $ Event a) $ pure NoEvent)


-- | Build a 'Signal' as a stepwise function.
mkSteps :: a -> [(Time, a)] -> Signal a
mkSteps a = Signal (const a) . fmap (fmap $ Step Nothing . const)


-- | Map a function over time. Since time is represented both co- and
-- contravariantly inside of 'Signal's, we must take both directions of the
-- function. This function must be monotonic.
invmapTime
  :: (Time -> Time)  -- ^ covariant
  -> (Time -> Time)  -- ^ contravariant
  -> Signal a
  -> Signal a
invmapTime co contra (Signal a as) =
  Signal (a . contra) $ fmap (co *** \m -> m { step = step m . contra }) as


-- | @'spliceAt' s1 t s2@ replaces the portion of @s1@ that occurs after @t@
-- with the portion of @s2@ that begins after @t=0@.
spliceAt :: Signal a -> Time -> Signal a -> Signal a
spliceAt (Signal a as) t0 bs =
  Signal a $ mconcat
    [ takeWhile ((< t0) . fst) as
    , fmap ((+ t0) *** \m -> m { step = step m . (subtract t0)}) $ fromZero bs
    ]


-- | Keep only the stepwise components of a 'Signal' that begin at 0.
fromZero :: Signal a -> [(Time, Step a)]
fromZero (Signal a0 as) =
  case as of
    [] -> [(0, Step Nothing a0)]
    ((t, a) : as') ->
      case compare t 0 of
        LT ->
          set (ix 0 . _1) 0
            $ fmap snd
            $ dropWhile ((< 0) . fst . fst)
            $ zip (cycle as') as
        EQ -> as
        GT -> (0, a) : as


-- | Observe whether a computation would diverge, and if so, return 'Nothing'
-- instead. This can be used to guard otherwise-sketchy combinators which need
-- to fold over infinite event streams.
--
-- This is implemented by terminating after 10ms of trying.
terminating :: a -> Maybe a
terminating a = unsafePerformIO (timeout 10_000 (evaluate a))

