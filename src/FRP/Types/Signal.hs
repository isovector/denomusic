module FRP.Types.Signal
  ( Step(..)
  , Signal(..)
  , values
  , events
  , mkDiscrete
  , mkSteps
  ) where

import Data.Maybe
import FRP.Event
import Data.These
import Control.Applicative

type Time = Rational


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

-- | Get the discrete (closed-endpoint) values of a 'Signal'.
values :: Signal a -> [(Time, a)]
values (Signal _ as) = mapMaybe (traverse stepVal) as


-- | Get the eventful values out of a 'Signal'.
events :: Signal (Event a) -> [(Time, a)]
events = mapMaybe (traverse eventToMaybe) . values


-- | Build a 'Signal' out of discrete values.
mkDiscrete :: [(Time, a)] -> Signal (Event a)
mkDiscrete = Signal (const NoEvent) . fmap (fmap $ \a -> Step (Just (Event a)) (pure NoEvent))


-- | Build a 'Signal' as a stepwise function.
mkSteps :: a -> [(Time, a)] -> Signal a
mkSteps a = Signal (const a) . fmap (fmap $ Step Nothing . const)

