module FRP.Types2 where

import Control.Applicative
import Control.Arrow
import Control.Category
import Data.Maybe
import Data.Monoid
import Data.These
import FRP.Event
import Prelude hiding (id, (.))

type Time = Rational

data Step a = Step
  { stepVal :: Maybe a
  , step :: Time -> a
  }
  deriving stock Functor

combine :: Time -> (a -> b -> c) -> Step a -> Step b -> Step c
combine t f (Step a fa) (Step b fb) =
  Step
    (asum
      [ liftA2 f a b
      , fmap (f (fa t)) b
      , fmap (flip f (fb t)) a
      ])
    (liftA2 f fa fb)


data Signal a = Signal (Time -> a) [(Time, Step a)]
  deriving stock Functor

instance Applicative Signal where
  pure a = Signal (pure a) mempty
  liftA2 f (Signal a as) (Signal b bs) =
    Signal (liftA2 f a b)
      $ fmap (uncurry $ \t -> (t,) . uncurry (combine t f))
      $ joining (Step Nothing a) (Step Nothing b) as bs


merge :: Ord a => [(a, b)] -> [(a, c)] -> [(a, These b c)]
merge [] ys       = fmap (fmap That) ys
merge (x : xs) [] = fmap (fmap This) (x : xs)
merge xx@((tx, x) : xs) yy@((ty, y) : ys) =
  case compare tx ty of
    LT -> (tx, This x) : merge xs yy
    GT -> (ty, That y) : merge xx ys
    EQ -> (tx, These x y) : merge xs ys

joining :: Ord a => b -> c -> [(a, b)] -> [(a, c)] -> [(a, (b, c))]
joining a0 b0 as bs =
  drop 1 $ scanl
    (\(_, (a, b)) (t, th) ->
      case th of
        This a' -> (t, (a', b))
        That b' -> (t, (a, b'))
        These a' b' -> (t, (a', b'))
    ) (undefined, (a0, b0)) $ merge as bs


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

values :: Signal a -> [(Time, a)]
values (Signal _ as) = mapMaybe (traverse stepVal) as

events :: Signal (Event a) -> [(Time, a)]
events = mapMaybe (traverse eventToMaybe) . values

discrete :: [(Time, a)] -> SF x (Event a)
discrete = SF . const . mkDiscrete

mkDiscrete :: [(Time, a)] -> Signal (Event a)
mkDiscrete = Signal (const NoEvent) . fmap (fmap $ \a -> Step (Just (Event a)) (pure NoEvent))

steps :: a -> [(Time, a)] -> SF x a
steps a = SF . const . mkSteps a

mkSteps :: a -> [(Time, a)] -> Signal a
mkSteps a = Signal (const a) . fmap (fmap $ Step Nothing . const)

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

