{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-orphans   #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module FRP.Types
  ( module FRP.Types
  , Interval(..)
  ) where

import Debug.RecoverRTTI
import Debug.Trace
import Control.Monad.State (evalState, get, put, State)
import Data.These
import Control.Applicative (WrappedArrow(..), Alternative(..))
import Control.Arrow
import Control.Category
import Data.Functor
import Data.Functor.Foldable.TH
import Data.IntervalMap.FingerTree (Interval(..))
import Data.Maybe
import Data.MemoTrie
import Data.Monoid
import Data.Ratio
import Data.Set (Set)
import Data.Set qualified as S
import Prelude hiding (id, (.))


type Time = Rational

instance HasTrie Rational where
  data Rational :->: x = RationalTrie (Integer :->: (Integer :->: x))
  trie f = RationalTrie $ trie $ trie . \n d -> f (n % d)
  untrie (RationalTrie x) = (\f r -> f (numerator r) (denominator r)) (untrie . untrie x)
  enumerate = error "no enumerate for Rational"

newtype Event a = MkEvent
  { eventToMaybe :: Maybe a
  }
  deriving stock (Foldable, Traversable)
  deriving newtype (Functor, Applicative, Monad, Eq, Ord, Show, Alternative)

{-# COMPLETE Event, NoEvent #-}
pattern Event :: a -> Event a
pattern Event a = MkEvent (Just a)

pattern NoEvent :: Event a
pattern NoEvent = MkEvent Nothing

instance Semigroup a => Semigroup (Event a) where
  NoEvent <> a = a
  Event a <> NoEvent = Event a
  Event a <> Event b = Event (a <> b)

instance Semigroup a => Monoid (Event a) where
  mempty = NoEvent


data Observation a = Observation
  { o_time :: Time
  , o_output :: a
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

observe :: SF () a -> [Observation a]
observe (SF f) = do
  case f $ pure () of
    Discrete k as -> do
      (t, a) <- as
      pure $ Observation t $ k a
    _ -> mempty


newtype Notes a = Notes
  { getNotes :: Set (Time, a)
  }
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid)


export :: (Ord a) => (Time, Time) -> SF () (Event (Notes a)) -> [(Interval Time, Set a)]
export (lo, hi)
  = mapMaybe (\o -> do
      let t = o_time o - lo
      (d, _) <- S.lookupMin $ getNotes $ o_output o
      pure (Interval t (t + d), S.map snd $ getNotes $ o_output o)
        )
  . mapMaybe sequenceA
  . fmap (fmap eventToMaybe)
  . takeWhile ((< hi) . o_time)
  . dropWhile ((< lo) . o_time)
  . observe


data Beat = Beat
  { duration :: Time
  , stress :: Priority
  }
  deriving stock (Eq, Ord, Show)

newtype Priority = P Int
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum)

data Meter a
  = Pulse a
  | Group [Meter a]
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Applicative Meter where
  pure = Pulse
  liftA2 f (Pulse a) (Pulse b) = Pulse $ f a b
  liftA2 f (Group a) (Pulse b) = Group $ fmap (fmap $ flip f b) a
  liftA2 f (Pulse a) (Group b) = Group $ fmap (fmap $ f a) b
  liftA2 f (Group a) (Group b) = Group $ liftA2 (liftA2 f) a b

instance Monad Meter where
  Pulse a >>= f = f a
  Group as >>= f = Group $ fmap (>>= f) as


data Signal a where
  Const      :: a -> Signal a
  Continuous :: (Time -> a) -> Signal a
  Discrete   :: (b -> a) -> [(Time, b)] -> Signal a
  Stepwise   :: (Time -> b -> a) -> b -> [(Time, b)] -> Signal a

deriving stock instance Functor Signal


instance Applicative Signal where
  pure = Const
  liftA2 f (Const a) b = fmap (f a) b
  liftA2 f a (Const b) = fmap (flip f b) a
  liftA2 f (Continuous a) (Continuous b) = Continuous $ liftA2 f a b
  liftA2 f (Discrete k as) (Continuous b) = Discrete id $ do
    (t, a) <- as
    pure (t, f (k a) $ b t)
  liftA2 f a@Continuous{} b@Discrete{} = liftA2 (flip f) b a

  liftA2 f (Stepwise k a as) (Continuous b) = Stepwise (\t x -> f (k t x) (b t)) a as
  liftA2 f a@Continuous{} b@Stepwise{} = liftA2 (flip f) b a

  liftA2 f (Discrete ka as) (Stepwise kb b0 bs) =
    Discrete id $
      flip evalState b0 $
        flip foldMap (merge as bs) $ uncurry $ \t -> \case
          This a -> do
            b <- get
            pure $ pure (t, f (ka a) (kb t b))
          That b -> do
            put b
            pure mempty
          These a b -> do
            put b
            pure $ pure (t, f (ka a) (kb t b))
  liftA2 f x@Stepwise{} y@Discrete{} = liftA2 (flip f) y x

  liftA2 f (Discrete ka as) (Discrete kb bs) =
    Discrete
      ( \case
          This a -> f (ka a) undefined
          That b -> f undefined (kb b)
          These a b -> f (ka a) (kb b)
      ) $ merge as bs
      -- (t, These a b) <-
      -- pure (t, f (ka a) (kb b))
  liftA2 f (Stepwise ka a0 as) (Stepwise kb b0 bs) =
    Stepwise (\t (a, b) -> f (ka t a) (kb t b)) (a0, b0) $ do
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

ev2ev :: ([(Time, a)] -> [(Time, b)]) -> SF (Event a) (Event b)
ev2ev f = SF $
  \case
    Discrete f' as -> Discrete Event $ f $ traceWith anythingToString $ mapMaybe (traverse eventToMaybe . fmap f') as
    Const{} -> error "impossible"
    Continuous{} -> error "impossible"
    Stepwise{} -> error "impossible"


deriving via Ap (State s) a instance Semigroup a => Semigroup (State s a)
deriving via Ap (State s) a instance Monoid a => Monoid (State s a)


merge :: Ord a => [(a, b)] -> [(a, c)] -> [(a, These b c)]
merge [] ys       = fmap (fmap That) ys
merge (x : xs) [] = fmap (fmap This) (x : xs)
merge xx@((tx, x) : xs) yy@((ty, y) : ys) =
  case compare tx ty of
    LT -> (tx, This x) : merge xs yy
    GT -> (ty, That y) : merge xx ys
    EQ -> (tx, These x y) : merge xs ys


makeBaseFunctor ''Meter

