{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module FRP.Types
  ( module FRP.Types
  , Interval(..)
  ) where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad.Writer (Writer, runWriter)
import Data.Coerce
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


-- | A (possibly infinite) list of interesting times
newtype Clock = Clock { getClock :: [Time] }

-- | Merge two clocks, keeping them in ascending time order
instance Semigroup Clock where
  Clock [] <> Clock ys = Clock ys
  Clock (x : xs) <> Clock [] = Clock (x : xs)
  xx@(Clock (x : xs)) <> yy@(Clock (y : ys)) =
    case compare x y of
      LT -> Clock $ x : coerce (Clock xs <> yy)
      GT -> Clock $ y : coerce (xx <> Clock ys)
      EQ -> Clock $ x : coerce (Clock xs <> Clock ys)

instance Monoid Clock where
  mempty = Clock []

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

data Signal m a = Ord m => UnsafeSignal
  { clock  :: Clock
  , sample :: Time -> Writer (Set (Time, m)) a
  }

pattern Signal :: () => Ord m => Clock -> (Time -> Writer (Set (Time, m)) a) -> Signal m a
pattern Signal c f <- UnsafeSignal c f
  where
    Signal c f = UnsafeSignal c $ memo f
{-# COMPLETE Signal #-}

instance Functor (Signal m) where
  fmap f (Signal c g) = Signal c $ fmap (fmap f) g

instance Ord m => Applicative (Signal m) where
  pure = Signal mempty . pure . pure
  liftA2 f (Signal c1 a) (Signal c2 b) =
    Signal (c1 <> c2) $ liftA2 (liftA2 f) a b

newtype SF m a b = SF { runSF :: Signal m a -> Signal m b }
  deriving (Functor, Applicative) via WrappedArrow (SF m) a
  deriving (Semigroup, Monoid) via Ap (SF m a) b

instance Category (SF m) where
  id = SF id
  SF g . SF f = SF (g . f)

instance Arrow (SF m) where
  arr = SF . fmap
  SF f *** SF g = SF $ \sg@(Signal{}) ->
    liftA2 (,) (f $ fmap fst sg) (g $ fmap snd sg)


data Observation a = Observation
  { o_time :: Time
  , o_output :: a
  }
  deriving stock (Eq, Ord, Show, Functor)

observe :: Ord m => SF m () a -> [Observation (Set (Time, m), a)]
observe (SF f) = do
  let Signal (Clock clk) s = f $ pure ()
  t <- clk
  let (a, stuff) = runWriter (s t)
  pure $ Observation t (stuff, a)


export :: Ord m => (Time, Time) -> SF m () x -> [(Interval Time, Set m)]
export (lo, hi) s
  = mapMaybe (\o -> do
      let t = o_time o
      (d, _) <- S.lookupMin $ o_output o
      pure (Interval t (t + d), S.map snd $ o_output o)
        )
  $ fmap (fmap fst)
  $ takeWhile ((<= hi) . o_time)
  $ dropWhile ((< lo) . o_time)
  $ observe s


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

makeBaseFunctor ''Meter

