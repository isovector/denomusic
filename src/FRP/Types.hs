{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-orphans   #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module FRP.Types
  ( module FRP.Types
  , Interval(..)
  ) where

import Control.Monad.State (evalState, get, put, State)
import Data.These
import Control.Applicative (WrappedArrow(..), Alternative(..), asum)
import Control.Arrow
import Control.Category
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

data Signal a = UnsafeSignal
  { clock  :: Clock
  , sample :: Time -> a
  }

pattern Signal :: Clock -> (Time -> a) -> Signal a
pattern Signal c f <- UnsafeSignal c f
  where
    Signal c f = UnsafeSignal c $ memo f
{-# COMPLETE Signal #-}

instance Functor Signal where
  fmap f (Signal c g) = Signal c $ fmap f g

instance Applicative Signal where
  pure = Signal mempty . pure
  liftA2 f (Signal c1 a) (Signal c2 b) =
    Signal (c1 <> c2) $ liftA2 f a b

newtype SF a b = SF { runSF :: Signal a -> Signal b }
  deriving (Functor, Applicative) via WrappedArrow SF a
  deriving (Semigroup, Monoid) via Ap (SF a) b

instance Category (SF) where
  id = SF id
  SF g . SF f = SF (g . f)

instance Arrow SF where
  arr = SF . fmap
  SF f *** SF g = SF $ \sg@(Signal{}) ->
    liftA2 (,) (f $ fmap fst sg) (g $ fmap snd sg)


data Observation a = Observation
  { o_time :: Time
  , o_output :: a
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

observe :: SF () a -> [Observation a]
observe (SF f) = do
  let Signal (Clock clk) s = f $ pure ()
  t <- clk
  pure $ Observation t $ s t


newtype Notes a = Notes
  { getNotes :: Set (Time, a)
  }
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid)


export :: (Ord a) => (Time, Time) -> SF () (Event (Notes a)) -> [(Interval Time, Set a)]
export (lo, hi) s
  = mapMaybe (\o -> do
      let t = o_time o - lo
      (d, _) <- S.lookupMin $ getNotes $ o_output o
      pure (Interval t (t + d), S.map snd $ getNotes $ o_output o)
        )
  $ mapMaybe sequenceA
  $ fmap (fmap eventToMaybe)
  $ takeWhile ((< hi) . o_time)
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


data Sig2 a
  = Const a
  | Continuous (Time -> a)
  | Discrete (Time -> a) [(Time, a)]
  | Stepwise (Time -> a) [(Time, a)]
  deriving stock (Functor)

instance Applicative Sig2 where
  pure = Const
  liftA2 f (Const a) b = fmap (f a) b
  liftA2 f a (Const b) = fmap (flip f b) a
  liftA2 f (Continuous a) (Continuous b) = Continuous $ liftA2 f a b
  liftA2 f (Discrete a0 as) (Continuous b) = Discrete (liftA2 f a0 b) $ do
    (t, a) <- as
    pure (t, f a $ b t)
  liftA2 f (Continuous a) (Discrete b0 bs) = Discrete (liftA2 f a b0) $ do
    (t, b) <- bs
    pure (t, f (a t) b)
  liftA2 f (Stepwise a0 as) (Continuous b) = Stepwise (liftA2 f a0 b)  $ do
    (t, a) <- as
    pure (t, f a $ b t)
  liftA2 f (Continuous a) (Stepwise b0 bs) = Stepwise (liftA2 f a b0) $ do
    (t, b) <- bs
    pure (t, f (a t) b)
  liftA2 f (Discrete a0 as) (Discrete b0 bs) =
    Discrete (liftA2 f a0 b0) $ do
      (t, These a b) <- align as bs
      pure (t, f a b)
  liftA2 f (Stepwise a0 as) (Stepwise b0 bs) =
    Stepwise (liftA2 f a0 b0) $ do
      let at0 = listToMaybe $ fmap fst as
          bt0 = listToMaybe $ fmap fst bs
          t0 = fromMaybe 0 $ asum
                [ liftA2 min at0 bt0
                , at0
                , bt0
                ]
          xs = scanl
                 (\(_, (a, b)) (t, th) ->
                   case th of
                     This a' -> (t, (a', b))
                     That b' -> (t, (a, b'))
                     These a' b' -> (t, (a', b'))
                 ) (t0, (a0 t0, b0 t0)) $ align as bs
      fmap (fmap $ uncurry f) xs
  liftA2 f (Discrete a0 as) (Stepwise b0 bs) =
    Discrete (liftA2 f a0 b0) $ do
      let mat0 = listToMaybe $ fmap fst as
          mbt0 = listToMaybe $ fmap fst bs
          b0' =
            case (mat0, mbt0) of
              (Nothing, _) -> error "impossible: unused"
              (Just at0, Nothing) -> b0 at0
              (Just at0, Just bt0) ->
                case compare at0 bt0 of
                  LT -> b0 at0
                  GT -> snd $ head bs
                  EQ -> snd $ head bs
      flip evalState b0' $ flip foldMap (align as bs) $ uncurry $ \t -> \case
        This a -> do
          b <- get
          pure $ pure (t, f a b)
        That b -> do
          put b
          pure mempty
        These a b -> do
          put b
          pure $ pure (t, f a b)
  liftA2 f x@Stepwise{} y@Discrete{} = liftA2 (flip f) y x


newtype SF2 a b = SF2 { runSF2 :: Sig2 a -> Sig2 b }
  deriving (Functor, Applicative) via WrappedArrow SF2 a
  deriving (Semigroup, Monoid) via Ap (SF2 a) b

instance Category (SF2) where
  id = SF2 id
  SF2 g . SF2 f = SF2 (g . f)

instance Arrow SF2 where
  arr = SF2 . fmap
  SF2 f *** SF2 g = SF2 $ \sg ->
    liftA2 (,) (f $ fmap fst sg) (g $ fmap snd sg)

ev2ev :: ([a] -> [b]) -> SF2 (Event a) (Event b)
ev2ev f = SF2 $
  \case
    Discrete _ as -> Discrete (const NoEvent) $ fmap (id *** _) as


deriving via Ap (State s) a instance Semigroup a => Semigroup (State s a)
deriving via Ap (State s) a instance Monoid a => Monoid (State s a)


align :: Ord a => [(a, b)] -> [(a, c)] -> [(a, These b c)]
align [] ys       = fmap (fmap That) ys
align (x : xs) [] = fmap (fmap This) (x : xs)
align xx@((tx, x) : xs) yy@((ty, y) : ys) =
  case compare tx ty of
    LT -> (tx, This x) : align xs yy
    GT -> (ty, That y) : align xx ys
    EQ -> (tx, These x y) : align xs ys


-- sample2 :: Sig2 a -> Time -> a
-- sample2 (Const k) = pure k
-- sample2 (Continuous f) = f
-- sample2 (Discrete as) = fromMaybe undefined . flip lookup as
-- sample2 (Stepwise as) = \t -> maybe undefined snd $ listToMaybe $ reverse $ takeWhile ((<= t) . fst) as

