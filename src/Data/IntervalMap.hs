{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE ViewPatterns       #-}

module Data.IntervalMap
  ( IntervalMap (IntervalMap)
  , module Data.IntervalMap.FingerTree
  , greatestView
  , toList
  , dominatorsQuery
  , intersectionsQuery
  , searchQuery
  , Query (..)
  ) where

import GHC.Generics
import Control.DeepSeq
import Data.IntervalMap.FingerTree
import qualified Data.FingerTree as FT
import Data.FingerTree (FingerTree, Measured(..), ViewL(..), ViewR(..), (<|))
import Unsafe.Coerce


data Node v a = Node (Interval v) a
    deriving (Eq, Ord, Show, Read
        , Generic
        , NFData
        )

instance Functor (Node v) where
    fmap f (Node i x) = Node i (f x)

instance Foldable (Node v) where
    foldMap f (Node _ x) = f x

instance Traversable (Node v) where
    traverse f (Node i x) = Node i <$> f x

-- rightmost interval (including largest lower bound) and largest upper bound.
data IntInterval v = NoInterval | IntInterval (Interval v) v
    deriving (Generic, NFData)

instance Ord v => Semigroup (IntInterval v) where
    (<>) = intervalUnion

instance Ord v => Monoid (IntInterval v) where
    mempty = NoInterval

intervalUnion :: Ord v => IntInterval v -> IntInterval v -> IntInterval v
NoInterval `intervalUnion` i  = i
i `intervalUnion` NoInterval  = i
IntInterval _ hi1 `intervalUnion` IntInterval int2 hi2 =
    IntInterval int2 (max hi1 hi2)

instance (Ord v) => Measured (IntInterval v) (Node v a) where
    measure (Node i _) = IntInterval i (high i)

newtype MyIntervalMap v a = MyIntervalMap (FingerTree (IntInterval v) (Node v a))

pattern IntervalMap :: FingerTree (IntInterval v) (Node v a) -> IntervalMap v a
pattern IntervalMap ft <- (unsafeCoerce -> MyIntervalMap ft)
  where
    IntervalMap ft = unsafeCoerce $ MyIntervalMap ft
{-# COMPLETE IntervalMap #-}


-- | /O(1)/.  @'greatestView' m@ returns @'Nothing'@ if @m@ is empty, and
-- otherwise @'Just' ((i, x), m')@, where @i@ is the greatest interval,
-- @x@ is the associated value, and @m'@ is the rest of the map.
--
-- @since 0.1.3.0
greatestView :: Ord v =>
    IntervalMap v a -> Maybe ((Interval v, a), IntervalMap v a)
greatestView (IntervalMap t) = case FT.viewr t of
    EmptyR -> Nothing
    t' FT.:> Node i a -> Just ((i, a), IntervalMap t')


toList :: Ord v => IntervalMap v a -> [(Interval v, a)]
toList (IntervalMap im0) = go im0
  where
    go im =
      case FT.viewl im of
        EmptyL -> []
        Node i x :< im' -> (i, x) : go im'


data Query v a = Query
  { q_before :: IntervalMap v a
  , q_between :: IntervalMap v a
  , q_after :: IntervalMap v a
  }
  deriving stock Functor

-- | /O(k log (n/\//k))/.  All intervals that intersect with the given
-- interval, in lexicographical order.
intersectionsQuery :: (Ord v) => Interval v -> IntervalMap v a -> Query v a
intersectionsQuery i = inRangeQuery (low i) (high i)

-- | /O(k log (n/\//k))/.  All intervals that contain the given interval,
-- in lexicographical order.
dominatorsQuery :: (Ord v) => Interval v -> IntervalMap v a -> Query v a
dominatorsQuery i = inRangeQuery (high i) (low i)

-- | /O(k log (n/\//k))/.  All intervals that contain the given point,
-- in lexicographical order.
searchQuery :: (Ord v) => v -> IntervalMap v a -> Query v a
searchQuery p = inRangeQuery p p


-- | /O(k log (n/\//k))/.  All intervals that intersect with the given
-- interval, in lexicographical order.
inRangeQuery :: forall v a. (Ord v) => v -> v -> IntervalMap v a -> Query v a
inRangeQuery lo hi (IntervalMap t) =
  let (before, rest) = FT.split (greater hi) t
      (after, results) = partition (atleast lo) rest
   in Query (IntervalMap before) (IntervalMap results) (IntervalMap after)

partition :: Measured v a => (v -> Bool) -> FingerTree v a -> (FingerTree v a, FingerTree v a)
partition f ft = do
  let (pfalse, rest) = FT.split f ft
  case FT.viewl rest of
    EmptyL -> (FT.empty, FT.empty)
    a :< ft' -> do
      let (falses, trues) = partition f ft'
      (pfalse <> falses, a <| trues)


atleast :: (Ord v) => v -> IntInterval v -> Bool
atleast k (IntInterval _ hi) = k <= hi
atleast _ NoInterval = error "atleast NoInterval"

greater :: (Ord v) => v -> IntInterval v -> Bool
greater k (IntInterval i _) = low i > k
greater _ NoInterval = error "greater NoInterval"

