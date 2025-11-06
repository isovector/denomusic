{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tile where

import Data.List (sort)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable
import Test.QuickCheck hiding (scale)
import Control.Monad
import Control.DeepSeq (NFData)
import Data.Traversable
import Control.Monad.State
import Euterpea (Music (..), rest)
import Euterpea qualified as E
import Data.Bifunctor
import Data.List (unfoldr)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Functor.Foldable
import Data.Functor.Foldable.TH

$(makeBaseFunctor [''E.Music])


data Tile a = Tile Rational (SHeap a)
  deriving stock (Show, Functor, Foldable, Traversable)

instance Ord a => Eq (Tile a) where
  t1 == t2 = duration t1 == duration t2 && flatten t1 == flatten t2

instance Semigroup (Tile a) where
  (<>) = (%%)

instance Monoid (Tile a) where
  mempty = delay 0


instance Applicative Tile where
  pure = tile 1
  (<*>) = ap

instance Monad Tile where
  Tile d Empty >>= _ = Tile d Empty
  Tile d (SHeap t es l r) >>= f =
    let es' = fmap (fmap f) es
        es'' = fmap (uncurry scaleTo) es'
        es''' = getSimul $ foldMap (Simul . co . re) es''
     in delay t <> es''' <> (Tile (d - t) (mergeSH l r) >>= f)


data SHeap a = Empty | SHeap Rational (Seq (Rational, a)) (SHeap a) (SHeap a)
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

scale :: Rational -> Tile a -> Tile a
scale m (Tile d sh)= Tile (d * m) $ scaleSH m sh

scaleTo :: Rational -> Tile a -> Tile a
scaleTo d t =
  let dur = duration t
   in scale (d / dur) t

scaleSH :: Rational -> SHeap a -> SHeap a
scaleSH m (SHeap t e l r) = SHeap (t * m) (fmap (first (* m)) e) (scaleSH m l) (scaleSH m r)
scaleSH _ Empty = Empty

(%%) :: Tile a -> Tile a -> Tile a
(%%) = tileProduct
infixr 5 %%

delay :: Rational -> Tile a
delay d = Tile d Empty

event :: a -> Tile a
event = tile 0

tile :: Rational -> a -> Tile a
tile r a = Tile r (SHeap 0 (Seq.singleton (r, a)) Empty Empty)

tileProduct :: Tile a -> Tile a -> Tile a
tileProduct (Tile d1 s1) (Tile d2 s2) = Tile (d1 + d2) (mergeSH s1 (shiftSH d1 s2))

mergeSH :: SHeap a -> SHeap a -> SHeap a
mergeSH l Empty = l
mergeSH Empty r = r
mergeSH heap1@(SHeap d1 e1 l1 r1) heap2@(SHeap d2 e2 l2 r2) =
  case compare d1 d2 of
    EQ -> SHeap d1 (e1 <> e2) (mergeSH l1 r1) (mergeSH l2 r2)
    LT -> SHeap d1 e1 (mergeSH r1 (shiftSH (-d1) heap2)) l1
    GT -> SHeap d2 e2 (mergeSH r2 (shiftSH (-d2) heap1)) l2

shiftSH :: Rational -> SHeap a -> SHeap a
shiftSH _ Empty = Empty
shiftSH d (SHeap d' e l r) = SHeap (d + d') e l r

duration :: Tile a -> Rational
duration (Tile d _) = d

data Events a = Events
  { e_events :: Seq (Rational, a)
  , e_at :: Rational
  }
  deriving stock (Show)

instance Ord a => Eq (Events a) where
  Events e1 a1 == Events e2 a2 = a1 == a2 && sort (nubOrd (toList e1)) == sort (nubOrd (toList e2))

-- TODO(sandy): broken; fails when the first event is negative and the start pos is more negative

-- splitT :: Tile a -> (Tile a, Tile a)
-- splitT (Tile d Empty)
--   | d < 0 = (Tile d Empty, mempty)
--   | otherwise = (mempty, Tile d Empty)
-- splitT t@(Tile d (SHeap i e l r))
--   | i < 0 =
--       let (x, y) = splitT $ Tile (d - i) (mergeSH l r)
--        in (Tile (d - duration y)(SHeap i e Empty Empty) <> x, y)
--   | otherwise = (mempty, t)

uncons :: Tile a -> Maybe (Events a, Tile a)
uncons (Tile _ Empty) = Nothing
uncons (Tile d (SHeap t e l r)) = Just (Events e t, Tile (d - t) (mergeSH l r))

flatten :: Tile a -> [Events a]
flatten = unfoldr uncons

inv :: Tile a -> Tile a
inv t = let d = duration t in delay (- d) %% t %% delay (- d)

re :: Tile a -> Tile a
re t = t %% delay (- (duration t))

co :: Tile a -> Tile a
co t = delay (- (duration t)) %% t

fork :: Tile a -> Tile a -> Tile a
fork a b = re a %% b

join :: Tile a -> Tile a -> Tile a
join a b = a %% co b


newtype Simul a = Simul { getSimul :: Tile a }

instance Semigroup (Simul a) where
  Simul a <> Simul b = Simul $ re a <> re b

instance Monoid (Simul a) where
  mempty = Simul mempty


simul :: [Tile a] -> Tile a
simul [] = mempty
simul as = foldr1 fork as

dumpRest :: E.Music a -> E.Music a
dumpRest = cata $ \case
  x :+:$ E.Prim (E.Rest 0) -> x
  E.Prim (E.Rest 0) :+:$ x -> x
  x :=:$ E.Prim (E.Rest 0) -> x
  E.Prim (E.Rest 0) :=:$ x -> x
  x -> embed x

toMusic :: Tile a -> Music a
toMusic t = dumpRest $ flip evalState 0 $ do
  es <- for (flatten t) $ \e -> do
    time <- get
    let now = time + e_at e
    put now
    pure $ rest now :+: foldr (\a b -> uncurry E.note a :=: b) (rest 0) (e_events e)
  pure $ foldr (:=:) (rest 0) es


playTile :: (E.ToMusic1 a, NFData a) => Tile a -> IO ()
playTile
  = E.playDev 2
  . toMusic


data MakeTile a
  = EmptyT
  | Event a
  | Delay Rational
  | TileT Rational a
  | Tensor (MakeTile a) (MakeTile a)
  deriving stock (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (MakeTile a) where
  arbitrary = sized $ \n ->
    case n <= 1 of
      True ->
        oneof
          [ pure EmptyT
          , Delay <$> arbitrary
          , Event <$> arbitrary
          , TileT <$> fmap getPositive arbitrary <*> arbitrary
          ]
      False ->
        oneof
          [ pure EmptyT
          , Delay <$> arbitrary
          , Event <$> arbitrary
          , TileT <$> fmap getPositive arbitrary <*> arbitrary
          , Tensor <$> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary
          ]
  shrink EmptyT = []
  shrink (Event a) = mconcat
    [ Event <$> shrink a
    , pure EmptyT
    ]
  shrink (Delay d) = mconcat
    [ Delay <$> shrink d
    , pure EmptyT
    ]
  shrink (TileT d a) = mconcat
    [ TileT <$> shrink d <*> pure a
    , TileT <$> pure d <*> shrink a
    , pure $ Event a
    , pure $ Delay d
    ]
  shrink (Tensor a b) = mconcat
    [ Tensor <$> shrink a <*> pure b
    , Tensor <$> pure a <*> shrink b
    , pure a
    , pure b
    ]

mkTile :: MakeTile a -> Tile a
mkTile EmptyT = mempty
mkTile (Event a) = event a
mkTile (Delay d) = delay d
mkTile (TileT d a) = tile d a
mkTile (Tensor a b) = mkTile a <> mkTile b





-- test =
--   quickCheck $ \(y :: MakeTile Char) ->
--     let x = mkTile y
--         (lo, hi) = splitT x in
--     x === lo <> hi

