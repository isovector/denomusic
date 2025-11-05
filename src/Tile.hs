{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tile where

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
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance Semigroup (Tile a) where
  (<>) = (%%)

instance Monoid (Tile a) where
  mempty = delay 0

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
    -- TODO(sandy): should this be ll and rr?
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


