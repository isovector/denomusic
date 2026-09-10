{-# OPTIONS_GHC -Wno-orphans #-}

module FRP.Clock where

import Data.Maybe
import Data.MemoTrie
import Data.Ratio
import Data.Bool

type Time = Rational

instance HasTrie Rational where
  data Rational :->: x = RationalTrie (Integer :->: (Integer :->: x))
  trie f = RationalTrie $ trie $ trie . \n d -> f (n % d)
  untrie (RationalTrie x) = (\f r -> f (numerator r) (denominator r)) (untrie . untrie x)
  enumerate = error "no enumerate for Rational"

data Clock
  = Never
  | At Time
  | Every Rational
  | Offset Rational Clock
  | Union Clock Clock
  | Intersect Clock Clock
  | Take Int Clock
  | Drop Int Clock
  deriving stock (Eq, Ord, Show)


toTimes :: Clock -> [Time]
toTimes Never         = []
toTimes (At t)        = [t]
toTimes (Every dt)    = iterate (+ dt) 0
toTimes (Offset dt c) = fmap (+ dt) $ toTimes c
toTimes (Union c1 c2) = go (toTimes c1) (toTimes c2)
  where
    go [] ys = ys
    go (x : xs) [] = x : xs
    go xx@(x : xs) yy@(y : ys) =
      case compare x y of
        LT -> x : go xs yy
        GT -> y : go xx ys
        EQ -> x : go xs ys
toTimes (Intersect c1 c2) = go (toTimes c1) (toTimes c2)
  where
    go [] _ = []
    go _ [] = []
    go xx@(x : xs) yy@(y : ys) =
      case compare x y of
        LT -> go xs yy
        GT -> go xx ys
        EQ -> x : go xs ys
toTimes (Take n c) = take n $ toTimes c
toTimes (Drop n c) = drop n $ toTimes c


check :: Clock -> Time -> Bool
check Never             = const False
check (At t0)           = \t -> t == t0
check (Every dt)        = \t -> denominator (t / dt) == 1
check (Offset dt c)     = \t -> check c $ t - dt
check (Union c1 c2)     = \t -> check c1 t || check c2 t
check (Intersect c1 c2) = \t -> check c1 t && check c2 t
check (Take n c)        = do
  let ts = reverse $ take n $ toTimes c
  case listToMaybe ts of
    Nothing -> const False
    Just tx -> \t -> t <= tx && check c t
check (Drop n c)        =
  case listToMaybe $ drop n $ toTimes c of
    Nothing -> const False
    Just t0 -> \t -> t0 <= t && check c t


latest :: Clock -> Time -> Rational
latest Never             = const (-9000)
latest (At t0)           = \t -> bool 0 t0 $ t >= t0
latest (Every dt)        = \t -> fromIntegral (floor (t / dt)) * dt
latest (Offset dt c)     = \t -> latest c t + dt
latest (Union c1 c2)     = \t -> max (latest c1 t) (latest c2 t)
latest (Intersect c1 c2) = \t -> min (latest c1 t) (latest c2 t)
latest (Take n c)        =
  case listToMaybe $ reverse $ take n $ toTimes c of
    Nothing -> const (-9001)
    Just tx -> \t -> min tx (latest c t)
latest (Drop n c)        =
  case listToMaybe $ drop n $ toTimes c of
    Nothing -> const (-9002)
    Just t0 -> \t -> bool 0 (latest c t) $ t0 <= t


bucket :: Clock -> (Time -> a) -> (Time -> a)
bucket c f =
  let getlatest = latest c
   in memo (f . getlatest) . getlatest

