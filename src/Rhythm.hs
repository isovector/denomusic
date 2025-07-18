{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE ViewPatterns       #-}

module Rhythm where

import Data.Ratio
import qualified Data.Map as M
import Data.Function.Step (SF)
import Data.Function.Step qualified as SF
import Control.Monad (ap)
import Control.Arrow ((***), (&&&))
import Test.QuickCheck
import Test.QuickCheck.Checkers

data Rhythm a
  = Empty
  | Full a
  | Interval (SF Rational (Rhythm a))
  | Par (Rhythm a) (Rhythm a)
  deriving stock (Show, Functor, Foldable, Traversable)

mu :: Rhythm a -> Rational -> [a]
mu _ n | n >= 1 = []
mu _ n | n < 0 = []
mu Empty _ = []
mu (Full a) _ = pure a
mu (Interval as) n =
  let ((left, right), m) = findInterval n as
      width = right - left
   in mu m $ left / width
mu (Par a b) n = mu a n <> mu b n


getBoundTop :: SF.Bound a -> a
getBoundTop (SF.Open a) = a
getBoundTop (SF.Closed a) = a


findInterval :: Rational -> SF Rational a -> ((Rational, Rational), a)
findInterval x (SF.SF m def) =
  case (M.lookupLE (SF.Open x) m, M.lookupGE (SF.Closed x) m) of
    (Just (lo, _), Just (hi, v)) -> ((getBoundTop lo, getBoundTop hi), v)
    (Nothing, Just (hi, v)) -> ((0, getBoundTop hi), v)
    (Just (lo, _), Nothing) -> ((getBoundTop lo, 1), def)
    (Nothing, Nothing) -> ((0, 1), def)

intervals :: SF Rational a -> [((Rational, Rational), a)]
intervals (SF.SF m def) = go 0 $ M.toAscList $ M.mapKeysMonotonic getBoundTop m
  where
    go lo []
      | lo <= 1 = pure ((lo, 1), def)
      | otherwise = mempty
    go lo ((hi, a) : as) = ((lo, hi), a) : go hi as

data Durated a = Durated
  { getDuration :: Rational
  , getValue :: a
  }
  deriving stock (Eq, Ord, Show)


mapDuration :: (Rational -> Rational) -> Durated a -> Durated a
mapDuration f (Durated d a) = Durated (f d) a



foldInterval :: Rhythm a -> [(Rational, Durated a)]
foldInterval Empty = mempty
foldInterval (Full a) = pure (0, Durated 1 a)
foldInterval (Interval as) =
  flip foldMap (intervals as) $ \((lo, hi), r) ->
    fmap ((+ lo) . (* (hi - lo)) *** mapDuration (* (hi - lo))) $ foldInterval r

  --  in flip foldMap (zip [0..] ds) $ uncurry $ \ix d ->
  --       fmap ((+ sz * ix) . (* sz) *** mapDuration (* sz)) d
foldInterval (Par as bs) = foldInterval as <> foldInterval bs


joinNotes :: Eq a => [(Rational, Durated a)] -> [(Rational, Durated a)]
joinNotes [] = []
joinNotes [x] = [x]
joinNotes (n1@(o1, Durated d1 a1) : n2@(o2, Durated d2 a2) : xs)
  | o1 + d1 == o2
  , a1 == a2
  = joinNotes $ (o1, Durated (d1 + d2) a1) : xs
  | otherwise
  = n1 : joinNotes (n2 : xs)

overlay :: (a -> b -> c) -> Rhythm a -> Rhythm b -> Rhythm c
overlay _ Empty _ = Empty
overlay _ _ Empty = Empty
overlay f (Full a) b = fmap (f a) b
overlay f a (Full b) = fmap (flip f b) a
overlay f (Par a b) c = Par (overlay f a c) (overlay f b c)
overlay f a (Par b c) = Par (overlay f a b) (overlay f a c)
overlay f (Interval as) (Interval bs) = Interval $ liftA2 (overlay f) as bs

instance Applicative Rhythm where
  pure = Full
  (<*>) = ap

instance Monad Rhythm where
  return = pure
  Full a >>= f = f a
  Empty >>= _ = Empty
  Interval as >>= f = Interval $ fmap (>>= f) as
  Par as bs >>= f = Par (as >>= f) (bs >>= f)

instance (EqProp a) => EqProp (Rhythm a) where
  a =-= b = mu a =-= mu b

evenly :: [Rhythm a] -> Rhythm a
evenly [] = Empty
evenly [a] = a
evenly (init &&& last -> (as, def)) = Interval $
  flip SF.SF def $ M.fromAscList $ do
    let len = fromIntegral $ length as + 1
    (ix, a) <- zip [1..] as
    pure (SF.Open (ix % len), a)


instance Arbitrary a => Arbitrary (Rhythm a) where
  arbitrary = oneof
    [ pure Empty
    , fmap Full arbitrary
    , sized $ \sz -> do
        n <- arbitrary
        fmap evenly $ vectorOf n $ resize (sz `div` n) arbitrary
    ]

test :: Rhythm String
test = evenly [pure "a", pure "b", pure "c"]

test2 :: Rhythm String
test2 = evenly [pure "1", pure "2"]

type Music = Rhythm

-- test3 :: Music String
-- test3 = Interval $ replicate 6 $ pure "."

-- test4 :: Music String
-- test4 = Interval
--   [ pure "a"
--   , Interval $ fmap pure
--     [ "b"
--     , "b"
--     , "a"
--     ]
--   ]

-- test5 :: Music String
-- test5 = Interval $ fmap pure
--   [ "a."
--   , "a."
--   , "a."
--   , "b."
--   , "b."
--   , "a."
--   ]


-- _main :: IO ()
-- _main = quickCheck $ property $ overlay (<>) test4 test3 =-= test5
