{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE ViewPatterns       #-}

module Rhythm where

import Data.List.Split (chunksOf)
import Control.Monad (ap)
import Control.Arrow ((***))
import Test.QuickCheck
import Test.QuickCheck.Checkers

data Rhythm a
  = Empty
  | Full a
  | Interval [Rhythm a]
  | Par (Rhythm a) (Rhythm a)
  deriving stock (Show, Functor, Foldable, Traversable)

instance Semigroup (Rhythm a) where
  Interval xs <> Interval ys = Interval $ xs <> ys
  Interval xs <> y = Interval $ xs <> pure y
  x <> Interval ys = Interval $ x : ys
  x <> y = Interval [x, y]

instance Monoid (Rhythm a) where
  mempty = Interval []

mu :: Rhythm a -> Rational -> [a]
mu Empty _ = []
mu (Full a) _ = pure a
mu (Interval []) _ = []
mu _ n | n >= 1 = []
mu _ n | n < 0 = []
mu (Interval as) n =
  let sz = length as
      width = recip $ fromIntegral sz
      offset = floor $ n * fromIntegral sz
      left = n - fromIntegral offset
   in mu (as !! offset) $ left / width
mu (Par a b) n = mu a n <> mu b n


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
  let ds = fmap foldInterval as
      sz = recip $ fromIntegral $ length as
   in flip foldMap (zip [0..] ds) $ uncurry $ \ix d ->
        fmap ((+ sz * ix) . (* sz) *** mapDuration (* sz)) d
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


instance Applicative Rhythm where
  pure = Full
  (<*>) = ap


overlay :: (a -> b -> c) -> Rhythm a -> Rhythm b -> Rhythm c
overlay _ Empty _ = Empty
overlay _ _ Empty = Empty
overlay f (Full a) b = fmap (f a) b
overlay f a (Full b) = fmap (flip f b) a
overlay f (Par a b) c = Par (overlay f a c) (overlay f b c)
overlay f a (Par b c) = Par (overlay f a b) (overlay f a c)
overlay f (Interval [a]) b = overlay f a b
overlay f a (Interval [b]) = overlay f a b
overlay f (Interval as) (Interval bs) = do
  let al = length as
      bl = length bs
      sz = lcm al bl
  case gcd al bl of
        1 ->
          Interval $ zipWith (overlay f)
              (replicate (div sz al) =<< as)
              (replicate (div sz bl) =<< bs)
        d -> do
          let !ax = div al d
              !bx = div bl d
          Interval $ zipWith ( \xs ys ->
            overlay f (Interval xs) (Interval ys)
            ) (chunksOf ax as) (chunksOf bx bs)


instance Monad Rhythm where
  return = pure
  Full a >>= f = f a
  Empty >>= _ = Empty
  Interval as >>= f = Interval $ fmap (>>= f) as
  Par as bs >>= f = Par (as >>= f) (bs >>= f)

instance (EqProp a) => EqProp (Rhythm a) where
  a =-= b = mu a =-= mu b

instance Arbitrary a => Arbitrary (Rhythm a) where
  arbitrary = oneof
    [ pure Empty
    , fmap Full arbitrary
    , sized $ \sz -> do
        n <- arbitrary
        case n of
          0 -> pure Empty
          1 -> fmap Full arbitrary
          _ -> fmap Interval $ vectorOf n $ resize (sz `div` n) arbitrary
    ]

test :: Rhythm String
test = Interval [pure "a", pure "b", pure "c"]

test2 :: Rhythm String
test2 = Interval [pure "1", pure "2"]

type Music = Rhythm

test3 :: Music String
test3 = Interval $ replicate 6 $ pure "."

test4 :: Music String
test4 = Interval
  [ pure "a"
  , Interval $ fmap pure
    [ "b"
    , "b"
    , "a"
    ]
  ]

test5 :: Music String
test5 = Interval $ fmap pure
  [ "a."
  , "a."
  , "a."
  , "b."
  , "b."
  , "a."
  ]


_main :: IO ()
_main = quickCheck $ property $ overlay (<>) test4 test3 =-= test5
