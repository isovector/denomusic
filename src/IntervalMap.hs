{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}

module IntervalMap where

import Control.Arrow ((***))
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data IntervalMap a
  = Empty
  | Full a
  | Interval [IntervalMap a]
  deriving stock (Show, Functor, Foldable, Traversable)

instance Semigroup (IntervalMap a) where
  Interval xs <> Interval ys = Interval $ xs <> ys
  Interval xs <> y = Interval $ xs <> pure y
  x <> Interval ys = Interval $ x : ys
  x <> y = Interval [x, y]

instance Monoid (IntervalMap a) where
  mempty = Interval []

mu :: IntervalMap a -> Rational -> Maybe a
mu Empty _ = Nothing
mu (Full a) _ = Just a
mu (Interval []) _ = Nothing
mu _ n | n >= 1 = Nothing
mu _ n | n < 0 = Nothing
mu (Interval as) n =
  let sz = length as
      width = recip $ fromIntegral sz
      offset = floor $ n * fromIntegral sz
      left = n - fromIntegral offset
   in mu (as !! offset) $ left / width


data Durated a = Durated
  { getDuration :: Rational
  , getValue :: a
  }
  deriving stock (Eq, Ord, Show)


mapDuration :: (Rational -> Rational) -> Durated a -> Durated a
mapDuration f (Durated d a) = Durated (f d) a



foldInterval :: IntervalMap a -> [(Rational, Durated a)]
foldInterval Empty = mempty
foldInterval (Full a) = pure (0, Durated 1 a)
foldInterval (Interval as) =
  let ds = fmap foldInterval as
      sz = recip $ fromIntegral $ length as
   in flip foldMap (zip [0..] ds) $ uncurry $ \ix d ->
        fmap ((+ sz * ix) . (* sz) *** mapDuration (* sz)) d


joinNotes :: Eq a => [(Rational, Durated a)] -> [(Rational, Durated a)]
joinNotes [] = []
joinNotes [x] = [x]
joinNotes (n1@(o1, Durated d1 a1) : n2@(o2, Durated d2 a2) : xs)
  | o1 + d1 == o2
  , a1 == a2
  = joinNotes $ (o1, Durated (d1 + d2) a1) : xs
  | otherwise
  = n1 : joinNotes (n2 : xs)


instance Applicative IntervalMap where
  pure = Full
  liftA2 _ Empty _ = Empty
  liftA2 _ _ Empty = Empty
  liftA2 f (Full a) (Full b) = Full $ f a b
  liftA2 f (Full a) (Interval b) = Interval $ fmap (fmap (f a)) b
  liftA2 f (Interval a) (Full b) = Interval $ fmap (fmap (flip f b)) a
  liftA2 f (Interval as) (Interval bs) =
    let al = length as
        bl = length bs
        sz = lcm al bl
     in Interval $ zipWith (liftA2 f)
          (replicate (div sz al) =<< as)
          (replicate (div sz bl) =<< bs)

instance Monad IntervalMap where
  return = pure
  Full a >>= f = f a
  Empty >>= _ = Empty
  Interval as >>= f = Interval $ fmap (>>= f) as

instance (EqProp a) => EqProp (IntervalMap a) where
  a =-= b = mu a =-= mu b

instance Arbitrary a => Arbitrary (IntervalMap a) where
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

test :: IntervalMap String
test = Interval [pure "a", pure "b", pure "c"]

test2 :: IntervalMap String
test2 = Interval [pure "1", pure "2"]

_main :: IO ()
_main = quickBatch $ monad @IntervalMap @Int @Int @Int undefined
