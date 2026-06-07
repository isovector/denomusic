{-# LANGUAGE DeriveAnyClass  #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IMusic where

import Data.Word
import Data.Foldable
import Data.List (sort)
import GHC.Generics
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck
import Data.Maybe (maybeToList)
import Data.IntervalMap.FingerTree (IntervalMap, Interval(..))
import Data.IntervalMap.FingerTree qualified as IM

data Bounds = NegInf | Bound Rational | PosInf
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass EqProp

instance Arbitrary Bounds where
  arbitrary = frequency
    [ (1, pure NegInf)
    , (1, pure PosInf)
    , (10, fmap Bound arbitrary)
    ]

deriving anyclass instance EqProp a => EqProp (Interval a)

instance Arbitrary (Interval Bounds) where
  arbitrary = do
    lo <- arbitrary `suchThat` (/= PosInf)
    hi <- arbitrary `suchThat` (> lo)
    pure $ Interval lo hi



newtype Music a = Music { unMusic :: IntervalMap Bounds a }
  deriving newtype (Functor, Show, Semigroup, Monoid)

everywhere :: Interval Bounds
everywhere = Interval NegInf PosInf

getIntervals :: IntervalMap Bounds a -> [(Interval Bounds, a)]
getIntervals = IM.intersections everywhere

getIntersection :: Ord a => Interval a -> Interval a -> Maybe (Interval a)
getIntersection (Interval a1 a2) (Interval b1 b2) = do
  let lo = max a1 b1
      hi = min a2 b2
  case lo < hi of
    True -> Just $ Interval lo hi
    False -> Nothing


instance Applicative Music where
  pure = Music . IM.singleton everywhere
  liftA2 f (Music ma) (Music mb) = Music $ mconcat $ do
    (ai, a) <- getIntervals ma
    (bi, b) <- IM.intersections ai mb
    i <- maybeToList $ getIntersection ai bi
    pure $ IM.singleton i $ f a b

instance (EqProp a, Ord a) => EqProp (Music a) where
  Music a =-= Music b = sort (getIntervals a) =-= sort (getIntervals b)

note :: Interval Bounds -> a -> Music a
note i = Music . IM.singleton i

target :: a -> Interval Bounds -> a -> Music a
target def i a = note everywhere def <> note i a

instance Arbitrary a => Arbitrary (Music a) where
  arbitrary = sized $ \n -> do
    let small =
          [ pure mempty
          , note <$> arbitrary <*> arbitrary
          , target <$> arbitrary <*> arbitrary <*> arbitrary
          ]
        large =
          [ mappend <$> decay 2 arbitrary <*> decay 2 arbitrary
          ]
    case n <= 1 of
      True -> oneof small
      False -> oneof $ small <> large

decay :: Int -> Gen a -> Gen a
decay n = scale (`div` n)

instance EqProp Word8 where
  (=-=) = (===)

crop :: Interval Bounds -> Music a -> Music a
crop i = Music . foldMap (uncurry IM.singleton) . IM.intersections i . unMusic

main :: IO ()
main = traverse_ (quickCheck . withMaxSuccess 10000 . snd) . snd $ applicative @Music @Int @Int @Int undefined

