{-# OPTIONS_GHC -fno-warn-orphans #-}

module DenoMusic.Generators where

import Control.Applicative
import DenoMusic.Types
import DenoMusic.Utils
import Test.QuickCheck (Arbitrary(..), oneof, resize, sized, Positive(..), (.&&.))
import Test.QuickCheck.Checkers (EqProp(..))
import Data.Map.Monoidal qualified as MM

instance (EqProp k, EqProp a) => EqProp (MM.MonoidalMap k a) where
  v1 =-= v2 = MM.toList v1 =-= MM.toList v2


instance EqProp a => EqProp (Voice a) where
  v1 =-= v2 = sample v1 =-= sample v2

instance (Show v, Arbitrary v, EqProp v, EqProp a) => EqProp (Music v a) where
  Music d1 m1 =-= Music d2 m2 = (d1 =-= d2) .&&. (m1 =-= m2)

instance (Arbitrary a, Semigroup a) => Arbitrary (Voice a) where
  arbitrary = Voice <$> arbitrary
  shrink (Voice a) = mconcat
    [ Voice <$> shrink a
    ]

instance (Ord v, Arbitrary v, Arbitrary a, Enum v, Bounded v, Semigroup a) => Arbitrary (Music v a) where
  arbitrary =
    sized $ \n -> oneof $
      case n <= 0 of
        True -> small
        False ->
          [ (##) <$> resize (div n 2) arbitrary <*> resize (div n 2) arbitrary
          , (<>) <$> resize (div n 2) arbitrary <*> resize (div n 2) arbitrary
          ] <> small
      where
        small =
          [ pure <$> arbitrary
          , pure empty
          ]
