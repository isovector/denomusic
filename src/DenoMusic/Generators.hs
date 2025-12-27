{-# OPTIONS_GHC -fno-warn-orphans #-}

module DenoMusic.Generators where

import Control.Applicative
import DenoMusic.Types
import DenoMusic.Utils
import Test.QuickCheck (Arbitrary(..), oneof, resize, sized)
import Test.QuickCheck.Checkers (EqProp(..))

instance EqProp a => EqProp (Voice a) where
  v1 =-= v2 = sample v1 =-= sample v2

instance (Show v, Arbitrary v, EqProp a) => EqProp (Music v a) where
  Music m1 =-= Music m2 = m1 =-= m2

instance (Arbitrary a, Semigroup a) => Arbitrary (Voice a) where
  arbitrary =
    sized $ \n -> oneof $
      case n <= 0 of
        True -> small
        False ->
          [ (##.) <$> resize (div n 2) arbitrary <*> resize (div n 2) arbitrary
          , delayV <$> arbitrary <*> resize (n - 1) arbitrary
          ] <> small
      where
        small =
          [ noteV <$> arbitrary <*> arbitrary
          , restV <$> arbitrary
          , pure <$> arbitrary
          ]
  shrink (Drone a) = mconcat
    [ Drone <$> shrink a
    , pure Empty
    ]
  shrink Empty = []
  shrink (Voice d a) = mconcat
    [ Voice <$> shrink d <*> pure a
    , Voice <$> pure d <*> shrink a
    , pure Empty
    ]

instance (Ord v, Arbitrary v, Arbitrary a, Semigroup a) => Arbitrary (Music v a) where
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
          [ voiceV <$> arbitrary <*> arbitrary
          , pure <$> arbitrary
          , pure empty
          ]
