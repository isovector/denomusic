{-# OPTIONS_GHC -Wno-orphans #-}

module Harness where

import Data.Foldable
import Test.Hspec
import Data.Containers.ListUtils (nubOrd)
import Data.Function (on)
import Data.List (sort)
import FRP.Types
import Test.QuickCheck hiding (sample)
import Test.QuickCheck.Checkers


instance Arbitrary Time where
  arbitrary = fmap Time arbitrary
  shrink = fmap Time . shrink . unTime

instance EqProp Time where
  (=-=) = (===)

instance CoArbitrary Time where
  coarbitrary = coarbitrary . unTime


instance CoArbitrary a => CoArbitrary (Event a) where
  coarbitrary = coarbitrary . eventToMaybe

instance Arbitrary a => Arbitrary (Event a) where
  arbitrary = oneof
    [ pure NoEvent
    , fmap Event arbitrary
    ]
  shrink = fmap (maybe NoEvent Event) . shrink . eventToMaybe


instance (Show a, Eq a) => EqProp (Event a) where
  (=-=) = on (===) eventToMaybe


genTimes :: Gen [Time]
genTimes = do
  t <- arbitrary
  ts <- listOf arbitrary
  pure $ sort $ nubOrd $ t : ts


instance Show a => Show (Signal a) where
  show = show . values

-- instance {-# OVERLAPPABLE #-} (Show a, Eq a) => EqProp (Signal a) where
--   s1 =-= s2 = conjoin
--     [ forAllShrinkShow arbitrary shrink show $ \t ->
--         sample s1 t === sample s2 t
--     , values s1 === values s2
--     ]


instance (Show a, Eq a) => EqProp (Signal (Event a)) where
  s1 =-= s2 = conjoin
    [ forAllShrinkShow arbitrary shrink show $ \t ->
        sample s1 t === sample s2 t
    , events s1 === events s2
    ]


instance (Show a, Eq a, Show b, Eq b) => EqProp (Signal (Event a, Event b)) where
  s1 =-= s2 = conjoin
    [ fmap fst s1 =-= fmap fst s2
    , fmap snd s1 =-= fmap snd s2
    ]


instance (Ord a, Arbitrary a) => Arbitrary (Signal (Event a)) where
  arbitrary = mkDiscrete <$> (zip <$> genTimes <*> arbitrary)
  shrink = fmap (mkDiscrete . sort . nubOrd) . shrink . events

instance (Arbitrary (Signal a), Arbitrary (Signal b)) => Arbitrary (Signal (a, b)) where
  arbitrary = liftA2 (,) <$> arbitrary <*> arbitrary
  shrink s = mconcat
    [ liftA2 (,) <$> shrink (fmap fst s) <*> pure (fmap snd s)
    , liftA2 (,) <$> pure (fmap fst s) <*> shrink (fmap snd s)
    ]


batchSpec :: (String, [Test]) -> Spec
batchSpec (n, ts) = describe n $ for_ ts $ uncurry it
