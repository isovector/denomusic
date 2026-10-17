{-# OPTIONS_GHC -Wno-orphans #-}

module SignalSpec where

import Data.Foldable
import Data.Function (on)
import Data.List (sort)
import Data.Containers.ListUtils (nubOrd)
import Test.Hspec
import Test.QuickCheck.Classes
import Test.QuickCheck hiding (sample)
import Test.QuickCheck.Checkers
import FRP.Types

instance Arbitrary Time where
  arbitrary = fmap Time arbitrary
  shrink = fmap Time . shrink . unTime

instance EqProp Time where
  (=-=) = (===)

instance CoArbitrary a => CoArbitrary (Event a) where
  coarbitrary = coarbitrary . eventToMaybe

instance Arbitrary a => Arbitrary (Event a) where
  arbitrary = oneof
    [ pure NoEvent
    , fmap Event arbitrary
    ]

instance (Show a, Eq a) => EqProp (Event a) where
  (=-=) = on (===) eventToMaybe

instance (Show a, Eq a) => EqProp (Signal a) where
  s1 =-= s2 = conjoin
    [ forAllShrinkShow arbitrary shrink show $ \t ->
        sample s1 t === sample s2 t
    , values s1 === values s2
    ]

instance Show a => Show (Signal a) where
  show = show . values

genTimes :: Gen [Time]
genTimes = do
  t <- arbitrary
  ts <- listOf arbitrary
  pure $ sort $ nubOrd $ t : ts

instance {-# OVERLAPPABLE #-} Arbitrary a => Arbitrary (Signal a) where
  arbitrary = mkSteps <$> arbitrary <*> (zip <$> genTimes <*> arbitrary)

instance {-# OVERLAPPING #-} (Ord a, Arbitrary a) => Arbitrary (Signal (Event a)) where
  arbitrary = mkDiscrete <$> (zip <$> genTimes <*> arbitrary)
  shrink = fmap (mkDiscrete . sort . nubOrd) . shrink . events

spec :: Spec
spec = do
  batchSpec $ applicative (undefined @_ @(Signal (Event Int, Event Int, Event Int)))

batchSpec :: (String, [Test]) -> Spec
batchSpec (n, ts) = describe n $ for_ ts $ uncurry it

