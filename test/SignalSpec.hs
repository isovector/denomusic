{-# OPTIONS_GHC -Wno-orphans #-}

module SignalSpec where

import FRP.Types
import Harness
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Test.QuickCheck hiding (sample)


spec :: Spec
spec = do
  describe "steps" $ do
    batchSpec $ applicative (undefined @_ @(Signal (Int, Int, Int)))
    prop "zipPrjA" $
      \(a :: Signal Int) (b :: Signal Int) ->
        fmap fst (liftA2 (,) a b) =-= a
    prop "zipPrjB" $
      \(a :: Signal Int) (b :: Signal Int) ->
        fmap snd (liftA2 (,) a b) =-= b
  describe "discrete" $ do
    batchSpec $ applicative (undefined @_ @(Signal (Event Int, Event Int, Event Int)))
    prop "zipPrjA" $
      \(a :: Signal (Event Int)) (b :: Signal (Event Int)) ->
        fmap fst (liftA2 (,) a b) =-= a
    prop "zipPrjB" $
      \(a :: Signal (Event Int)) (b :: Signal (Event Int)) ->
        fmap snd (liftA2 (,) a b) =-= b


instance {-# OVERLAPPABLE #-} (Show a, Eq a) => EqProp (Signal a) where
  s1 =-= s2 = conjoin
    [ forAllShrinkShow arbitrary shrink show $ \t ->
        sample s1 t === sample s2 t
    , values s1 === values s2
    ]


instance {-# OVERLAPPABLE #-} (Arbitrary a) => Arbitrary (Signal a) where
  arbitrary = mkSteps <$> arbitrary <*> (zip <$> genTimes <*> arbitrary)
  -- shrink = fmap (mkSteps . sort . nubOrd) . shrink . events
