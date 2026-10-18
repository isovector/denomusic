{-# OPTIONS_GHC -Wno-orphans #-}

module SignalSpec where

import FRP.Types
import Harness
import Test.Hspec
import Test.QuickCheck.Classes


spec :: Spec
spec = do
  batchSpec $ applicative (undefined @_ @(Signal (Event Int, Event Int, Event Int)))

