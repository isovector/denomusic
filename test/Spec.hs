module Spec where

import Data.Foldable
import Tile
import Data.Traversable
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck

instance (Show a, Ord a) => EqProp (Tile a) where
  (=-=) = (===)

main :: IO ()
main = hspec $ pure ()
