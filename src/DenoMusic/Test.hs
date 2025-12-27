{-# LANGUAGE AllowAmbiguousTypes              #-}
{-# LANGUAGE PartialTypeSignatures            #-}
{-# OPTIONS -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans             #-}

module DenoMusic.Test where

import Control.Applicative
import Data.Foldable
import Data.Set qualified as S
import Data.Word
import DenoMusic
import DenoMusic.Types
import DenoMusic.Utils
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


instance (Eq a, Show a) => EqProp (Set a) where
  s1 =-= s2 = S.toList s1 === S.toList s2

instance EqProp Word8 where
  (=-=) = (===)

data SATB = VS | VA | VT | VB
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance Arbitrary SATB where
  arbitrary = elements $ enumFromTo minBound maxBound

type S = Set PitchClass

instance Arbitrary PitchClass where
  arbitrary = elements $ enumFromTo minBound maxBound

instance EqProp PitchClass where
  (=-=) = (===)

instance CoArbitrary PitchClass where
  coarbitrary x = coarbitrary (fromEnum x)

classBatch
  :: forall p
   . ( Alternative p
     , Monoid (p S)
     , _
     )
  => Spec
classBatch = do
  for_
    [ semigroup   @(p S) @Int     undefined
    , monoid      @(p S)          undefined
    , functor     @p     @S @S @S undefined
    , applicative @p     @S @S @S undefined
    , alternative @p     @S       undefined
    ] $ \(group, props) -> do
          describe group $ do
            for_ props $ \(test, p) -> do
              prop test p

main :: IO ()
main = hspec $ do
  describe "Voice" $ do
    classBatch @Voice
    prop "uncurry (##) . separate = id" $ \(m :: Voice S) t -> do
      let (l, r) = separateV t m
      l ##. r =-= m
  describe "Music" $ do
    classBatch @(Music SATB)
