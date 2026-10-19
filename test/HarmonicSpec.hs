{-# OPTIONS_GHC -Wno-orphans #-}

module HarmonicSpec where

import Control.Lens (view)
import Music.Harmony
import Music.Types
import Test.Hspec
import Test.Hspec.QuickCheck
import Data.Semigroup
import Test.QuickCheck

spec :: Spec
spec = do
  describe "chordalSteps" $ do
    prop "should be invertible" $ \t ->
      invertible @24 @(Vertical Fiver) $ chordalSteps t

  describe "neo-riemannian transformations" $ do
    prop "should be an involution" $ \(v1 :: Quad) v2 ->
      involution @24 $ neoriemannian v1 v2

    prop "should be invertible" $ \(v1 :: Quad) v2 ->
      invertible @24 $ neoriemannian v1 v2

    it "should implement P" $
      applyVoiceLeading (neoriemannian Root Fifth) rootTriad ===
        Vertical (\case { Root -> 0; Third -> 3; Fifth -> 7})

    it "should implement R" $
      applyVoiceLeading (neoriemannian Root Third) rootTriad ===
        Vertical (\case { Root -> 0; Third -> 4; Fifth -> 9})

    it "should implement L" $
      applyVoiceLeading (neoriemannian Third Fifth) rootTriad ===
        Vertical (\case { Root -> -1; Third -> 4; Fifth -> 7})

    it "should follow the book example" $
      applyVoiceLeading (neoriemannian Oner Twoer) bookEx ===
        Vertical (\case { Oner -> -6; Twoer -> 1; Threer -> 10; Fourer -> 12; Fiver -> 17})

    it "should reflect around a repeated selected voice" $
      applyVoiceLeading (neoriemannian Root Root) rootTriad ===
        Vertical (\case { Root -> 0; Third -> -4; Fifth -> -7})

    prop "should preserve the chosen voices" $ \(c :: Vertical Quad (Note 24)) v1 v2 -> do
      let c' = applyVoiceLeading (neoriemannian v1 v2) c
      shouldBe
        (view (voice v1) c', view (voice v2) c')
        (view (voice v1) c,  view (voice v2) c )

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Vertical a b) where
  arbitrary = fmap Vertical arbitrary
  shrink = fmap Vertical . shrink . getVertical


rootTriad :: Vertical Triad (Note 12)
rootTriad = Vertical $ \case
  Root -> 0
  Third -> 4
  Fifth -> 7


bookEx :: Vertical Fiver (Note 12)
bookEx = Vertical $ \case
  Oner -> -6
  Twoer -> 1
  Threer -> 7
  Fourer -> 9
  Fiver -> 14


involution
  :: forall s f
   . (Show (f (Note s)), Eq (f (Note s)))
  => VoiceLeading f s
  -> f (Note s)
  -> Property
involution vl c =
  applyVoiceLeading (stimes 2 vl) c === c


invertible
  :: forall s f
   . (Eq (f (Note s)), Show (f (Note s)))
  => VoiceLeading f s
  -> f (Note s)
  -> Property
invertible vl c =
  conjoin
    [ applyVoiceLeading (vl <> invert vl) c === c
    , applyVoiceLeading (invert vl <> vl) c === c
    ]


instance Arbitrary Dyad where
  arbitrary = elements $ enumFromTo minBound maxBound

instance CoArbitrary Dyad where
  coarbitrary = coarbitrary . fromEnum

instance Arbitrary Triad where
  arbitrary = elements $ enumFromTo minBound maxBound

instance CoArbitrary Triad where
  coarbitrary = coarbitrary . fromEnum

instance Arbitrary Quad where
  arbitrary = elements $ enumFromTo minBound maxBound

instance CoArbitrary Quad where
  coarbitrary = coarbitrary . fromEnum

instance Arbitrary Fiver where
  arbitrary = elements $ enumFromTo minBound maxBound

instance CoArbitrary Fiver where
  coarbitrary = coarbitrary . fromEnum

instance Arbitrary (Note n) where
  arbitrary = fmap (flip Note [] . Deg) arbitrary
  shrink = fmap (flip Note [] . Deg) . shrink . getDeg . noteDeg
