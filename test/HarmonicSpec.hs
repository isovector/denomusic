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
      invertible @24 @(Vertical 5) $ chordalSteps t

  describe "neo-riemannian transformations" $ do
    prop "should be an involution" $ \(v1 :: Fin 4) v2 ->
      involution @24 $ neoriemannian v1 v2

    prop "should be invertible" $ \(v1 :: Fin 4) v2 ->
      invertible @24 $ neoriemannian v1 v2

    it "should implement P" $
      applyVoiceLeading (neoriemannian 0 2) rootTriad ===
        (0 :> 3 :> 7 :> Nil)

    it "should implement R" $
      applyVoiceLeading (neoriemannian 0 1) rootTriad ===
        (0 :> 4 :> 9 :> Nil)

    it "should implement L" $
      applyVoiceLeading (neoriemannian 1 2) rootTriad ===
        (-1 :>4 :> 7 :> Nil)

    it "should follow the book example" $
      applyVoiceLeading (neoriemannian 0 1) bookEx ===
        (-6 :> 1 :> 10 :> 12 :> 17 :> Nil)

    it "should reflect around a repeated selected voice" $
      applyVoiceLeading (neoriemannian 0 0) rootTriad ===
        (0 :> -4 :> -7 :> Nil)

    prop "should preserve the chosen voices" $ \(c :: Vertical 4 (Note 24)) v1 v2 -> do
      let c' = applyVoiceLeading (neoriemannian v1 v2) c
      shouldBe
        (view (voice v1) c', view (voice v2) c')
        (view (voice v1) c,  view (voice v2) c )


rootTriad :: Vertical 3 (Note 12)
rootTriad = 0 :> 4 :> 7 :> Nil


bookEx :: Vertical 5 (Note 12)
bookEx = -6 :> 1 :> 7 :> 9 :> 14 :> Nil


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

instance Arbitrary (Note n) where
  arbitrary = fmap (flip Note [] . Deg) arbitrary
  shrink = fmap (flip Note [] . Deg) . shrink . getDeg . noteDeg

