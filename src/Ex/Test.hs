{-# LANGUAGE DeriveAnyClass #-}

module Ex.Test where

import Data.Set qualified as S
import DenoMusic.Play
import GHC.Generics
import Control.Lens
import Data.Kind
import GHC.TypeLits
import FRP
import Music.Types
import Music.Extra
import DenoMusic.Harmony
import Control.Applicative (ZipList(..))
import Euterpea qualified as E
import Control.DeepSeq (NFData)


dyad :: Scale 2 3
dyad = mkScale [0, 1]


type V :: Nat -> Type -> Type
newtype V n a = V
  { unV :: [a]
  }
  deriving stock (Functor, Foldable, Traversable, Generic)
  deriving newtype (Eq, Ord, Show)
  deriving anyclass NFData

instance (E.ToMusic1 a) => E.ToMusic1 (V n a) where
  toMusic1 =
    E.mFold
      (\case
        E.Note dur v ->
          foldr
            (\a m -> m E.:=: E.toMusic1 (E.Prim (E.Note dur a)))
            (E.Prim $ E.Rest 0)
            (unV v)
        E.Rest dur -> E.Prim $ E.Rest dur
      )
      (E.:+:)
      (E.:=:)
      E.Modify

instance KnownNat n => Applicative (V n) where
  pure = V . replicate (scaleSize @n)
  liftA2 f (V a) (V b) = V $ getZipList $ liftA2 f (ZipList a) (ZipList b)


iota :: forall n. KnownNat n => V n (Note n)
iota = V $ do
  i <- [0 .. scaleSize @n - 1]
  pure $ fromIntegral i


type Voice :: Nat -> Type
data Voice n = KnownNat n => UnsafeVoice
  { unVoice :: Int
  }

deriving stock instance Eq (Voice n)
deriving stock instance Ord (Voice n)

instance KnownNat n => Enum (Voice n) where
  toEnum = UnsafeVoice
  fromEnum = unVoice

instance KnownNat n => Bounded (Voice n) where
  minBound = 0
  maxBound = -1

instance Show (Voice n) where
  showsPrec n (UnsafeVoice v) = showsPrec n v

instance KnownNat n => Num (Voice n) where
  UnsafeVoice a + UnsafeVoice b = UnsafeVoice $ mod (a + b) (scaleSize @n)
  UnsafeVoice a - UnsafeVoice b = UnsafeVoice $ mod (a - b) (scaleSize @n)
  UnsafeVoice a * UnsafeVoice b = UnsafeVoice $ mod (a * b) (scaleSize @n)
  abs = id
  signum = error "signum @Voice"
  fromInteger = UnsafeVoice . flip mod (scaleSize @n) . fromInteger

voice :: Voice n -> Lens' (V n a) a
voice (UnsafeVoice n) = lens ((!! n) . unV) (\(V as) a -> V $ set (ix n) a as)


exchangeToward :: forall n m. KnownNat m => Voice n -> Voice n -> V n (Note m) -> V n (Note m)
exchangeToward x y v =
  let ax = (getDeg $ noteDeg $ view (voice x) v, x)
      by = (getDeg $ noteDeg $ view (voice y) v, y)
      (lo, src) = min ax by
      (hi, dst) = max ax by
      d = mod (hi - lo) $ scaleSize @m
   in v & voice src +~ fromIntegral d
        & voice dst -~ fromIntegral d


exchangeAway :: forall n m. KnownNat m => Voice n -> Voice n -> V n (Note m) -> V n (Note m)
exchangeAway x y v =
  let ax = (getDeg $ noteDeg $ view (voice x) v, x)
      by = (getDeg $ noteDeg $ view (voice y) v, y)
      (lo, src) = min ax by
      (hi, dst) = max ax by
      d = scaleSize @m - mod (hi - lo) (scaleSize @m)
   in v & voice src -~ fromIntegral d
        & voice dst +~ fromIntegral d


song :: SF () (Event (Notes (V 2 (Note 12))))
song = proc _ -> do
  t <- hold mempty <<< accum mempty <<< every 1 (<> lead @3 @7 0 4) -< ()
  b <- rhythm [1/3, 2/3] (<> lead @2 @3 0 2) -< ()
  d <- hold mempty <<< accum mempty -< b
  returnA -< flip fmap (1/3 <$ b) $ \x -> Notes $ S.singleton (x, fmap (fmap (+ 60) $ applyScale $ apply d dyad >>> apply t triad >>> diatonic) $ iota @2)

main :: IO ()
main = play $ export (0, 12) song


