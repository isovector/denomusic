{-# LANGUAGE AllowAmbiguousTypes           #-}
{-# LANGUAGE TypeAbstractions              #-}
{-# LANGUAGE ViewPatterns                  #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Music.Harmony
  ( module Music.Harmony
  , KnownNat
  , Group(..)
  ) where

import Data.Functor
import Control.Lens ((+~), (&), view, (-~), (.~))
import Data.Bool
import Data.Fin (knownInduction, Induction)
import Data.Foldable
import Data.Functor.Compose
import Data.Iso
import Data.Monoid
import Data.Proxy
import Data.Ratio
import Data.Set qualified as S
import Data.Type.Ord
import GHC.TypeLits
import Music.Types.Deg
import Music.Types.Note
import Music.Types.Scale
import Music.Types.Vertical
import Prelude hiding (id, (.))


triad :: Scale 3 7
triad = mkScale $ S.fromList [0, 2, 4]

diatonic :: Scale 7 12
diatonic = mkScale $ S.fromList [0, 2, 4, 5, 7, 9, 11]


data T c s = T
  { intrinsicT :: Deg c
  , extrinsicT :: Deg s
  }
  deriving stock (Eq, Ord, Show)

instance Semigroup (T c s) where
  T a1 b1 <> T a2 b2 = T (a1 + a2) (b1 + b2)

instance Monoid (T c s) where
  mempty = T 0 0

instance Num (T c s) where
  T a1 b1 + T a2 b2 = T (a1 + a2) (b1 + b2)
  T a1 b1 - T a2 b2 = T (a1 - a2) (b1 - b2)
  T a1 b1 * T a2 b2 = T (a1 * a2) (b1 * b2)
  abs    (T a b) = T (abs a)    (abs b)
  signum (T a b) = T (signum a) (signum b)
  fromInteger n = T (fromInteger n) (fromInteger n)


lead :: forall c s. (KnownNat c, KnownNat s) => Deg s -> Deg s -> T c s
lead (Deg from) (Deg to) =
  let i = to - from
      c = scaleSize @c
      s = scaleSize @s

      angle_offset = c % s

      -- First wrap i to [0, s)
      i_wrapped = mod i s

      -- Calculate angular position as fraction of full rotation
      angle = fromIntegral i_wrapped * angle_offset
      fractional = angle - fromIntegral (truncate @_ @Int angle)

      -- Wrap to (-0.5, 0.5] to determine direction
      wrapped = bool id (subtract 1) (fractional > 0.5) fractional

      -- If moving counterclockwise (wrapped < 0), subtract scaleSize
      tLevel = bool id (subtract s) (wrapped < 0) i_wrapped

      -- Solve for sTrans to minimize voice leading
      sTrans = negate $ round (fromIntegral tLevel * angle_offset)
   in T (fromIntegral sTrans) $ fromIntegral tLevel


applyT :: T c s -> (Note c -> Note s) -> Note c -> Note s
applyT (T i e) sc n = sc (n + Note i mempty) + Note e mempty


intrinsic
  :: forall f c r
   . (Foldable f, Functor f, KnownNat c, 1 <= c)
  => f (Note c)
  -> (forall ins. Induction ins => Proxy ins -> (Note ins -> Note c) -> f (Note ins) -> r)
  -> r
intrinsic voiced k = do
  let pcSet = S.map normalizeNote $ S.fromList $ toList voiced
      pcs = S.toList pcSet
      n = length pcs
  withSomeSNat (fromIntegral n) $ \case
    Nothing -> error "impossble!"
    Just (SNat :: SNat ins) ->
      knownInduction @ins $
        k
          (Proxy @ins)
          (applyScaleMap $ (pcs !!) . fromIntegral)
          $ voiced <&> \x ->
            Note
              (Deg $ S.findIndex (normalizeNote x) pcSet + div (getDeg $ noteDeg x) (scaleSize @c) * n)
              mempty


newtype VoiceLeading f s = VoiceLeading
  { voiceLeadingAnIso :: AnIso' (f (Note s)) (f (Note s))
  }
  deriving (Semigroup, Monoid, Group) via Endomorphism (GenericIso (Compose f Note)) s

voiceLeadingIso :: VoiceLeading f s -> Iso' (f (Note s)) (f (Note s))
voiceLeadingIso v = cloneIso $ voiceLeadingAnIso v

applyVoiceLeading :: VoiceLeading f s -> f (Note s) -> f (Note s)
applyVoiceLeading v = view $ voiceLeadingIso v

instance (KnownNat v) => Show (VoiceLeading (Vertical v) s) where
  show vl =
    let v = iota
     in show $ liftA2 (\x y -> unwords [show x, "→", show y]) v (applyVoiceLeading vl v)

voiceLead
    :: (Foldable f, Functor f, KnownNat s, 1 <= s)
    => Deg s
    -> Deg s
    -> VoiceLeading f s
voiceLead from to = VoiceLeading $
  iso
    ( \voiced ->
      intrinsic voiced $ \(Proxy @ins) sc v' -> do
        let t = lead @ins from to
        fmap (applyT t sc) v'
    )
    ( \voiced ->
      intrinsic voiced $ \(Proxy @ins) sc v' -> do
        let t = lead @ins to from
        fmap (applyT t sc) v'
    )

-- TODO(sandy): It's not immediately clear to me whether the inverse direction here is correct
expand :: (KnownNat m) => Fin v -> Fin v -> VoiceLeading (Vertical v) m
expand src dst = VoiceLeading $ iso (expandF src dst) (contractF src dst)

contract :: (KnownNat m) => Fin v -> Fin v -> VoiceLeading (Vertical v) m
contract src dst = invert $ expand src dst


-- | A voice leading that swaps the two voices by moving each towards the
-- other.
contractF :: forall m v. (KnownNat m) => Fin v -> Fin v -> Vertical v (Note m) -> Vertical v (Note m)
contractF x y v =
  let ax = (getDeg $ noteDeg $ view (voice x) v, x)
      by = (getDeg $ noteDeg $ view (voice y) v, y)
      (lo, src) = min ax by
      (hi, dst) = max ax by
      d = mod (hi - lo) $ scaleSize @m
   in v & voice src +~ fromIntegral d
        & voice dst -~ fromIntegral d


-- | A voice leading that swaps the two voices by moving each away from the
-- other.
expandF :: forall m v. (KnownNat m) => Fin v -> Fin v -> Vertical v (Note m) -> Vertical v (Note m)
expandF x y v =
  let ax = (getDeg $ noteDeg $ view (voice x) v, x)
      by = (getDeg $ noteDeg $ view (voice y) v, y)
      (lo, src) = min ax by
      (hi, dst) = max ax by
      d = scaleSize @m - mod (hi - lo) (scaleSize @m)
   in v & voice src -~ fromIntegral d
        & voice dst +~ fromIntegral d


invertAround :: Rational -> Note n -> Note n
invertAround z (Note (Deg (fromIntegral -> d)) mods) =
  Note (Deg $ floor $ negate (d - z) + z) $ fmap negate mods


invertAround2
  :: Note n  -- ^ Fixed note 1
  -> Note n  -- ^ Fixed note 2
  -> Note n
  -> Note n
invertAround2 fp1 fp2 x = fp1 + fp2 - x


neoriemannian
  :: forall s v
   . (KnownNat v, KnownNat s, 1 <= s)
  => Fin v
  -> Fin v
  -> VoiceLeading (Vertical v) s
neoriemannian v1 v2 = VoiceLeading $ selfDual $ \v -> do
  let fp1 = view (voice v1) v
      fp2 = view (voice v2) v
  case normalizeNote fp1 == normalizeNote fp2 of
    True ->
      fmap (invertAround2 fp1 fp2) v
        & voice v1 .~ fp1
        & voice v2 .~ fp2
    False ->
      intrinsic v $ \(Proxy @ins) sc v' -> knownInduction @ins $ do
        let -- Reflect in chord-degree space so that the fixed points swap,
            -- then reflect in pitch space so they swap back. The composite
            -- fixes both chosen voices exactly and is monotone, so it maps
            -- the chord onto its inversion voice-by-voice.
            a = view (voice v1) v'
            b = view (voice v2) v'
        fmap (invertAround2 fp1 fp2 . sc . flip subtract (a + b)) v'


-- | The @t@ operation in "Tonality"
chordalSteps :: (Foldable f, Functor f, KnownNat s, 1 <= s) => Int -> VoiceLeading f s
chordalSteps n = VoiceLeading $
  iso
    ( \c -> intrinsic c $ \(Proxy @ins) sc c' ->
        fmap (sc . (+ fromIntegral n)) c'
    )
    ( \c -> intrinsic c $ \(Proxy @ins) sc c' ->
        fmap (sc . (subtract $ fromIntegral n)) c'
    )


-- | The @T@ operation in "Tonality"
scaleSteps :: (Functor f) => Int -> VoiceLeading f s
scaleSteps (fromIntegral -> n) = VoiceLeading $
  iso (fmap (+ n)) (fmap $ subtract n)

