{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeAbstractions    #-}
{-# LANGUAGE ViewPatterns        #-}

module Music.Harmony
  ( module Music.Harmony
  , KnownNat
  , Group(..)
  ) where

import Control.Lens ((+~), (&), view, (-~), (.~))
import Data.Bool
import Data.Foldable
import Data.Functor.Compose
import Data.IntMap qualified as IM
import Data.Iso
import Data.List (findIndex)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Proxy
import Data.Ratio
import Data.Set qualified as S
import GHC.TypeLits
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

apply :: T c s -> Scale c s -> Scale c s
apply (T (flip MkNote [] -> i) (flip MkNote [] -> o)) (Scale f) = Scale $
  iso (+ i) (subtract i) . f . iso (+ o) (subtract o)


intrinsic
  :: forall f c r
   . (Foldable f, Functor f, KnownNat c)
  => f (Note c)
  -> (forall ins. KnownNat ins => Proxy ins -> Scale ins c -> f (Note ins) -> r)
  -> r
intrinsic voiced k = do
  let notes = S.map normalizeNote $ S.fromList $ toList voiced
  withSomeSNat (fromIntegral $ length notes) $ \case
    Just (snat :: SNat ins) -> do
      withKnownNat snat $ do
        let sc = mkScale @ins notes
        k Proxy sc $ fmap (invertScale sc) voiced
    Nothing -> error "impossible"


newtype VoiceLeading f s = VoiceLeading
  { voiceLeadingAnIso :: AnIso' (f (Note s)) (f (Note s))
  }
  deriving (Semigroup, Monoid, Group) via Endomorphism (GenericIso (Compose f Note)) s

voiceLeadingIso :: VoiceLeading f s -> Iso' (f (Note s)) (f (Note s))
voiceLeadingIso v = cloneIso $ voiceLeadingAnIso v

applyVoiceLeading :: VoiceLeading f s -> f (Note s) -> f (Note s)
applyVoiceLeading v = view $ voiceLeadingIso v

instance (Enum v, Bounded v, Show v) => Show (VoiceLeading (Vertical v) s) where
  show vl =
    let v = iota
     in prettyVertical $ liftA2 (\x y -> unwords [show x, "→", show y]) v (applyVoiceLeading vl v)

voiceLead
    :: (Foldable f, Functor f, KnownNat s)
    => Deg s
    -> Deg s
    -> VoiceLeading f s
voiceLead from to = VoiceLeading $
  iso
    ( \voiced ->
      intrinsic voiced $ \(Proxy @ins) sc v' -> do
        let t = lead @ins from to
        fmap (applyScale $ apply t sc) v'
    )
    ( \voiced ->
      intrinsic voiced $ \(Proxy @ins) sc v' -> do
        let t = lead @ins to from
        fmap (applyScale $ apply t sc) v'
    )

-- TODO(sandy): It's not immediately clear to me whether the inverse direction here is correct
expand :: (KnownNat m, Ord v) => v -> v -> VoiceLeading (Vertical v) m
expand src dst = VoiceLeading $ iso (expandF src dst) (contractF src dst)

contract :: (KnownNat m, Ord v) => v -> v -> VoiceLeading (Vertical v) m
contract src dst = invert $ expand src dst


-- | A voice leading that swaps the two voices by moving each towards the
-- other.
contractF :: forall m v. (KnownNat m, Ord v) => v -> v -> Vertical v (Note m) -> Vertical v (Note m)
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
expandF :: forall m v. (KnownNat m, Ord v) => v -> v -> Vertical v (Note m) -> Vertical v (Note m)
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
  Note (Deg $ floor $ negate (d - z) + z) mods


invertAround2
  :: Note n  -- ^ Fixed note 1
  -> Note n  -- ^ Fixed note 2
  -> Note n
  -> Note n
invertAround2 fp1 fp2 = invertAround $ (deg fp1 + deg fp2) / 2
  where deg = fromIntegral . getDeg . noteDeg


neoriemannian
  :: forall s v
   . (Enum v, Bounded v, Ord v, KnownNat s)
  => v
  -> v
  -> VoiceLeading (Vertical v) s
neoriemannian v1 v2 = VoiceLeading $ selfDual $ \v -> do
  let sSize = scaleSize @s
      fp1 = view (voice v1) v
      fp2 = view (voice v2) v
      pcs = foldMap (S.singleton . normalizeNote) v
      inv = normalizeNote . invertAround2 fp1 fp2
      locate f =
        let z = f (normalizeNote fp1) (normalizeNote fp2)
          in ( inv z
             , fromMaybe (error "neoriemannian: impossible")
                 $ findIndex (== z)
                 $ S.toList pcs
             )
  case normalizeNote fp1 == normalizeNote fp2 of
    True ->
      fmap (invertAround2 fp1 fp2) v
        & voice v1 .~ fp1
        & voice v2 .~ fp2
    False ->
      -- Convert the chord into an intrinsic scale. We don't care about the
      -- transformation to get back, since we will compute our own.
      intrinsic v $ \(Proxy @ins) _ -> do
        let insSize = scaleSize @ins
            ins = S.toList $ S.map inv pcs
            (hiX, loOffset) = locate min
            (loX, hiOffset) = locate max
            cycled = ins <> fmap (+ fromIntegral sSize) cycled
            cyclic
              = filter (/= loX)
              $ filter (/= hiX)
              $ dropWhile ((/=) loX) cycled
            indices
              = take (insSize - 2)
              $ filter ((/= hiOffset) . snd)
              $ filter ((/= loOffset) . snd)
              $ fmap (flip divMod insSize)
              $ drop loOffset [0 ..]
            placements
              = IM.fromList
              $ (loOffset, loX)
              : (hiOffset, hiX)
              : do
                  ((d, m), note) <- zip indices cyclic
                  pure
                    ( m
                    , note - fromIntegral (d * sSize)
                    )
        fmap $ applyScale $ mkExoticScale @ins $ do
          index <- [0 .. insSize - 1]
          pure $ placements IM.! index

