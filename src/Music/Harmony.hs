{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeAbstractions    #-}
{-# LANGUAGE ViewPatterns        #-}

module Music.Harmony
  ( module Music.Harmony
  , KnownNat
  , Group(..)
  ) where

import Data.Functor.Compose
import Data.Iso
import Control.Lens ((+~), (&), view, (-~))
import Data.Bool
import Data.Monoid
import Data.Proxy
import Data.Ratio
import GHC.TypeLits
import Music.Types.Note
import Music.Types.Scale
import Music.Types.Vertical
import Prelude hiding (id, (.))


triad :: Scale 3 7
triad = mkScale [0, 2, 4]

diatonic :: Scale 7 12
diatonic = mkScale [0, 2, 4, 5, 7, 9, 11]


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
  :: forall v c r
   . (Enum v, Bounded v, KnownNat c)
  => (Vertical v (Note c))
  -> (forall ins. KnownNat ins => Proxy ins -> Scale ins c -> Vertical v (Note ins) -> r)
  -> r
intrinsic voiced k = do
  let notes = fmap (getVertical voiced) $ enumFromTo minBound maxBound
  withSomeSNat (fromIntegral $ length notes) $ \case
    Just (snat :: SNat ins) -> do
      withKnownNat snat $ do
        let sc = mkScale @ins notes
        k Proxy sc $ fmap (invertScale sc) voiced
    Nothing -> error "impossible"


newtype VoiceLeading v s = VoiceLeading
  { voiceLeadingAnIso :: AnIso' (Vertical v (Note s)) (Vertical v (Note s))
  }
  deriving (Semigroup, Monoid, Group) via Endomorphism (GenericIso (Compose (Vertical v) Note)) n

voiceLeadingIso :: VoiceLeading v s -> Iso' (Vertical v (Note s)) (Vertical v (Note s))
voiceLeadingIso v = cloneIso $ voiceLeadingAnIso v

applyVoiceLeading :: VoiceLeading v s -> Vertical v (Note s) -> Vertical v (Note s)
applyVoiceLeading v = view $ voiceLeadingIso v

instance (Show v, Enum v, Bounded v) => Show (VoiceLeading v s) where
  show vl =
    let v = iota
     in prettyVertical $ liftA2 (\x y -> unwords [show x, "→", show y]) v (applyVoiceLeading vl v)

voiceLead
    :: (Enum v, Bounded v, KnownNat s)
    => Deg s
    -> Deg s
    -> VoiceLeading v s
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
expand :: (KnownNat m, Ord v) => v -> v -> VoiceLeading v m
expand src dst = VoiceLeading $ iso (expandF src dst) (contractF src dst)

contract :: (KnownNat m, Ord v) => v -> v -> VoiceLeading v m
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

