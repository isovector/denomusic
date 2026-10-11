{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeAbstractions    #-}
{-# LANGUAGE ViewPatterns        #-}

module Music.Harmony
  ( module Music.Harmony
  , KnownNat
  ) where

import Control.Category
import Data.Bool
import Data.List (findIndex, sort, nub)
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
apply (T i o) (Scale f) = Scale $ (+ MkNote o []) . f . (+ MkNote i [])


intrinsic
  :: forall v c r
   . (Enum v, Bounded v, KnownNat c)
  => (Vertical v (Note c))
  -> (forall ins. KnownNat ins => Proxy ins -> Scale ins c -> Vertical v (Note ins) -> r)
  -> r
intrinsic voiced k = do
  let notes = fmap (getVertical voiced) $ enumFromTo minBound maxBound
  withSomeSNat (fromIntegral $ length notes) $ \case
    Just (snat :: SNat ins) ->
      let ds' = nub $ sort $ fmap normalizeNote notes
          n = length ds'
          o = scaleSize @c
       in withKnownNat snat
            $ k @ins Proxy (mkScale notes)
            $ flip fmap voiced $ \(Note (Deg a) xs) -> do
                let (q, r) = divMod a o
                case findIndex ((== Deg r) . noteDeg) ds' of
                  Just idx -> MkNote (fromIntegral (q * n + idx))xs
                  Nothing -> error "impossible"
    Nothing -> error "impossible"


voiceLead
    :: (Enum v, Bounded v, KnownNat s)
    => Deg s
    -> Deg s
    -> Vertical v (Note s)
    -> Vertical v (Note s)
voiceLead from to voiced =
  intrinsic voiced $ \(Proxy @ins) sc v' -> do
    let t = lead @ins from to
    fmap (applyScale $ apply t sc) v'

