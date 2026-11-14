module Music.Types.Scale where

import Control.Lens (view, from)
import Data.Bool
import Data.IntMap qualified as IM
import Data.Iso
import Data.List (minimumBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing, Down(..))
import Data.Set (Set)
import Data.Set qualified as S
import GHC.TypeLits
import Music.Types.Deg
import Music.Types.Note
import Music.Types.Ornaments
import Prelude hiding (id, (.))


newtype Scale a b = Scale
  { scaleAnIso :: AnIso' (Note a) (Note b)
  }
  deriving Category via GenericIso Note

scaleIso :: Scale a b -> Iso' (Note a) (Note b)
scaleIso s = cloneIso $ scaleAnIso s

opScale :: Scale a b -> Scale b a
opScale s = Scale $ from $ scaleIso s

applyScale :: Scale a b -> Note a -> Note b
applyScale s = view $ scaleIso s

invertScale :: Scale a b -> Note b -> Note a
invertScale = applyScale . opScale

elim
  :: forall a b
   . (KnownNat a, KnownNat b)
  => (Deg a -> Note b) -> Note a -> Note b
elim f (Note a (Ornaments os)) = do
  let lo = scaleSize @a
      hi = scaleSize @b
      (below, mhere, above) = IM.splitLookup hi os
      own
        | lo < hi = IM.findWithDefault 0 lo below
        | otherwise = 0
      stale = IM.filterWithKey (\k _ -> k /= lo || lo == hi) below
  case IM.keys stale of
    [] -> f (a + Deg own) + Note (Deg $ fromMaybe 0 mhere) (Ornaments above)
    ks -> error $ unwords
      [ "elim: ornament levels"
      , show ks
      , "skipped by scale"
      , show lo
      , "->"
      , show hi
      ]


-- | Generate a scale from a set of "generalized pitch classes." In particular,
-- this normalizes all notes to lie within a single octave, and then
-- deduplicates them. It then maps @0 :: Note s@ to be the least element in the
-- set.
mkScale :: forall c s. (KnownNat c, KnownNat s) => Set (Note s) -> Scale c s
mkScale = mkExoticScale . S.toList . S.map normalizeNote


-- | Generate an "exotic" scale. Unlike 'mkScale', this function performs no
-- normalization/ordering/deduplication on the incoming list. Thus you can very
-- odd things like make a non-monotonic, non-bijective scale. In such a case,
-- the isomorphism behind a scale degrades to a section/retraction.
mkExoticScale :: forall c s. (KnownNat c, KnownNat s) => [Note s] -> Scale c s
mkExoticScale ds' = do
  let o = scaleSize @s
      n = length ds'
  case n == scaleSize @c of
    True ->
      Scale $
        iso
          (elim $ \(Deg i) -> do
            let (q, r) = divMod i n
            (ds' !! r) + fromIntegral (q * o)
          )
          $ \(Note (Deg i) mods) -> do
            let (cd, cr) = minimumBy (comparing $ \(_, r) -> (abs r, Down $ r > 0)) $ do
                  qq <- [div i o - 1 .. div i o + 1]
                  (k, d) <- zip [0 ..] ds'
                  let sDeg = fromIntegral (noteDeg d) + qq * o
                  pure (qq * n + k, i - sDeg)
            Note (Deg cd) (ornament @s cr <> mods)
    False -> error $ unwords
      [ "mkExoticScale: declared as chord size"
      , show $ scaleSize @c
      , "but given"
      , show n
      , "notes."
      ]

