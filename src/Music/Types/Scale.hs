module Music.Types.Scale where

import Data.Bool
import Data.Fin
import Data.Iso
import Data.List (find, minimumBy)
import Data.Ord (Down(..), comparing)
import Data.Set (Set)
import Data.Set qualified as S
import GHC.TypeLits
import Music.Types.Deg
import Music.Types.Note
import Music.Types.Accidentals
import Prelude hiding (id, (.))


data Scale a b where
  Scale
    :: (KnownNat a, KnownNat b)
    => (Fin a -> Note b)
    -> Scale a b

applyScale
    :: forall m n
     . Scale m n
    -> Note m
    -> Note n
applyScale (Scale f) (Note (Deg dm) o)
  = knownInduction @m
  $ let (oct, pc) = divMod dm $ scaleSize @m
        (Note (Deg dn) o') = f $ toEnum pc
        (o'', od) = decompAccidental @n o
    in Note (od + Deg (dn + oct * scaleSize @n)) $ o' <> o''


-- | Generate a scale from a set of "generalized pitch classes." In particular,
-- this normalizes all notes to lie within a single octave, and then
-- deduplicates them. It then maps @0 :: Note s@ to be the least element in the
-- set.
mkScale :: forall c s. (Induction c, KnownNat s) => Set (Note s) -> Scale c s
mkScale = mkExoticScale . S.toList . S.map normalizeNote


-- | Generate an "exotic" scale. Unlike 'mkScale', this function performs no
-- normalization/ordering/deduplication on the incoming list. Thus you can very
-- odd things like make a non-monotonic, non-bijective scale.
mkExoticScale :: forall c s. (Induction c, KnownNat s) => [Note s] -> Scale c s
mkExoticScale ds' = do
  let n = length ds'
  case n == scaleSize @c of
    True -> Scale $ \c -> ds' !! fromIntegral c
    False -> error $ unwords
      [ "mkExoticScale: declared as chord size"
      , show $ scaleSize @c
      , "but given"
      , show n
      , "notes."
      ]


invertScale
    :: forall a b
     . (1 <= a)
    => (forall x. Ord x => Induction b => Fin b -> [(Fin b, x)] -> (Int, x))
       -- ^ Given the target pitch class and the (non-empty) image of the
       -- scale, choose a candidate along with an octave offset to apply to
       -- it. Parametricity in @x@ guarantees the result came from the image.
    -> Scale a b
    -> Scale b a
invertScale
    choose
    (Scale f) = knownInduction @a $ knownInduction @b $ do
  let img :: [(Fin b, Note a)]
      img = do
        a <- [0 .. maxBound @(Fin a)]
        let (Note (Deg db) os) = f a
        let (o, pc) = divMod db $ scaleSize @b
        let b = toEnum @(Fin b) pc
        pure (b, Note (fromIntegral a - Deg (scaleSize @a * o)) $ fmap negate os)
  Scale $ \b ->
    case lookup b img of
      Just a -> a
      Nothing -> do
        let (ox, chosen) = choose b img
            (b', Note da os) =
              case find ((== chosen) . snd) img of
                Just c -> c
                Nothing -> error "invertScale: impossible; chooser escaped the image"
            -- How far (in steps of @b@) the requested pitch class is from
            -- the chosen candidate, accounting for any octave wraparound.
            diff = fromIntegral b - (fromIntegral b' + ox * scaleSize @b)
        Note (da + Deg (ox * scaleSize @a)) $ os <> accidental @b diff


-- | Build a chooser that prefers candidates satisfying @acceptable@, picking
-- among them with @getBest@. If none are acceptable, wrap around by
-- @octOffset@ octaves and pick from the whole image.
directional
    :: Ord x
    => (Fin b -> Fin b -> Bool)
    -> (forall y. Ord y => [y] -> y)
    -> Int
    -> Fin b
    -> [(Fin b, x)]
    -> (Int, x)
directional acceptable getBest octOffset b img =
  case filter (flip acceptable b . fst) img of
    [] -> (octOffset, snd $ getBest img)
    cs -> (0, snd $ getBest cs)

-- Invert a scale. If @a < b@, then the resulting scale must somehow map notes
-- from @b@ into @a@. It does this by rounding down in @a@ and choosing a note
-- above that.
invertScaleAbove :: (1 <= a) => Scale a b -> Scale b a
invertScaleAbove = invertScale $ directional (<=) maximum (-1)

-- Invert a scale. If @a < b@, then the resulting scale must somehow map notes
-- from @b@ into @a@. It does this by rounding up in @a@ and choosing a note
-- below that.
invertScaleBelow :: (1 <= a) => Scale a b -> Scale b a
invertScaleBelow = invertScale $ directional (>=) minimum 1

-- | Invert a scale, choosing the candidate (possibly in a neighbouring octave)
-- which requires the smallest ornamental offset. Ties prefer the candidate
-- below the target, as in 'invertScaleDown'.
invertScaleNearest :: forall a b. (1 <= a, Induction b) => Scale a b -> Scale b a
invertScaleNearest = invertScale $ \b img -> fmap snd $ do
  let key (ox, (cb, n)) =
        let d = fromIntegral b - (fromIntegral cb + ox * scaleSize @b) :: Int
         in (abs d, d < 0, Down n)
  minimumBy (comparing key) $ do
    c <- img
    ox <- [-1, 0, 1]
    pure (ox, c)

