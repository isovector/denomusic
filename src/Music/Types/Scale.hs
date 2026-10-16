module Music.Types.Scale where

import Control.Category
import Control.Lens
import Data.Bool
import Data.List (sort, nub, minimumBy)
import Data.Ord (comparing, Down(..))
import GHC.TypeLits
import Music.Types.Note
import Prelude hiding (id, (.))


newtype Scale a b = Scale
  { scaleIso :: Iso' (Note a) (Note b)
  }

opScale :: Scale a b -> Scale b a
opScale (Scale s) = Scale $ from s

instance Category Scale where
  id = Scale id
  Scale g . Scale f = Scale $ f . g

applyScale :: Scale a b -> Note a -> Note b
applyScale (Scale s) = view s

invertScale :: Scale a b -> Note b -> Note a
invertScale = applyScale . opScale

elim :: (Deg a -> Note b) -> Note a -> Note b
elim f (Note a []) = f a
elim f (Note a (j : js)) = f a + Note (Deg j) js


mkScale :: forall c s. (KnownNat c, KnownNat s) => [Note s] -> Scale c s
mkScale ds = do
  let ds' = nub $ sort $ fmap normalizeNote ds
      o = scaleSize @s
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
            Note (Deg cd) (cr : mods)
    False -> error $ unwords
      [ "mkScale: declared as chord size"
      , show $ scaleSize @c
      , "but given"
      , show n
      , "notes."
      ]

