module Music.Types.Scale where

import Control.Category
import Data.Bool
import Data.List (sort, nub)
import GHC.TypeLits
import Music.Types.Note
import Prelude hiding (id, (.))


data Scale a b = Scale
  { applyScale :: Note a -> Note b
  }

instance Category Scale where
  id = Scale id
  Scale g . Scale f = Scale $ g . f


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
      Scale $ elim $ \(Deg i) -> do
        let (q, r) = divMod i n
        (ds' !! r) + fromIntegral (q * o)
    False -> error $ unwords
      [ "mkScale: declared as chord size"
      , show $ scaleSize @c
      , "but given"
      , show n
      , "notes."
      ]

