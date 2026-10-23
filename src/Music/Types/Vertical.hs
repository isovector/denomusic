module Music.Types.Vertical
  ( module Music.Types.Vertical
  , Vec(..)
  , Fin(..)
  , voice
  , Distributive(..)
  ) where

import Data.Distributive
import Music.Types.Note
import Music.Types.Scale
import Data.Vec (Vec(..), voice)
import Data.Vec qualified as V
import Data.Fin

type Vertical = Vec

getVertical :: Vertical n a -> Fin n -> a
getVertical = (V.!!)

iota :: (KnownNat v, Num a) => Vertical v a
iota = fmap (fromIntegral . toNatural) V.indicesI

chord :: KnownNat v => Scale c s -> Vertical v (Note s)
chord s = fmap (applyScale s) iota

