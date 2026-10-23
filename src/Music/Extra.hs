{-# OPTIONS_GHC -Wno-orphans #-}

module Music.Extra where

import FRP
import Music.Harmony
import Music.Types


leading' :: (KnownNat c, KnownNat s) => SF (Event (Deg s)) (Event (T c s))
leading' = proc ed' -> do
  ed <- offset (0.000000001) <<< hold 0 -< ed'
  returnA -< fmap (lead ed) ed'

msumE :: Monoid a => SF (Event a) a
msumE = arr (fmap (<>)) >>> accum mempty >>> hold mempty


toV :: (KnownNat v, Functor f) => f (Vertical v a) -> Vertical v (f a)
toV = distribute


fromV :: (KnownNat v, Applicative f) => Vertical v (f a) -> f (Vertical v a)
fromV = sequenceA

