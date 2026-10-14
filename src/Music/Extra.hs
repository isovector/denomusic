{-# OPTIONS_GHC -Wno-orphans #-}

module Music.Extra where

import FRP
import Music.Beat
import Music.Harmony
import Music.Types


leading' :: (KnownNat c, KnownNat s) => SF (Event (Deg s)) (Event (T c s))
leading' = proc ed' -> do
  ed <- offset (0.000000001) <<< hold 0 -< ed'
  returnA -< fmap (lead ed) ed'

msumE :: Monoid a => SF (Event a) a
msumE = arr (fmap (<>)) >>> accum mempty >>> hold mempty


instance HasDuration Beat where
  duration = #bduration


toV :: SF i (Vertical v a) -> Vertical v (SF i a)
toV = distribute


toH :: (Enum v, Bounded v, Eq v) => Vertical v (SF i a) -> SF i (Vertical v a)
toH = sequenceA

