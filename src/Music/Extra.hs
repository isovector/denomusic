module Music.Extra where

import Data.Set qualified as S
import DenoMusic.Harmony
import Music.Beat
import FRP


leading' :: (KnownNat c, KnownNat s) => SF (Event (Deg s)) (Event (T c s))
leading' = proc ed' -> do
  ed <- offset (0.000000001) <<< hold 0 -< ed'
  returnA -< fmap (lead ed) ed'

msumE :: Monoid a => SF (Event a) a
msumE = arr (fmap (<>)) >>> accum mempty >>> hold mempty

class HasDuration a where
  duration :: a -> Time

instance HasDuration Beat where
  duration = bduration

instance HasDuration Time where
  duration = id

note :: HasDuration a => (b -> c) -> Event (a, b) -> Event (Notes c)
note f = fmap (\(a, b) -> Notes $ S.singleton (duration a, f b))

