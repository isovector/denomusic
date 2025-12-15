module Music.Play
  ( play
  ) where

import Control.Arrow
import Data.Functor
import Data.Tree.DUAL
import Euterpea qualified as E
import Music.Types


play :: Music -> IO ()
play
  = E.playDev 2
  . fmap (first toStupidEuterpeaPitchClass)
  . toEuterpea


toEuterpea
  :: Music
  -> E.Music (PitchClass, Int)
toEuterpea s = do
  foldr (E.:=:) (E.rest 0) $
    (flatten $ unMusic s) <&> \(n, e) ->
      E.rest (e_offset e) E.:+: E.note (e_duration e)
        (fromReg $ export e n)


toStupidEuterpeaPitchClass :: PitchClass -> E.PitchClass
toStupidEuterpeaPitchClass Aff = E.Aff
toStupidEuterpeaPitchClass Af = E.Af
toStupidEuterpeaPitchClass A = E.A
toStupidEuterpeaPitchClass As = E.As
toStupidEuterpeaPitchClass Ass = E.Ass
toStupidEuterpeaPitchClass Bff = E.Bff
toStupidEuterpeaPitchClass Bf = E.Bf
toStupidEuterpeaPitchClass B = E.B
toStupidEuterpeaPitchClass Bs = E.Bs
toStupidEuterpeaPitchClass Bss = E.Bss
toStupidEuterpeaPitchClass Cff = E.Cff
toStupidEuterpeaPitchClass Cf = E.Cf
toStupidEuterpeaPitchClass C = E.C
toStupidEuterpeaPitchClass Cs = E.Cs
toStupidEuterpeaPitchClass Css = E.Css
toStupidEuterpeaPitchClass Dff = E.Dff
toStupidEuterpeaPitchClass Df = E.Df
toStupidEuterpeaPitchClass D = E.D
toStupidEuterpeaPitchClass Ds = E.Ds
toStupidEuterpeaPitchClass Dss = E.Dss
toStupidEuterpeaPitchClass Eff = E.Eff
toStupidEuterpeaPitchClass Ef = E.Ef
toStupidEuterpeaPitchClass E = E.E
toStupidEuterpeaPitchClass Es = E.Es
toStupidEuterpeaPitchClass Ess = E.Ess
toStupidEuterpeaPitchClass Fff = E.Fff
toStupidEuterpeaPitchClass Ff = E.Ff
toStupidEuterpeaPitchClass F = E.F
toStupidEuterpeaPitchClass Fs = E.Fs
toStupidEuterpeaPitchClass Fss = E.Fss
toStupidEuterpeaPitchClass Gff = E.Gff
toStupidEuterpeaPitchClass Gf = E.Gf
toStupidEuterpeaPitchClass G = E.G
toStupidEuterpeaPitchClass Gs = E.Gs
toStupidEuterpeaPitchClass Gss = E.Gss

