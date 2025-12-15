module Music.Play
  ( play
  ) where

import Control.Arrow
import Data.Tree.DUAL
import Euterpea qualified as E
import Music.Types
import Data.Maybe


play :: Music -> IO ()
play
  = E.playDev 2
  . fmap (first toStupidEuterpeaPitchClass)
  . toEuterpea


toEuterpea
  :: Music
  -> E.Music (PitchClass, Int)
toEuterpea (Music tree) = fromMaybe (E.rest 0) $
  foldDUAL
    (\e t ->
      E.rest (e_offset e) E.:+: E.note (e_duration e)
        (fromReg $ export e t)
    )
    (E.rest 0)
    (foldr1 (E.:=:))
    (const id)
    (\case
      TimeSignature{} -> id
      Tempo note beats ->
        -- Euterpea's default tempo is 120 quarter notes per minute, and
        -- 'E.tempo' performs a scaling. So we need to rescale it to 1 whole
        -- note per second, and then change the tempo to our needs.
        E.tempo $ do
          let metro setting dur = 60 / (fromIntegral @Integer setting * dur)
          metro beats
            ( -- The tempo sets the duration of a quarter note! So we must
              -- divide our unit down by a quarter note too! Guess how long
              -- this took me to sort out.
              note / (1/4)
            ) / metro 120 (1/4)

      Phrase ->
        E.phrase $ pure $ E.Art $
          -- This mysterious parameter sets a scaling factor on the duration of
          -- non-terminal notes in a phrase. Chosen "by ear."
          E.Slurred 1.7
      Articulate _ -> id
    )
    tree


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

