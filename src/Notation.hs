
module Notation where

import Debug.Trace
import Data.Foldable
import Data.List (partition)
import Data.Maybe
import Control.Monad.State
import Data.Foldable
import Data.Music.Lilypond
import Euterpea qualified as E
import Text.Pretty (pretty)
import Etude16
import Tile
import Data.Bool


toPitch :: E.Pitch -> Pitch
toPitch (pc, o) =
  let (pn, a) = pitchClassToPitchAndAccidental pc
   in  Pitch (pn, a, o)

pitchClassToPitchAndAccidental :: E.PitchClass -> (PitchName, Int)
pitchClassToPitchAndAccidental E.C   = (C, 0)
pitchClassToPitchAndAccidental E.Cff = (C, -2)
pitchClassToPitchAndAccidental E.Cf  = (C, -1)
pitchClassToPitchAndAccidental E.Cs  = (C, 1)
pitchClassToPitchAndAccidental E.Css = (C, 2)
pitchClassToPitchAndAccidental E.D   = (D, 0)
pitchClassToPitchAndAccidental E.Dff = (D, -2)
pitchClassToPitchAndAccidental E.Df  = (D, -1)
pitchClassToPitchAndAccidental E.Ds  = (D, 1)
pitchClassToPitchAndAccidental E.Dss = (D, 2)
pitchClassToPitchAndAccidental E.E   = (E, 0)
pitchClassToPitchAndAccidental E.Eff = (E, -2)
pitchClassToPitchAndAccidental E.Ef  = (E, -1)
pitchClassToPitchAndAccidental E.Es  = (E, 1)
pitchClassToPitchAndAccidental E.Ess = (E, 2)
pitchClassToPitchAndAccidental E.F   = (F, 0)
pitchClassToPitchAndAccidental E.Fff = (F, -2)
pitchClassToPitchAndAccidental E.Ff  = (F, -1)
pitchClassToPitchAndAccidental E.Fs  = (F, 1)
pitchClassToPitchAndAccidental E.Fss = (F, 2)
pitchClassToPitchAndAccidental E.G   = (G, 0)
pitchClassToPitchAndAccidental E.Gff = (G, -2)
pitchClassToPitchAndAccidental E.Gf  = (G, -1)
pitchClassToPitchAndAccidental E.Gs  = (G, 1)
pitchClassToPitchAndAccidental E.Gss = (G, 2)
pitchClassToPitchAndAccidental E.A   = (A, 0)
pitchClassToPitchAndAccidental E.Aff = (A, -2)
pitchClassToPitchAndAccidental E.Af  = (A, -1)
pitchClassToPitchAndAccidental E.As  = (A, 1)
pitchClassToPitchAndAccidental E.Ass = (A, 2)
pitchClassToPitchAndAccidental E.B   = (B, 0)
pitchClassToPitchAndAccidental E.Bff = (B, -2)
pitchClassToPitchAndAccidental E.Bf  = (B, -1)
pitchClassToPitchAndAccidental E.Bs  = (B, 1)
pitchClassToPitchAndAccidental E.Bss = (B, 2)

forking :: [(Rational, E.Pitch)] -> Music -> Music
forking [] m = m
forking es m = simultaneous (forked es) m

forked :: [(Rational, E.Pitch)] -> Music
forked [] = rest
forked es = foldr1 simultaneous $ fmap (\(d, p) -> Note (NotePitch (toPitch p) Nothing) (Just $ Duration d) []) es

chorded :: Rational -> [E.Pitch] -> Music
chorded d ps = Chord (fmap ((, []) . (\p -> NotePitch p Nothing) . toPitch) ps) (Just $ Duration d) []

toLilypond' :: [Events E.Pitch] -> Music
toLilypond' [] = rest
toLilypond' (Events notes _ : es) = do
  let min_dur = minimum $ fmap fst notes
      mnext_wait = fmap e_at $ listToMaybe es
  case mnext_wait of
    Nothing -> forked (toList notes)
    Just next_wait ->
      case min_dur <= next_wait of
        True -> do
          -- we can fit in at least one note before the next section
          let (short, long) = partition ((== min_dur) . fst) $ toList notes
          let remaining = toLilypond' es
          forking
            long
            $ sequential (chorded min_dur $ fmap snd short)
            $ (bool (sequential (Rest (Just $ Duration $ next_wait - min_dur) [])) id (min_dur == next_wait)) remaining
        False -> do
          let remaining = toLilypond' es
          forking (toList notes)
            $ sequential (Rest (Just $ Duration $ next_wait) []) remaining


toLilypond :: Tile E.Pitch -> Music
toLilypond = toLilypond' . flatten



