{-# LANGUAGE DerivingStrategies                   #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fno-warn-orphans                 #-}

module Notation where

import Text.Pretty  (pretty)
import Data.Containers.ListUtils (nubOrd)
import Control.Exception
import Data.Bifunctor
import Data.Sequence qualified as Seq
import Data.Sequence (Seq(..))
import Data.IntervalMap.FingerTree (IntervalMap, Interval(..))
import Data.IntervalMap.FingerTree qualified as IM
import Data.Maybe
import Data.Foldable
import Data.Music.Lilypond hiding (chord)
import Theory.Chords
import Euterpea qualified as E
-- import Text.Pretty (pretty)
import Score hiding (im)
import Data.Tree.DUAL


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

imToSeq :: Ord v => IntervalMap v a -> Seq (Interval v, a)
imToSeq im =
  case IM.bounds im of
    Just b -> Seq.fromList $ IM.intersections b im
    Nothing -> Seq.empty

seqToIm :: Ord v => Seq (Interval v, a) -> IntervalMap v a
seqToIm = foldMap (uncurry IM.singleton)

overIM
  :: Ord v
  => (Seq (Interval v, a) -> Seq (Interval v, b))
  -> IntervalMap v a -> IntervalMap v b
overIM f = seqToIm . f . imToSeq

everywhere
  :: PostEvent
  -> Seq (Interval v, ([PostEvent], a))
  -> Seq (Interval v, ([PostEvent], a))
everywhere pe = fmap $ fmap $ first (pe :)

around
  :: PostEvent
  -> PostEvent
  -> Seq (Interval v, ([PostEvent], a))
  -> Seq (Interval v, ([PostEvent], a))
around _ _ Empty = Empty
around s e ((i, (pes, a)) :<| Empty) = (i, (s : e : pes, a)) :<| Empty
around s e ((i1, (pes1, a1)) :<| (xs :|> (i2, (pes2, a2)))) =
  (i1, (s : pes1, a1)) :<| (xs :|> (i2, (e : pes2, a2)))

scoreToIM :: Score a -> IntervalMap Rational ([PostEvent], a)
scoreToIM = fromMaybe mempty .
  foldDUAL
    (\(Envelope s o) a ->
      let lo = min o (o + s)
          hi = min o (o + s)
       in IM.singleton (Interval lo hi) (mempty, a))
    mempty
    fold
    (const id)
    (\ann ->
      case ann of
        Phrase -> overIM $ around BeginPhraseSlur EndPhraseSlur
    )
    . unScore

deriving stock instance Ord Markup
deriving stock instance Ord Articulation
deriving stock instance Ord PostEvent

imToLilypond :: IntervalMap Rational ([PostEvent], E.Pitch) -> Music
imToLilypond im =
  case IM.leastView im of
    Nothing -> rest
    Just ((i, xs), im') -> do
      let start = IM.low i
          concurrent = IM.search start im'
          notes = xs : fmap snd concurrent
      assert (all ((== IM.high i) . IM.high . fst) concurrent) $
        sequential (Rest (Just $ Duration start) []) $
          Chord
            (fmap ((, []) . (\p -> NotePitch p Nothing) . toPitch . snd) notes)
            (Just $ Duration $ IM.high i - IM.low i)
            $ nubOrd $ foldMap fst notes


toLilypond :: Score E.Pitch -> String
toLilypond = show . pretty . imToLilypond . scoreToIM

-- forking :: [(Rational, E.Pitch)] -> Music -> Music
-- forking [] m = m
-- forking es m = simultaneous (forked es) m

-- forked :: [(Rational, E.Pitch)] -> Music
-- forked [] = rest
-- forked es = foldr1 simultaneous $ fmap (\(d, p) -> Note (NotePitch (toPitch p) Nothing) (Just $ Duration d) []) es

-- chorded :: Rational -> [E.Pitch] -> Music
-- chorded d ps = Chord (fmap ((, []) . (\p -> NotePitch p Nothing) . toPitch) ps) (Just $ Duration d) []

-- toLilypond' :: [Events E.Pitch] -> Music
-- toLilypond' [] = rest
-- toLilypond' (Events notes _ : es) = do
--   let min_dur = minimum $ fmap fst notes
--       mnext_wait = fmap e_at $ listToMaybe es
--   case mnext_wait of
--     Nothing -> forked (toList notes)
--     Just next_wait ->
--       case min_dur <= next_wait of
--         True -> do
--           -- we can fit in at least one note before the next section
--           let (short, long) = partition ((== min_dur) . fst) $ toList notes
--           let remaining = toLilypond' es
--           forking
--             long
--             $ sequential (chorded min_dur $ fmap snd short)
--             $ (bool (sequential (Rest (Just $ Duration $ next_wait - min_dur) [])) id (min_dur == next_wait)) remaining
--         False -> do
--           let remaining = toLilypond' es
--           forking (toList notes)
--             $ sequential (Rest (Just $ Duration $ next_wait) []) remaining


-- toLilypond :: Tile E.Pitch -> Music
-- toLilypond = toLilypond' . flatten



