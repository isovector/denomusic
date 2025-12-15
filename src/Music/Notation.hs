{-# OPTIONS_GHC -fno-warn-deprecations  #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# OPTIONS_GHC -fno-warn-x-partial     #-}

module Music.Notation (toLilypond, toPdf) where

import Control.Arrow ((&&&))
import Control.Monad.State
import Data.Bifunctor
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable
import Data.Function
import Data.IntervalMap.FingerTree (IntervalMap, Interval(..))
import Data.IntervalMap.FingerTree qualified as IM
import Data.Lilypond
import Data.List (sortOn, groupBy, partition)
import Data.Music.Lilypond.Pitch hiding (PitchName(..))
import Data.Music.Lilypond.Pitch qualified as L
import Data.Sequence (Seq(..))
import Data.Sequence qualified as Seq
import Data.Tree.DUAL
import Music.Types
import System.Cmd (rawSystem)
import Text.PrettyPrint.HughesPJClass (pPrint)


toPitch :: Reg PitchClass -> Pitch
toPitch (Reg o pc) =
  let (pn, a) = pitchClassToPitchAndAccidental pc
   in  Pitch (pn, a, o + 1)

pitchClassToPitchAndAccidental :: PitchClass -> (L.PitchName, Int)
pitchClassToPitchAndAccidental C   = (L.C, 0)
pitchClassToPitchAndAccidental Cff = (L.C, -2)
pitchClassToPitchAndAccidental Cf  = (L.C, -1)
pitchClassToPitchAndAccidental Cs  = (L.C, 1)
pitchClassToPitchAndAccidental Css = (L.C, 2)
pitchClassToPitchAndAccidental D   = (L.D, 0)
pitchClassToPitchAndAccidental Dff = (L.D, -2)
pitchClassToPitchAndAccidental Df  = (L.D, -1)
pitchClassToPitchAndAccidental Ds  = (L.D, 1)
pitchClassToPitchAndAccidental Dss = (L.D, 2)
pitchClassToPitchAndAccidental E   = (L.E, 0)
pitchClassToPitchAndAccidental Eff = (L.E, -2)
pitchClassToPitchAndAccidental Ef  = (L.E, -1)
pitchClassToPitchAndAccidental Es  = (L.E, 1)
pitchClassToPitchAndAccidental Ess = (L.E, 2)
pitchClassToPitchAndAccidental F   = (L.F, 0)
pitchClassToPitchAndAccidental Fff = (L.F, -2)
pitchClassToPitchAndAccidental Ff  = (L.F, -1)
pitchClassToPitchAndAccidental Fs  = (L.F, 1)
pitchClassToPitchAndAccidental Fss = (L.F, 2)
pitchClassToPitchAndAccidental G   = (L.G, 0)
pitchClassToPitchAndAccidental Gff = (L.G, -2)
pitchClassToPitchAndAccidental Gf  = (L.G, -1)
pitchClassToPitchAndAccidental Gs  = (L.G, 1)
pitchClassToPitchAndAccidental Gss = (L.G, 2)
pitchClassToPitchAndAccidental A   = (L.A, 0)
pitchClassToPitchAndAccidental Aff = (L.A, -2)
pitchClassToPitchAndAccidental Af  = (L.A, -1)
pitchClassToPitchAndAccidental As  = (L.A, 1)
pitchClassToPitchAndAccidental Ass = (L.A, 2)
pitchClassToPitchAndAccidental B   = (L.B, 0)
pitchClassToPitchAndAccidental Bff = (L.B, -2)
pitchClassToPitchAndAccidental Bf  = (L.B, -1)
pitchClassToPitchAndAccidental Bs  = (L.B, 1)
pitchClassToPitchAndAccidental Bss = (L.B, 2)

imToSeq :: Ord v => IntervalMap v a -> Seq (Interval v, a)
imToSeq im =
  case IM.bounds im of
    Just b -> Seq.fromList $ IM.intersections b im
    Nothing -> Seq.empty

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

deriving stock instance Ord Markup
deriving stock instance Ord Articulation
deriving stock instance Ord PostEvent

grouping :: Eq a => [(a, b)] -> [(a, [b])]
grouping = fmap (fst . head &&& fmap snd) . groupBy (on (==) fst)

imsToLilypond :: [IntervalMap Rational ([PostEvent], Reg PitchClass)] -> Score
imsToLilypond
  = removeParallelRests
  . tieLengths
  . makeTuplets
  . Simultaneous True
  . fmap (flip evalState 0 . imToLilypond . grouping. traversalOrder)

iDur :: Interval Rational -> Rational
iDur (Interval lo hi) = hi - lo

mkNotes :: Interval Rational -> [([PostEvent], Reg PitchClass)] -> Score
mkNotes i [] = Rest (iDur i) []
mkNotes i [(ps, e)] = Note (NotePitch $ toPitch e) (iDur i) ps
mkNotes i notes =
  Chord
    (fmap ((, []) . NotePitch . toPitch . snd) notes)
    (IM.high i - IM.low i)
    $ nubOrd $ foldMap fst notes

imToLilypond :: [(Interval Rational, [([PostEvent], Reg PitchClass)])] -> State Rational Score
imToLilypond [] = error "impossible"
imToLilypond [(i, es)] = do
  prev <- get
  put $ IM.high i
  pure $ delayed (IM.low i - prev) $ mkNotes i es
imToLilypond ((i, es) : is) = do
  prev <- get
  put $ IM.high i
  remaining <- imToLilypond is
  pure $ delayed (IM.low i - prev) $ sequential (mkNotes i es) remaining

delayed :: Rational -> Score -> Score
delayed 0 m = m
delayed d m = sequential (Rest d []) m

average :: [Int] -> Float
average i = fromIntegral (sum i) / fromIntegral (length i)

averagePitch :: (a -> Int) -> IntervalMap Rational a -> Int
averagePitch f = round . average . fmap f . toList

inject :: (Score, Score) -> Score
inject (treble, bass) =
  New "GrandStaff" Nothing $ Simultaneous False
    [ New "Staff" Nothing $ Sequential [ Clef Treble, Command "accidentalStyle piano-cautionary", treble ]
    , New "Staff" Nothing $ Sequential [ Clef Bass, Command "accidentalStyle piano-cautionary", bass ]
    ]


finalizeLily :: [IntervalMap Rational ([PostEvent], Reg PitchClass)] -> String
finalizeLily
  = show
  . pPrint
  . inject
  . (bimap imsToLilypond imsToLilypond)
  . partition ((>= 4) . averagePitch (getReg . snd))

header :: String
header = unlines
  [ "\\header { tagline = \"\" }"
  , "\\absolute"
  ]


data Terminal a = Start a | Stop a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

traversalOrder :: Ord v => IntervalMap v a -> [(Interval v, a)]
traversalOrder = sortOn (\((Interval lo hi), _) -> (lo, hi)) . toList . imToSeq


toVoices :: Music -> [IntervalMap Rational ([PostEvent], Reg PitchClass)]
toVoices
  = fmap toIM
  . groupBy (on (==) $ e_voice . fst)
  . sortOn (e_voice . fst)
  . fmap (\(t, e) -> (e, export e t))
  . flatten
  . unMusic


toIM :: [(Envelope, Reg a)] -> IntervalMap Rational ([x], Reg a)
toIM = foldMap $ \(e, a) ->
  let o = e_offset e
      s = e_duration e
      lo = min o (o + s)
      hi = max o (o + s)
    in IM.singleton (Interval lo hi) (mempty, a)


toLilypond :: Music -> String
toLilypond = finalizeLily . toVoices


toPdf :: Music -> IO ()
toPdf m = do
  let lp = read @String $ show $ pPrint $ toLilypond m
  writeFile "/tmp/out.lily" $ header <> lp
  _ <- rawSystem "lilypond" ["-o", "/tmp/song", "/tmp/out.lily"]
  pure ()
