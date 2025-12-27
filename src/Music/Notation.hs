{-# OPTIONS_GHC -fno-warn-deprecations  #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# OPTIONS_GHC -fno-warn-x-partial     #-}

module Music.Notation (finalizeLily, header, footer) where

import Control.Arrow ((&&&))
import Control.Monad.State
import Data.Bifunctor
import Data.Containers.ListUtils (nubOrd)
import Data.Either
import Data.Foldable
import Data.Function
import Data.IntervalMap.FingerTree (low, high)
import Data.Lilypond hiding (Tempo)
import Data.List (sortOn, groupBy, partition)
import Data.Maybe
import Data.Music.Lilypond.Pitch hiding (PitchName(..))
import Data.Music.Lilypond.Pitch qualified as L
import Data.Ord
import Data.Ratio (denominator)
import Data.Sequence (Seq(..))
import Data.Sequence qualified as Seq
import DenoMusic.Types hiding (Empty)
import Text.PrettyPrint.HughesPJClass (pPrint)


toPitch :: Reg PitchClass -> Pitch
toPitch (Reg o pc) =
  let (pn, a) = pitchClassToPitchAndAccidental pc
   in  Pitch (pn, a, o + 1)

pitchClassToPitchAndAccidental :: PitchClass -> (L.PitchName, Int)
pitchClassToPitchAndAccidental C   = (L.C, 0)
pitchClassToPitchAndAccidental Cs  = (L.C, 1)
pitchClassToPitchAndAccidental D   = (L.D, 0)
pitchClassToPitchAndAccidental Df  = (L.D, -1)
pitchClassToPitchAndAccidental Ds  = (L.D, 1)
pitchClassToPitchAndAccidental E   = (L.E, 0)
pitchClassToPitchAndAccidental Ef  = (L.E, -1)
pitchClassToPitchAndAccidental F   = (L.F, 0)
pitchClassToPitchAndAccidental Fs  = (L.F, 1)
pitchClassToPitchAndAccidental G   = (L.G, 0)
pitchClassToPitchAndAccidental Gf  = (L.G, -1)
pitchClassToPitchAndAccidental Gs  = (L.G, 1)
pitchClassToPitchAndAccidental A   = (L.A, 0)
pitchClassToPitchAndAccidental Af  = (L.A, -1)
pitchClassToPitchAndAccidental As  = (L.A, 1)
pitchClassToPitchAndAccidental B   = (L.B, 0)
pitchClassToPitchAndAccidental Bf  = (L.B, -1)

everywhere
  :: PostEvent
  -> [(a, Either b ([PostEvent], c))]
  -> [(a, Either b ([PostEvent], c))]
everywhere pe = fmap $ fmap $ fmap $ first (pe :)

around
  :: PostEvent
  -> PostEvent
  -> [(a, Either b ([PostEvent], c))]
  -> [(a, Either b ([PostEvent], c))]
around s e = toList . around' . Seq.fromList
  where
    around' z =
      let (ctrl, z') = Seq.breakl (isRight . snd) z
          (ctrr, z'') = Seq.breakr (isRight . snd) z'
       in (<> ctrr) $ (ctrl <>) $
        case z'' of
          Empty -> Empty
          ((i, Right (pes, a)) :<| Empty) -> (i, Right (s : e : pes, a)) :<| Empty
          ((i1, Right (pes1, a1))
            :<| (xs :|> (i2, Right (pes2, a2))))
                -> (i1, Right (s : pes1, a1)) :<| (xs :|> (i2, Right (e : pes2, a2)))
          x -> error "impossible!"


grouping :: (Eq a) => [(a, Either b c)] -> [(a, ([b], [c]))]
grouping =
  fmap (fst . head &&& partitionEithers . fmap snd)
  . groupBy (on (==) fst)

imsToLilypond :: [[(Interval Rational, Either Score ([PostEvent], Reg PitchClass))]] -> Score
imsToLilypond
  = tieLengths
  . splitRests
  . makeTuplets
  . Simultaneous True
  . fmap (flip evalState 0 . imToLilypond . grouping)

iDur :: Interval Rational -> Rational
iDur (Interval lo hi) = hi - lo

mkNotes :: Interval Rational -> [([PostEvent], Reg PitchClass)] -> Score
mkNotes i [] = Rest (iDur i) []
mkNotes i [(ps, e)] = Note (iDur i) (NotePitch $ toPitch e)  ps
mkNotes i notes =
  Chord
    (fmap (NotePitch . toPitch . snd) notes)
    (high i - low i)
    $ nubOrd $ foldMap fst notes

imToLilypond :: [(Interval Rational, ([Score], [([PostEvent], Reg PitchClass)]))] -> State Rational Score
imToLilypond [] = pure mempty
imToLilypond [(i, (sc, es))] = do
  prev <- get
  put $ high i
  pure $ barCheck prev <> delayed (low i - prev) (fold sc <> mkNotes i es)
imToLilypond ((i, (sc, es)) : is) = do
  prev <- get
  put $ high i
  remaining <- imToLilypond is
  pure $ barCheck prev <>
      delayed (low i - prev) (mconcat $ sc <> [mkNotes i es, remaining])


barCheck :: Rational -> Score
barCheck r =
  case r /= 0 && denominator r == 1 of
    True -> BarCheck
    False -> mempty


delayed :: Rational -> Score -> Score
delayed 0 m = m
delayed d m = Sequential [Rest d [],  m]

average :: [Int] -> Float
average i = fromIntegral (sum i) / fromIntegral (length i)

averagePitch :: Foldable t => (a -> Int) -> t a -> Int
averagePitch f = round . average . fmap f . toList

inject :: (Score, Score) -> Score
inject (treble, bass) =
  New "GrandStaff" Nothing $ Simultaneous False
    [ New "Staff" Nothing $ Sequential [ Clef Treble, Command "accidentalStyle piano-cautionary", treble ]
    , New "Staff" Nothing $ Sequential [ Clef Bass, Command "accidentalStyle piano-cautionary", bass ]
    ]


finalizeLily :: [[(Interval Rational, Either Score ([PostEvent], Reg PitchClass))]] -> String
finalizeLily
  = show
  . pPrint
  . inject
  . bimap imsToLilypond imsToLilypond
  . partition ((>= 4) . averagePitch (getReg . snd) . mapMaybe (hush . snd))
  . sortOn (Down . averagePitch (getReg . snd) . mapMaybe (hush . snd))

hush :: Either e a -> Maybe a
hush = either (const Nothing) Just

header :: String
header = unlines
  [ "\\header { tagline = \"\" }"
  , "\\absolute"
  ]


footer :: String
footer = unlines
  [ ""
  , "\\paper {"
  , "ragged-last = ##t"
  , "}"
  ]

sameTimeAs :: [(Interval Rational, Either b c)] -> b -> [(Interval Rational, Either b c)]
sameTimeAs [] _ = error "no time like the present"
sameTimeAs ab@((Interval lo hi, _) : _) b = (Interval lo hi, Left b) : ab

