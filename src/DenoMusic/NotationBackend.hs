{-# OPTIONS_GHC -fno-warn-deprecations  #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module DenoMusic.NotationBackend (finalizeLily, makeScore, header, footer, pPrint, toPitch) where

import Data.Containers.ListUtils (nubOrd)
import Data.Function (on)
import Data.Lilypond
import Data.List (groupBy, sortOn, unsnoc)
import Data.Maybe (isJust, mapMaybe)
import Data.Music.Lilypond.Pitch hiding (PitchName (..))
import Data.Music.Lilypond.Pitch qualified as L
import Data.Ord (Down (..))
import DenoMusic.Types
import Text.PrettyPrint.HughesPJClass (pPrint)


--------------------------------------------------------------------------------
-- Pitch conversion
--------------------------------------------------------------------------------

toPitch :: Reg PitchClass -> Pitch
toPitch (Reg o pc) =
  let (pn, a) = pitchClassToPitchAndAccidental pc
   in Pitch (pn, a, o + 1)

pitchClassToPitchAndAccidental :: PitchClass -> (L.PitchName, Int)
pitchClassToPitchAndAccidental C  = (L.C, 0)
pitchClassToPitchAndAccidental Cs = (L.C, 1)
pitchClassToPitchAndAccidental D  = (L.D, 0)
pitchClassToPitchAndAccidental Df = (L.D, -1)
pitchClassToPitchAndAccidental Ds = (L.D, 1)
pitchClassToPitchAndAccidental E  = (L.E, 0)
pitchClassToPitchAndAccidental Ef = (L.E, -1)
pitchClassToPitchAndAccidental F  = (L.F, 0)
pitchClassToPitchAndAccidental Fs = (L.F, 1)
pitchClassToPitchAndAccidental G  = (L.G, 0)
pitchClassToPitchAndAccidental Gf = (L.G, -1)
pitchClassToPitchAndAccidental Gs = (L.G, 1)
pitchClassToPitchAndAccidental A  = (L.A, 0)
pitchClassToPitchAndAccidental Af = (L.A, -1)
pitchClassToPitchAndAccidental As = (L.A, 1)
pitchClassToPitchAndAccidental B  = (L.B, 0)
pitchClassToPitchAndAccidental Bf = (L.B, -1)


--------------------------------------------------------------------------------
-- Intermediate representation
--
-- Rather than compiling a timeline of events directly to lilypond, we first
-- normalize each voice into a gapless sequence of 'Span's. A 'Span' occupies a
-- half-open interval of time and carries a 'Body' describing what sounds (or
-- doesn't sound) during that interval. Zero-width control spans (clefs, time
-- signatures, etc.) are threaded through without consuming any time.
--------------------------------------------------------------------------------

-- | The contents of a single 'Span'.
data Body
  = -- | Silence.
    Rest'
  | -- | A zero-width control event injected verbatim into the stream.
    Control Score
  | -- | One or more simultaneous pitches (a note or a chord) that share a
    -- single set of post events.
    Notes [PostEvent] [Reg PitchClass]

-- | An event occupying the half-open time interval @[spanStart, spanEnd)@.
data Span = Span
  { spanStart :: Rational
  , spanEnd   :: Rational
  , spanBody  :: Body
  }

isControl :: Span -> Bool
isControl (Span _ _ (Control _)) = True
isControl _                      = False

spanDur :: Span -> Rational
spanDur s = spanEnd s - spanStart s


--------------------------------------------------------------------------------
-- Voice -> Score
--------------------------------------------------------------------------------

type Voice = [(Interval Rational, Either Score ([PostEvent], Reg PitchClass))]

-- | Turn a voice's raw timeline into intermediate 'Span's. Notes sharing the
-- same interval are merged into a single chord.
toSpans :: Voice -> [Span]
toSpans xs = controls <> notes
  where
    controls =
      [ Span lo lo (Control sc)
      | (Interval lo _, Left sc) <- xs
      ]

    notes =
      fmap mkChord
        . groupBy (on (==) fst)
        . sortOn fst
        $ [ (Interval lo hi, (pes, p))
          | (Interval lo hi, Right (pes, p)) <- xs
          ]

    mkChord [] = Span 0 0 (Notes [] [])
    mkChord grp@((Interval lo hi, _) : _) =
      let pes = nubOrd $ foldMap (fst . snd) grp
          ps  = fmap (snd . snd) grp
       in Span lo hi (Notes pes ps)

-- | Sort spans by start time, placing zero-width controls before the notes
-- that begin at the same instant.
sortSpans :: [Span] -> [Span]
sortSpans = sortOn (\s -> (spanStart s, not (isControl s)))

-- | Walk a sorted span list and insert explicit rests into every gap, so the
-- resulting sequence is completely gapless from time zero onward. Controls do
-- not advance the clock or produce rests.
fillRests :: [Span] -> [Span]
fillRests = go 0
  where
    go _ [] = []
    go cur (s : ss)
      | isControl s = s : go cur ss
      | spanStart s > cur =
          Span cur (spanStart s) Rest' : s : go (spanEnd s) ss
      | otherwise = s : go (max cur (spanEnd s)) ss

-- | Split note and rest spans so that none crosses a barline, which would be
-- musically ungrammatical (e.g. a note from 0.75 to 1.5 must be notated as a
-- quarter note tied to a half note, not as a dotted half note). Each time a
-- span would cross a barline it is cut there; the earlier of the two pieces of
-- a split /note/ gains a 'Tie' so the parts remain a single sounding event.
-- Rests are simply cut, never tied.
--
-- The current measure length is tracked in whole notes, defaulting to 4/4 (one
-- whole note) and updated whenever a @\\time@ control is encountered. The
-- control's position is taken to be a barline, so meter changes need not align
-- with the previous meter's grid.
splitBars :: [Span] -> [Span]
splitBars = go 1 0
  where
    go _ _ [] = []
    go barLen barStart (s : ss)
      | isControl s =
          let (barLen', barStart') = updateMeter barLen barStart s
           in s : go barLen' barStart' ss
      | otherwise = splitOne barLen barStart s <> go barLen barStart ss

    updateMeter _ _ (Span pos _ (Control (Time m n))) =
      (fromIntegral m / fromIntegral n, pos)
    updateMeter barLen barStart _ = (barLen, barStart)

    splitOne barLen barStart s
      | spanDur s > 0 && nextBar < spanEnd s =
          Span (spanStart s) nextBar (tieBody (spanBody s))
            : splitOne barLen barStart s { spanStart = nextBar }
      | otherwise = [s]
      where
        k = floor ((spanStart s - barStart) / barLen) :: Integer
        nextBar = barStart + barLen * fromIntegral (k + 1)

    tieBody (Notes pes ps) = Notes (pes <> [Tie]) ps
    tieBody body           = body

-- | Split note and rest spans at beat-group boundaries /within/ a measure, so
-- that durations don't obscure the metric structure. For example, in 4/4 a note
-- must not cross the middle of the bar unless it is a single notatable value
-- that begins on beat one; in 6/8 a note must not cross the boundary between the
-- two dotted-quarter beats. 'splitBars' is assumed to have run first, so no span
-- crosses a barline here.
--
-- As with 'splitBars', the earlier of the two pieces of a split /note/ gains a
-- 'Tie'; rests are merely cut.
splitBeats :: [Span] -> [Span]
splitBeats = go (4, 4) 0
  where
    go _ _ [] = []
    go sig barStart (s : ss)
      | isControl s =
          let (sig', barStart') = updateMeter sig barStart s
           in s : go sig' barStart' ss
      | spanDur s <= 0 = s : go sig barStart ss
      | otherwise = splitOne sig barStart s <> go sig barStart ss

    updateMeter _ _ (Span pos _ (Control (Time m n))) = ((m, n), pos)
    updateMeter sig barStart _                         = (sig, barStart)

    splitOne (m, n) barStart s =
      let barLen    = fromIntegral m / fromIntegral n
          k         = floor ((spanStart s - barStart) / barLen) :: Integer
          measStart = barStart + barLen * fromIntegral k
          tree      = measureMetric m n measStart
          pieces    = splitMetric tree (spanStart s, spanEnd s)
       in retie (spanBody s) pieces

    retie body pieces =
      case unsnoc pieces of
        Nothing -> []
        Just (initPs, (lo, hi)) ->
          fmap (\(a, b) -> Span a b (tieBody body)) initPs
            <> [Span lo hi body]

    -- Avoid stacking a second tie onto a note 'splitBars' already tied.
    tieBody (Notes pes ps)
      | Tie `elem` pes = Notes pes ps
      | otherwise      = Notes (pes <> [Tie]) ps
    tieBody body       = body


--------------------------------------------------------------------------------
-- Metric trees
--
-- A 'Metric' describes the recursive division of a measure into beat groups,
-- beats, and sub-beats. 'splitMetric' walks this tree to decide where a note or
-- rest must be broken so that each resulting piece is a single notatable value
-- aligned to the meter.
--------------------------------------------------------------------------------

-- | A node covering the half-open interval @[start, end)@, subdivided into
-- child nodes. Leaves have no children.
data Metric = Metric Rational Rational [Metric]

-- | Build the metric tree for one measure of @m/n@ time beginning at @start@
-- (in whole-note units).
measureMetric :: Integer -> Integer -> Rational -> Metric
measureMetric m n start = buildNode start (1 / fromIntegral n) m

-- | Build a node spanning @count@ base units of length @unit@, subdividing per
-- 'splitCount'. A single base unit is divided binarily all the way down.
buildNode :: Rational -> Rational -> Integer -> Metric
buildNode start unit count
  | count <= 1 = buildBinary start unit
  | otherwise =
      let parts    = splitCount count
          offs     = scanl (+) 0 parts
          mk o c   = buildNode (start + unit * fromIntegral o) unit c
          children = zipWith mk offs parts
       in Metric start (start + unit * fromIntegral count) children

-- | Recursively halve an interval down to a 64th-note granularity.
buildBinary :: Rational -> Rational -> Metric
buildBinary start len
  | len <= 1 / 64 = Metric start (start + len) []
  | otherwise =
      let half = len / 2
       in Metric start (start + len)
            [ buildBinary start half
            , buildBinary (start + half) half
            ]

-- | How to subdivide a group of @n@ base units: compound groups (a multiple of
-- three, as in 6/8 or 9/8) break into dotted beats of three; otherwise we halve
-- when possible and fall back to a leading power-of-two chunk.
splitCount :: Integer -> [Integer]
splitCount n
  | n `mod` 3 == 0 && n > 3 = replicate (fromIntegral (n `div` 3)) 3
  | even n                  = [n `div` 2, n `div` 2]
  | n == 3                  = [2, 1]
  | otherwise               = [n - 1, 1]

-- | Split the note/rest interval @(s, e)@ against a metric tree, returning the
-- gapless pieces it must be notated as. A piece is kept whole when it exactly
-- fills a node, or when it starts on a node boundary and is itself a single
-- notatable value (which permits dotted notes); otherwise it is broken at the
-- boundaries of the overlapping children and each fragment recurses.
splitMetric :: Metric -> (Rational, Rational) -> [(Rational, Rational)]
splitMetric (Metric ms me children) (s, e)
  | s >= e               = []
  | s == ms && e == me   = [(s, e)]                      -- fills this node
  | s == ms && notatable (e - s) = [(s, e)]              -- left-aligned single value
  | otherwise =
      case filter overlaps children of
        [] -> [(s, e)]                                   -- leaf: nothing finer
        cs -> concatMap descend cs
  where
    overlaps (Metric cs ce _) = cs < e && s < ce
    descend (Metric cs ce cc) =
      let s' = max s cs
          e' = min e ce
       in if s' < e' then splitMetric (Metric cs ce cc) (s', e') else []

-- | Is @r@ (in whole-note units) a single notatable duration, possibly dotted?
notatable :: Rational -> Bool
notatable r = r > 0 && isJust (separateDots (Duration r))

-- | Compile a single 'Span' into a lilypond note/chord/rest.
spanToScore :: Span -> Score
spanToScore s = case spanBody s of
  Rest'         -> Rest (spanDur s) []
  Control sc    -> sc
  Notes pes [p] -> Note (spanDur s) (NotePitch (toPitch p)) pes
  Notes pes ps  -> Chord (fmap (NotePitch . toPitch) ps) (spanDur s) pes

-- | Compile a voice all the way down to a lilypond expression, filling rests,
-- splitting over-long durations, generating tuplets, and tying everything into
-- notatable note values.
voiceToScore :: Voice -> Score
voiceToScore =
    tieLengths
  . splitRests
  . makeTuplets
  . Sequential
  . fmap spanToScore
  . splitBeats
  . splitBars
  . fillRests
  . sortSpans
  . toSpans


--------------------------------------------------------------------------------
-- Assembling the grand staff
--------------------------------------------------------------------------------

-- | Set up the nth voice's stem directions and collision behaviour. LilyPond's
-- @\\voiceOne@ .. @\\voiceFour@ are just shorthands for
-- @make-voice-props-set 0@ .. @3@; using the Scheme form directly lets us
-- support arbitrarily many parallel voices (see @voiceFive@ and beyond).
voiceSetter :: Int -> Score
voiceSetter n =
  Raw $ "#(context-spec-music (make-voice-props-set " <> show n <> ") 'Voice)"

-- | Wrap a voice's music as @\\new Voice { <props> \\autoChange \\fixed c' { ... } }@
-- so that @\\autoChange@ can distribute its notes between the two staves of the
-- piano. The @\\fixed c'@ block establishes the octave reference that our
-- absolute pitches are notated against.
wrapVoice :: Int -> Score -> Score
wrapVoice n music =
  New "Voice" Nothing $
    Sequential
      [ voiceSetter n
      , Command "autoChange"
      , Fixed (Pitch (L.C, 0, 4)) music
      ]

-- | Approximate absolute height of a single pitch, in semitones, used only to
-- compare voices against one another.
pitchHeight :: Reg PitchClass -> Int
pitchHeight (Reg o pc) = o * 12 + fromEnum pc

-- | A voice's average pitch height, used to rank voices from high to low.
-- Voices with no sounding notes sort to the bottom.
voiceHeight :: Voice -> Double
voiceHeight v =
  case mapMaybe pitchOf v of
    [] -> -1 / 0
    hs -> fromIntegral (sum hs) / fromIntegral (length hs)
  where
    pitchOf (_, Right (_, p)) = Just (pitchHeight p)
    pitchOf _                 = Nothing

-- | Reorder voices into the order LilyPond expects for numbered voices:
--
-- > Voice 1: highest      Voice 2: lowest
-- > Voice 3: 2nd highest   Voice 4: 2nd lowest
-- > Voice 5: 3rd highest   Voice 6: 3rd lowest   (etc.)
--
-- The input is first sorted high-to-low, then interleaved from both ends.
voiceOrder :: [Voice] -> [Voice]
voiceOrder = interleave . sortOn (Down . voiceHeight)
  where
    interleave []       = []
    interleave [x]      = [x]
    interleave (x : xs) = x : last xs : interleave (init xs)

-- | The upper staff. Every voice lives here; @\\autoChange@ relocates the low
-- notes to the "down" staff automatically.
upStaff :: [Score] -> Score
upStaff voices =
  New "Staff" (Just "up") $
    Simultaneous False $
      [ Command "set Timing.beamExceptions = #'()"
      , Command "set Timing.beatStructure = #'(4)"
      , Command "accidentalStyle piano-cautionary"
      ]
        <> zipWith wrapVoice [0 ..] voices

-- | The lower staff. It holds no music of its own; @\\autoChange@ fills it in.
downStaff :: Score
downStaff =
  New "Staff" (Just "down") $
    Sequential
      [ Clef Bass
      , Command "accidentalStyle piano-cautionary"
      ]

makeScore :: [Voice] -> Score
makeScore voices =
  New "PianoStaff" Nothing $
    Simultaneous False
      [ upStaff (fmap voiceToScore (voiceOrder voices))
      , downStaff
      ]


--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

finalizeLily :: Score -> String
finalizeLily = show . pPrint

header :: String
header =
  unlines
    [ "\\header { tagline = \"\" }"
    , "\\absolute"
    ]

footer :: String
footer =
  unlines
    [ ""
    , "\\paper {"
    , "ragged-last = ##t"
    , "}"
    ]

