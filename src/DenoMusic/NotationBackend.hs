{-# OPTIONS_GHC -fno-warn-deprecations  #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module DenoMusic.NotationBackend (finalizeLily, makeScore, header, footer, pPrint, toPitch) where

import Data.Ratio (denominator)
import Data.Bits ((.&.))
import Data.Containers.ListUtils (nubOrd)
import Data.Function (on)
import Data.Lilypond
import Data.List (groupBy, sortOn, unsnoc)
import Data.Maybe (isJust)
import Data.Music.Lilypond.Pitch hiding (PitchName (..))
import Data.Music.Lilypond.Pitch qualified as L
import Data.Ord (Down (..))
import DenoMusic.Types
import Euterpea qualified as E
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
  | -- | Silence that occupies time but is not drawn (a lilypond spacer rest).
    -- Used when another voice already shows a rest or note covering the same
    -- time, so repeating the rest here would just add clutter.
    Spacer
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
-- Music -> Spans
--
-- We compile a Euterpea 'E.Music' directly by structural recursion. Parallel
-- composition (@:=:@) splits a piece into independent voices, while a parallel
-- of simultaneous notes within a voice becomes a chord. Each voice is rendered
-- into an absolute-time 'Span' timeline, which the rest of this module already
-- knows how to notate.
--
-- Simplifying assumption: no sequential-of-parallel-of-sequential. In other
-- words, @:=:@ is used either to stack whole voices (its operands are
-- themselves sequential music) or to stack the simultaneous notes of a single
-- chord (its operands are bare notes) \-\- never to splice a multi-voice passage
-- into the middle of another voice.
--------------------------------------------------------------------------------

-- | Split a piece of music into its parallel voices. A top-level @:=:@ is read
-- as parallel composition of voices; anything else is a single voice whose
-- timeline is produced by 'musicToSpans'.
musicToVoices :: E.Music (Reg PitchClass) -> [[Span]]
musicToVoices = \case
  m1 E.:=: m2   -> musicToVoices m1 <> musicToVoices m2
  E.Modify _ m  -> musicToVoices m
  m             -> [snd (musicToSpans 0 m)]

-- | Render a single (parallel-free, except for chords) voice into an
-- absolute-time span timeline, starting at time @t@. Returns the end time
-- alongside the spans so sequential composition can thread the clock through.
musicToSpans :: Rational -> E.Music (Reg PitchClass) -> (Rational, [Span])
musicToSpans t = \case
  E.Prim (E.Note d p)
    | d <= 0    -> (t, [])
    | otherwise -> (t + d, [Span t (t + d) (Notes [] [p])])
  E.Prim (E.Rest d)
    | d <= 0    -> (t, [])
    | otherwise -> (t + d, [Span t (t + d) Rest'])
  m1 E.:+: m2 ->
    -- Sequential composition: lay @m2@ down right after @m1@.
    let (t1, s1) = musicToSpans t m1
        (t2, s2) = musicToSpans t1 m2
     in (t2, s1 <> s2)
  m1 E.:=: m2 ->
    -- Parallel composition within a voice: a chord. Both operands start at the
    -- same instant; merge notes sharing an interval into a single chord span.
    let (t1, s1) = musicToSpans t m1
        (t2, s2) = musicToSpans t m2
     in (max t1 t2, mergeChords (s1 <> s2))
  E.Modify _ m -> musicToSpans t m

-- | Merge note spans that share an identical interval into a single chord span,
-- combining their pitches and post events. Non-note spans are left untouched.
mergeChords :: [Span] -> [Span]
mergeChords = fmap merge . groupBy (on (==) key) . sortOn key
  where
    key s = (spanStart s, spanEnd s)

    merge grp@(s0 : _) =
      case [ (pes, ps) | Span _ _ (Notes pes ps) <- grp ] of
        []    -> s0
        notes ->
          Span (spanStart s0) (spanEnd s0) $
            Notes (nubOrd (foldMap fst notes)) (foldMap snd notes)
    merge [] = Span 0 0 (Notes [] [])

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
      -- Everything, even a tuplet member, must be cut at a barline: nothing may
      -- cross one. A non-dyadic span that lies wholly inside a bar is left
      -- untouched here (its 'nextBar' falls past its end), so in-bar tuplets are
      -- preserved for 'makeTuplets'.
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
      -- Tuplet members live on non-dyadic grid positions; leave them for
      -- 'makeTuplets' rather than shredding them against the dyadic beat grid.
      | not (dyadicSpan s) = s : go sig barStart ss
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

-- | A rational is dyadic when its denominator is a power of two. Such values
-- fall on the ordinary binary rhythmic grid; anything else (1\/3, 1\/6, 1\/5,
-- ...) is part of a tuplet and must be left for 'makeTuplets' to handle.
isDyadic :: Rational -> Bool
isDyadic r =
  let d = denominator r
   in d == 1 || (d .&. (d - 1)) == 0

-- | A note\/rest span sits on the dyadic grid when both its endpoints (and
-- hence its duration) are dyadic. Tuplet members fail this test.
dyadicSpan :: Span -> Bool
dyadicSpan s = isDyadic (spanStart s) && isDyadic (spanEnd s)

-- | Compile a single 'Span' into a lilypond note/chord/rest.
spanToScore :: Span -> Score
spanToScore s = case spanBody s of
  Rest'         -> Rest (spanDur s) []
  Spacer        -> Skip (spanDur s) []
  Control sc    -> sc
  Notes pes [p] -> Note (spanDur s) (NotePitch (toPitch p)) pes
  Notes pes ps  -> Chord (fmap (NotePitch . toPitch) ps) (spanDur s) pes

-- | Normalize a voice into a gapless, sorted sequence of 'Span's, with every
-- silence made explicit as a 'Rest''. This is the first half of compilation;
-- it is kept separate from 'spansToScore' so that 'shareRests' can look at all
-- voices' spans together before each is notated.
normalizeVoice :: [Span] -> [Span]
normalizeVoice = fillRests . sortSpans

-- | Compile a normalized span list all the way down to a lilypond expression,
-- splitting over-long durations, generating tuplets, and tying everything into
-- notatable note values.
spansToScore :: [Span] -> Score
spansToScore =
    tieLengths
  . splitRests
  . makeTuplets
  . Sequential
  . fmap spanToScore
  . insertBarlines
  . splitBeats
  . splitBars

-- | Insert a zero-width 'BarCheck' control at every barline that coincides with
-- a span boundary. These markers do nothing for ordinary notation, but they
-- stop 'makeTuplets' from merging tuplet notes on opposite sides of a barline
-- into one bar-spanning tuplet (tuplets may not cross barlines).
--
-- Meter is tracked exactly as in 'splitBars': the measure length defaults to
-- 4/4 and is reset, along with the bar origin, by each @\\time@ control.
insertBarlines :: [Span] -> [Span]
insertBarlines = go 1 0
  where
    go _ _ [] = []
    go barLen barStart (s : ss)
      | isControl s =
          let (barLen', barStart') = updateMeter barLen barStart s
           in s : go barLen' barStart' ss
      | onBarline barLen barStart s =
          Span (spanStart s) (spanStart s) (Control BarCheck)
            : s
            : go barLen barStart ss
      | otherwise = s : go barLen barStart ss

    updateMeter _ _ (Span pos _ (Control (Time m n))) =
      (fromIntegral m / fromIntegral n, pos)
    updateMeter barLen barStart _ = (barLen, barStart)

    onBarline barLen barStart s =
      let n = (spanStart s - barStart) / barLen
       in spanStart s > barStart && denominator n == 1


--------------------------------------------------------------------------------
-- Cross-voice rest sharing
--
-- When several voices share a staff, a rest in one voice is redundant if, over
-- the same span of time, another voice already shows a note or rest: the reader
-- can see the beat from that other voice. 'shareRests' turns such redundant
-- rests into invisible spacer rests, keeping exactly one visible rest at each
-- instant where /every/ voice is silent.
--------------------------------------------------------------------------------

-- | Rewrite redundant rests across a set of normalized voices into spacers.
-- A rest in voice @i@ over some sub-interval becomes a 'Spacer' unless either
-- (a) some other voice is sounding a note there (its note shows the beat), or
-- (b) all voices are silent there and @i@ is the lowest-indexed silent voice,
--     which is elected to carry the one visible rest.
shareRests :: [[Span]] -> [[Span]]
shareRests voices =
  [ concatMap (shareSpan i) sv
  | (i, sv) <- zip [0 ..] voices
  ]
  where
    n = length voices

    -- Every note/rest boundary anywhere, used to cut rests so each piece sees a
    -- constant set of other-voice states. Only dyadic boundaries are used:
    -- cutting a rest on a tuplet boundary would leave non-dyadic rest fragments
    -- that 'makeTuplets' would then mis-group, so tuplet regions are left whole
    -- for the normal rest/tuplet machinery downstream.
    breakpoints =
      nubOrd $
        sortOn id
          [ t
          | sv <- voices
          , s <- sv
          , not (isControl s)
          , t <- [spanStart s, spanEnd s]
          , isDyadic t
          ]

    -- The body of the span covering instant @t@ in voice @j@, if any.
    bodyAt j t =
      case [ spanBody s
           | s <- voices !! j
           , not (isControl s)
           , spanStart s <= t
           , t < spanEnd s
           ] of
        (b : _) -> Just b
        []      -> Nothing

    isSounding (Just (Notes _ _)) = True
    isSounding _                  = False

    isResting (Just Rest') = True
    isResting _            = False

    shareSpan _ s@(Span _ _ Rest') | spanDur s <= 0 = [s]
    -- A rest that is itself a tuplet member (non-dyadic endpoints) must be left
    -- whole and visible; cutting it would strand non-dyadic fragments that
    -- 'makeTuplets' would then mis-group.
    shareSpan _ s@(Span _ _ Rest') | not (dyadicSpan s) = [s]
    shareSpan i s@(Span lo hi Rest') =
      rebuild lo $
        mergeRuns
          [ (a, b, decide i a b)
          | (a, b) <- subIntervals lo hi
          ]
    shareSpan _ s = [s]

    -- Cut @[lo, hi)@ at every interior breakpoint.
    subIntervals lo hi =
      let cuts  = filter (\t -> lo < t && t < hi) breakpoints
          edges = lo : cuts <> [hi]
       in zip edges (drop 1 edges)

    -- Decide whether voice @i@'s rest over @[a, b)@ stays visible or hides.
    decide i a b
      -- Spacer rests inside a tuplet are ungrammatical, so only ever hide rests
      -- that sit on the ordinary dyadic grid.
      | not (isDyadic a && isDyadic b) = Rest'
      | otherwise =
          let mid    = (a + b) / 2
              others = [j | j <- [0 .. n - 1], j /= i]
           in if any (\j -> isSounding (bodyAt j mid)) others
                then Spacer
                else
                  let resting = [j | j <- [0 .. n - 1], isResting (bodyAt j mid)]
                   in case resting of
                        (owner : _) | owner == i -> Rest'
                        _                        -> Spacer

    -- Coalesce neighbouring sub-intervals that reached the same verdict.
    mergeRuns [] = []
    mergeRuns ((a, b, body) : rest) =
      case mergeRuns rest of
        (a', b', body') : more
          | sameBody body body' && b == a' -> (a, b', body) : more
        merged -> (a, b, body) : merged

    rebuild _ runs = [Span a b body | (a, b, body) <- runs]

    sameBody Rest'  Rest'  = True
    sameBody Spacer Spacer = True
    sameBody _      _      = False


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
voiceHeight :: [Span] -> Double
voiceHeight v =
  case concatMap pitchesOf v of
    [] -> -1 / 0
    hs -> fromIntegral (sum hs) / fromIntegral (length hs)
  where
    pitchesOf (Span _ _ (Notes _ ps)) = fmap pitchHeight ps
    pitchesOf _                       = []

-- | Reorder voices into the order LilyPond expects for numbered voices:
--
-- > Voice 1: highest      Voice 2: lowest
-- > Voice 3: 2nd highest   Voice 4: 2nd lowest
-- > Voice 5: 3rd highest   Voice 6: 3rd lowest   (etc.)
--
-- The input is first sorted high-to-low, then interleaved from both ends.
voiceOrder :: [[Span]] -> [[Span]]
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
      [ Command "accidentalStyle piano-cautionary"
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

makeScore :: E.Music (Reg PitchClass) -> Score
makeScore music =
  New "PianoStaff" Nothing $
    Simultaneous False
      [ upStaff (fmap spansToScore sharedVoices)
      , downStaff
      ]
  where
    sharedVoices =
      shareRests (fmap normalizeVoice (voiceOrder (musicToVoices music)))


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

