{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# OPTIONS_GHC -fno-warn-deprecations  #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module DenoMusic.NotationBackend
  ( NotationOptions (..)
  , Staves (..)
  , defaultNotationOptions
  , finalizeLily
  , makeScore
  , header
  , preamble
  , footer
  , pPrint
  , toPitch
  , note12ToReg
  ) where

import Data.Ratio (denominator)
import Data.Bits ((.&.))
import Data.Containers.ListUtils (nubOrd)
import Data.Function (on)
import Data.IntMap qualified as IM
import Data.Lilypond
import Data.List (groupBy, sortOn, unsnoc)
import Data.Maybe (isJust)
import Data.Music.Lilypond.Pitch hiding (PitchName (..))
import Data.Music.Lilypond.Pitch qualified as L
import Data.Ord (Down (..))
import Data.Proxy (Proxy (..))
import DenoMusic.Types
import Euterpea qualified as E
import GHC.TypeLits (KnownNat, Nat, natVal)
import Music.Types.Accidentals (getAccidentals)
import Music.Types.Deg (Deg (..))
import Music.Types.Note qualified as N
import Text.PrettyPrint.HughesPJClass (pPrint)


--------------------------------------------------------------------------------
-- Options
--------------------------------------------------------------------------------

-- | Options controlling how music of type @'N.Note' n@ is notated.
data NotationOptions (n :: Nat) = NotationOptions
  { no_timesignature :: Maybe ()
    -- ^ When 'Nothing', every voice is put in @\\cadenzaOn@ mode and no
    -- barline\/beat-based tying or rest splitting is performed.
  , no_staves :: Staves n
    -- ^ How to lay the music out onto staves.
  }
  deriving (Eq, Show)

-- | Staff layouts, indexed by the size of the scale whose notes they display.
data Staves (n :: Nat) where
  -- | A grand staff; notes are distributed between the upper and lower
  -- staves by @\\autoChange@.
  PianoStaff :: Staves 12
  -- | A single staff, using whichever of treble or bass clef fits more
  -- notes within its lines.
  DiatonicStaff :: Staves 12
  -- | A @\\finStaff@ for an @n@-note scale. Each scale degree occupies its
  -- own staff position, with degree 0 on the bottom line (written as @c'@).
  IntrinsicStaff :: KnownNat n => Staves n

deriving instance Eq (Staves n)
deriving instance Show (Staves n)

isPianoStaff :: Staves n -> Bool
isPianoStaff PianoStaff = True
isPianoStaff _          = False

defaultNotationOptions :: NotationOptions 12
defaultNotationOptions =
  NotationOptions
    { no_timesignature = Just ()
    , no_staves = PianoStaff
    }

metered :: NotationOptions n -> Bool
metered = isJust . no_timesignature


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

-- | Spell a chromatic @'N.Note' 12@ (degree 0 is middle C) using sharps.
-- Accidentals are ignored, matching 'DenoMusic.Play.play'.
note12ToReg :: N.Note 12 -> Reg PitchClass
note12ToReg n =
  let d = getDeg (N.noteDeg n)
   in Reg (div d 12 + 4) (spelling !! mod d 12)
  where
    spelling = [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]


--------------------------------------------------------------------------------
-- Staff pitches
--
-- Every staff layout first lowers its notes into a 'StaffPitch': a fully
-- resolved description of how a single notehead is drawn. Everything after this
-- point is agnostic to the original scale. To support new notational features
-- (e.g. intrinsic accidentals), extend 'StaffPitch' and 'staffPitch', and teach
-- 'spanToScore' to emit them.
--------------------------------------------------------------------------------

data StaffPitch = StaffPitch
  { sp_pitch  :: Pitch
    -- ^ The lilypond pitch that puts the notehead on the right staff position.
  , sp_height :: Int
    -- ^ A height used only to rank voices relative to one another.
  , sp_accs   :: [(Int, Int)]
    -- ^ Generalized accidentals as @(scale size, steps)@, rendered with
    -- @\\acc@. Empty for conventionally-spelled pitches.
  }

staffNote :: StaffPitch -> Note
staffNote p = case sp_accs p of
  [] -> NotePitch (sp_pitch p)
  as -> NoteAcc as (sp_pitch p)

-- | Lower a note into its staff representation for the given layout.
staffPitch :: Staves n -> N.Note n -> StaffPitch
staffPitch PianoStaff     = chromaticStaffPitch
staffPitch DiatonicStaff  = chromaticStaffPitch
staffPitch IntrinsicStaff = intrinsicStaffPitch

chromaticStaffPitch :: N.Note 12 -> StaffPitch
chromaticStaffPitch n =
  let r@(Reg o pc) = note12ToReg n
   in StaffPitch (toPitch r) (o * 12 + fromEnum pc) []

-- | Degree @d@ sits @d@ staff steps above @c'@ (the bottom line of a
-- @\\finStaff@). Accidentals are emitted as @\\acc@ annotations, which the
-- preamble's @Deno_accidental_engraver@ tracks and draws.
intrinsicStaffPitch :: N.Note n -> StaffPitch
intrinsicStaffPitch n@(N.Note _ as) =
  let d    = getDeg (N.noteDeg n)
      step = diatonicStepOf (Pitch (L.C, 0, 5)) + d
      accs = IM.toList $ IM.filter (/= 0) $ getAccidentals as
   in StaffPitch (fromDiatonicStep step) d accs

-- | Diatonic step of a lilypond pitch, counting from C0 (so @c'@ is 28).
diatonicStepOf :: Pitch -> Int
diatonicStepOf (Pitch (pn, _, o)) = (o - 1) * 7 + fromEnum pn

-- | Inverse of 'diatonicStepOf', for natural pitches.
fromDiatonicStep :: Int -> Pitch
fromDiatonicStep s = Pitch (toEnum (mod s 7), 0, div s 7 + 1)


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
    Notes [PostEvent] [StaffPitch]

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
musicToVoices :: E.Music StaffPitch -> [[Span]]
musicToVoices = \case
  m1 E.:=: m2   -> musicToVoices m1 <> musicToVoices m2
  E.Modify _ m  -> musicToVoices m
  m             -> [snd (musicToSpans 0 m)]

-- | Render a single (parallel-free, except for chords) voice into an
-- absolute-time span timeline, starting at time @t@. Returns the end time
-- alongside the spans so sequential composition can thread the clock through.
musicToSpans :: Rational -> E.Music StaffPitch -> (Rational, [Span])
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
  Notes pes [p] -> Note (spanDur s) (staffNote p) pes
  Notes pes ps  -> Chord (fmap staffNote ps) (spanDur s) pes

-- | Normalize a voice into a gapless, sorted sequence of 'Span's, with every
-- silence made explicit as a 'Rest''. This is the first half of compilation;
-- it is kept separate from 'spansToScore' so that 'shareRests' can look at all
-- voices' spans together before each is notated.
normalizeVoice :: [Span] -> [Span]
normalizeVoice = fillRests . sortSpans

-- | Compile a normalized span list all the way down to a lilypond expression,
-- splitting over-long durations, generating tuplets, and tying everything into
-- notatable note values.
spansToScore :: NotationOptions n -> [Span] -> Score
spansToScore opts =
    tieLengths
  . splitRests
  . makeTuplets
  . Sequential
  . fmap spanToScore
  . metricSplitting
  where
    metricSplitting
      | metered opts = insertBarlines . splitBeats . splitBars
      | otherwise    = id

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
wrapVoice :: NotationOptions n -> Int -> Score -> Score
wrapVoice opts n music =
  New "Voice" Nothing $
    Sequential $
      [ voiceSetter n ]
        <> concat
             [ [ Command "omit Staff.TimeSignature"
               , Command "cadenzaOn"
               ]
             | not (metered opts)
             ]
        <> [ Command "autoChange" | isPianoStaff (no_staves opts) ]
        <> [ Fixed (Pitch (L.C, 0, 4)) music ]

-- | A voice's average pitch height, used to rank voices from high to low.
-- Voices with no sounding notes sort to the bottom.
voiceHeight :: [Span] -> Double
voiceHeight v =
  case concatMap pitchesOf v of
    [] -> -1 / 0
    hs -> fromIntegral (sum hs) / fromIntegral (length hs)
  where
    pitchesOf (Span _ _ (Notes _ ps)) = fmap sp_height ps
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
upStaff :: NotationOptions n -> [Score] -> Score
upStaff opts voices =
  New "Staff" (Just "up") $
    Simultaneous False $
      [ Command "accidentalStyle piano-cautionary"
      ]
        <> zipWith (wrapVoice opts) [0 ..] voices

-- | The lower staff. It holds no music of its own; @\\autoChange@ fills it in.
downStaff :: Score
downStaff =
  New "Staff" (Just "down") $
    Sequential
      [ Clef Bass
      , Command "accidentalStyle piano-cautionary"
      ]

-- | A single staff holding every voice, beginning with the given clef command.
singleStaff :: NotationOptions n -> Score -> [Score] -> Score
singleStaff opts clef voices =
  New "Staff" Nothing $
    Simultaneous False $
      [ Sequential
          [ clef
          , Command "accidentalStyle modern-cautionary"
          ]
      ]
        <> zipWith (wrapVoice opts) [0 ..] voices

-- | Pick whichever of treble (E4..F5) or bass (G2..A3) clef places more of the
-- given notes within the staff lines. Ties go to treble.
chooseClef :: [[Span]] -> Clef
chooseClef voices =
  if inRange 18 26 > inRange 30 38 then Bass else Treble
  where
    steps =
      [ diatonicStepOf (sp_pitch p)
      | v <- voices
      , Span _ _ (Notes _ ps) <- v
      , p <- ps
      ]
    inRange lo hi = length (filter (\s -> lo <= s && s <= hi) steps)

-- | Insert @\\finOttava@s into a voice on an @n@-note @\\finStaff@, so that
-- music far off the staff is drawn back onto it.
--
-- Each note (or chord) first picks its own ideal octave shift: the one that
-- best recenters it on the staff, or 0 if it lies within four ledger lines.
-- Runs of fewer than four notes are absorbed into a neighbouring run of the
-- same direction (or dropped back to 0), and an ottava change is emitted at
-- every boundary between runs. A passage that descends steadily therefore
-- becomes e.g. @-3@, @-2@, @-1@ rather than one compromise block.
--
-- Rests and controls never change the shift. Assumes 'sp_height' is the
-- scale degree, with degree 0 on the bottom line, as produced by
-- 'intrinsicStaffPitch'.
--
-- Note that ottava is a staff-wide setting, so overlapping runs in different
-- voices of one staff will fight with one another.
finOttavas :: Int -> [Span] -> [Span]
finOttavas n spans = emit 0 spans smoothed
  where
    -- Staff lines sit on even degrees 0, 2, .., top.
    top    = 2 * (n `div` 2 + 1)
    center = fromIntegral top / 2 :: Double
    ledger = 3 -- four ledger lines, two degrees apart
    minRun = 4

    heights s = case spanBody s of
      Notes _ ps@(_ : _) -> Just (fmap sp_height ps)
      _                  -> Nothing

    -- The ideal shift for a single note or chord. Chords that reach far off
    -- both sides at once can't be fixed by an ottava, so they stay at 0.
    ideal hs =
      let above = any (>= top + ledger) hs
          below = any (<= negate ledger) hs
          mid   = fromIntegral (minimum hs + maximum hs) / 2
          k     = round ((mid - center) / fromIntegral n) :: Int
       in if | above && not below -> max 1 k
             | below && not above -> min (-1) k
             | otherwise          -> 0

    noteShifts = [ideal hs | Just hs <- fmap heights spans]

    -- Smooth the per-note shifts so every nonzero run is at least 'minRun'
    -- notes long.
    smoothed = concat (absorb 0 (groupBy (==) noteShifts))

    absorb :: Int -> [[Int]] -> [[Int]]
    absorb _ [] = []
    absorb _ ([] : rs) = absorb 0 rs
    absorb prev (r@(k : _) : rs)
      | k == 0 || length r >= minRun = r : absorb k rs
      | otherwise =
          let next = case rs of
                (r'@(k'' : _) : _) | length r' >= minRun -> k''
                _                                        -> 0
              sameSign x = x /= 0 && signum x == signum k
              k' | sameSign prev = prev
                 | sameSign next = next
                 | otherwise     = 0
           in fmap (const k') r : absorb k' rs

    -- Walk the spans alongside the smoothed per-note shifts, emitting an
    -- ottava change whenever the shift changes, and closing any open ottava
    -- after the final note.
    emit :: Int -> [Span] -> [Int] -> [Span]
    emit _ [] _ = []
    emit cur (s : rest) ks
      | isJust (heights s), (k : ks') <- ks =
          let pre   = [ottava (spanStart s) k | k /= cur]
              close = [ottava (spanEnd s) 0 | k /= 0, null ks']
           in pre <> (s : close) <> emit k rest ks'
      | otherwise = s : emit cur rest ks

    ottava t k =
      Span t t $ Control $
        Raw $ "\\finOttava #" <> show n <> " #" <> show k

makeScore :: forall n. NotationOptions n -> E.Music (N.Note n) -> Score
makeScore opts music =
  case no_staves opts of
    PianoStaff ->
      New "PianoStaff" Nothing $
        Simultaneous False
          [ upStaff opts scores
          , downStaff
          ]
    DiatonicStaff ->
      singleStaff opts (Clef (chooseClef sharedVoices)) scores
    IntrinsicStaff ->
      let size = fromInteger (natVal (Proxy @n))
       in singleStaff opts (Command "intrinsicClef") $
            fmap (spansToScore opts . finOttavas size) sharedVoices
  where
    scores = fmap (spansToScore opts) sharedVoices
    sharedVoices =
      shareRests $
        fmap normalizeVoice $
          voiceOrder $
            musicToVoices $
              fmap (staffPitch (no_staves opts)) music


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

-- | Top-level definitions required by the chosen 'NotationOptions'; must be
-- emitted before the score.
preamble :: forall n. NotationOptions n -> String
preamble opts = case no_staves opts of
  IntrinsicStaff ->
    let n = natVal (Proxy @n) in
    finPreamble
      <> accPreamble
      <> unlines
        [ "intrinsicClef = {"
        , "  \\finStaff #" <> show n
        , "  \\finClef #" <> show n
        , "}"
        ]
  _ -> ""

-- | LilyPond definitions for @Fin n@ staves: @\\finStaff@, @\\finClef@ and
-- @\\finOttava@. The staff has @n `div` 2 + 2@ lines with position 0 on the
-- bottom line; the clef is the numeral @n@ followed by @}:@ straddling
-- position @n@.
finPreamble :: String
finPreamble =
  unlines
    [ "% Generate the staff line positions."
    , "#(define (fin-line-positions n)"
    , "   (let ((count (+ (quotient n 2) 2)))"
    , "     (let loop ((i 0))"
    , "       (if (= i count)"
    , "           '()"
    , "           (cons (+ (- 1 n) (* 2 i))"
    , "                 (loop (+ i 1)))))))"
    , ""
    , "#(define (fin-staff-center n)"
    , "   (/ (+ (- 1 n)"
    , "         (+ (- 1 n) (* 2 (+ (quotient n 2) 1))))"
    , "      2))"
    , ""
    , "% Convert decimal digits to Unicode mathematical double-struck digits."
    , "#(define (fin-blackboard-number n)"
    , "   (list->string"
    , "     (map (lambda (c)"
    , "            (integer->char"
    , "              (+ #x1D7D8 (- (char->integer c) (char->integer #\\0)))))"
    , "          (string->list (number->string n)))))"
    , ""
    , "% Configure the staff."
    , "finStaff ="
    , "#(define-music-function (n) (integer?)"
    , "   #{"
    , "     \\override Staff.StaffSymbol.line-positions = #(fin-line-positions n)"
    , "     \\set Staff.middleCPosition = #( - 1 n)"
    , "   #})"
    , ""
    , "% Configure the clef."
    , "finClef ="
    , "#(define-music-function (n) (integer?)"
    , "   (let ((center (fin-staff-center n)))"
    , "     #{"
    , "       \\override Staff.TimeSignature.Y-offset = #(/ center 2)"
    , "       % Measure clef spacing from its right edge, since it is wide."
    , "       \\override Staff.Clef.space-alist.time-signature = #'(extra-space . 1.5)"
    , "       \\override Staff.Clef.space-alist.key-signature = #'(extra-space . 1.5)"
    , "       \\override Staff.Clef.space-alist.first-note = #'(extra-space . 2.0)"
    , "       \\override Staff.Clef.space-alist.next-note = #'(extra-space . 1.0)"
    , "       \\override Staff.Clef.extra-offset ="
    , "         #(if (>= n 10) '(-0.5 . 0) '(0 . 0))"
    , "       \\override Staff.Clef.stencil ="
    , "         #(lambda (grob)"
    , "            (let* ((digits"
    , "                     (map (lambda (c)"
    , "                            (grob-interpret-markup"
    , "                              grob"
    , "                              (markup #:vcenter #:bold #:fontsize 4"
    , "                                      (fin-blackboard-number"
    , "                                        (- (char->integer c)"
    , "                                           (char->integer #\\0))))))"
    , "                          (string->list (number->string n))))"
    , "                   ;; Kern multi-digit numerals 0.5 closer together."
    , "                   (number"
    , "                     (fold (lambda (d acc)"
    , "                             (ly:stencil-combine-at-edge acc X RIGHT d -0.5))"
    , "                           (car digits)"
    , "                           (cdr digits)))"
    , "                   (colon"
    , "                     (grob-interpret-markup"
    , "                       grob"
    , "                       (markup"
    , "                         #:combine"
    , "                         (markup #:draw-circle 0.2 0 #t)"
    , "                         (markup #:translate '(0 . 1)"
    , "                                 #:draw-circle 0.2 0 #t))))"
    , "                   (x (+ (cdr (ly:stencil-extent number X)) 0.5))"
    , "                   ;; Brace whose point is centered on the colon's midpoint."
    , "                   (brace"
    , "                     (ly:stencil-translate-axis"
    , "                       (grob-interpret-markup"
    , "                         grob"
    , "                         (markup #:vcenter #:fontsize 2 \"}\"))"
    , "                       0.5 Y))"
    , "                   (brace-w (interval-length (ly:stencil-extent brace X)))"
    , "                   (colon-x (+ x brace-w 0.3))"
    , "                   ;; Undo the clef grob's own staff-position offset."
    , "                   (sp (ly:grob-property grob 'staff-position 0))"
    , "                   (dy (- (/ sp 2))))"
    , "              (ly:stencil-translate-axis"
    , "                (ly:stencil-add"
    , "                  (ly:stencil-translate-axis number (/ center 2) Y)"
    , "                  (ly:stencil-add"
    , "                    (ly:stencil-translate-axis brace x X)"
    , "                    (ly:stencil-translate colon (cons colon-x 0))))"
    , "                dy Y)))"
    , "     #}))"
    , ""
    , "finOttava ="
    , "#(define-music-function (n octave) (integer? integer?)"
    , "   (let ((displacement (* n octave)))"
    , "     #{"
    , "       \\ottava #octave"
    , "       \\set Staff.middleCPosition = #( - (+ displacement (- n 1)))"
    , "       \\set Staff.ottavation ="
    , "         #(format #f \"~a~a\""
    , "                  (if (> displacement 0) \"+\" \"\")"
    , "                  displacement)"
    , "     #}))"
    , ""
    ]

-- | LilyPond machinery for generalized accidentals. Notes are annotated with
-- @\\acc #'((scale . steps) ...)@; the pitch itself carries no alteration.
-- @Deno_accidental_engraver@ tracks, per staff and per staff position, the
-- accidental stack in force for the current bar. Whenever any part of a
-- note's stack changes, the whole stack is drawn: changed entries plainly,
-- unchanged ones parenthesized. A stack that resets entirely is drawn as a
-- natural sign.
accPreamble :: String
accPreamble =
  unlines
    [ "#(define (deno-normalize alist)"
    , "   (sort (filter (lambda (kv) (not (zero? (cdr kv)))) alist)"
    , "         (lambda (a b) (< (car a) (car b)))))"
    , ""
    , "#(define (deno-diff prev cur)"
    , "   ;; every scale whose value changed, with its new value (0 = reset)"
    , "   (let* ((scales (delete-duplicates (map car (append prev cur))))"
    , "          (get (lambda (al k) (let ((p (assv k al))) (if p (cdr p) 0)))))"
    , "     (sort"
    , "      (filter-map"
    , "       (lambda (k)"
    , "         (let ((old (get prev k)) (new (get cur k)))"
    , "           (and (not (= old new)) (cons k new))))"
    , "       scales)"
    , "      (lambda (a b) (< (car a) (car b))))))"
    , ""
    , "#(define (Deno_accidental_engraver context)"
    , "   (let ((state (make-hash-table)))"
    , "     (make-engraver"
    , "      ((start-translation-timestep engraver)"
    , "       (let ((mp (ly:context-property context 'measurePosition #f)))"
    , "         (if (and mp (zero? (ly:moment-main mp)))"
    , "             (hash-clear! state))))"
    , "      (listeners"
    , "       ((note-event engraver event)"
    , "        (let* ((pitch (ly:event-property event 'pitch))"
    , "               (key (cons (ly:pitch-octave pitch) (ly:pitch-notename pitch)))"
    , "               (cur (deno-normalize"
    , "                     (ly:event-property event 'deno-accidentals '())))"
    , "               (prev (hash-ref state key '())))"
    , "          (hash-set! state key cur)"
    , "          (let ((diff (deno-diff prev cur)))"
    , "            (if (pair? diff)"
    , "                ;; display entries: (scale steps changed?)"
    , "                ;; = every entry still in force, plus any resets to 0"
    , "                (let* ((resets (filter (lambda (kv) (zero? (cdr kv))) diff))"
    , "                       (shown (sort (append cur resets)"
    , "                                    (lambda (a b) (< (car a) (car b)))))"
    , "                       (display"
    , "                        (map (lambda (kv)"
    , "                               (list (car kv) (cdr kv)"
    , "                                     (and (assv (car kv) diff) #t)))"
    , "                             shown)))"
    , "                  (ly:event-set-property! event 'deno-display"
    , "                    (if (null? cur) 'natural display))"
    , "                  (ly:event-set-property! event 'force-accidental #t))))))))))"
    , ""
    , "%% ---- Tweakable layout parameters (staff spaces unless noted) -------------"
    , "#(define deno-whiteout-thickness 1.1)    % outline whiteout behind digits"
    , "#(define deno-scripts-raise -0.8)        % vertical offset of sub/super block"
    , "#(define deno-super-raise -0.25)         % extra vertical offset of superscript"
    , "#(define deno-single-digit-dx -0.2)      % nudge for 1-digit subscripts"
    , "#(define deno-sharp-dx 0.35)             % scripts after a sharp"
    , "#(define deno-natural-dx 0.45)           % scripts after a natural"
    , "#(define deno-flat-dx 0)                 % scripts after a flat"
    , "#(define deno-stack-padding 0.15)        % padding around the whole stack"
    , "#(define deno-stack-fontsize -2)         % font size of the whole stack"
    , ""
    , "%% 'below: scale size centred under the glyph, step count centred above it."
    , "%% 'right: scripts to the right of the glyph."
    , "#(define deno-script-layout 'below)"
    , "#(define deno-below-gap 0.1)             % glyph bottom -> subscript top"
    , "#(define deno-above-gap 0.1)             % glyph top -> superscript bottom"
    , "#(define deno-below-script-size -3)      % relative fontsize of below scripts"
    , "#(define deno-sharp-below-dx 0)          % horizontal nudges under each glyph"
    , "#(define deno-flat-below-dx -0.15)       % (flat's bowl is left of its box centre)"
    , "#(define deno-natural-below-dx 0)"
    , ""
    , "#(define (deno-glyph n k)"
    , "   (cond"
    , "    ((zero? k) (make-natural-markup))"
    , "    (else (if (> k 0) (make-sharp-markup) (make-flat-markup)))))"
    , ""
    , "#(define (deno-whiteout m)"
    , "   (make-override-markup (cons 'thickness deno-whiteout-thickness)"
    , "     (make-whiteout-markup m)))"
    , ""
    , "%% Place `scripts` just right of `glyph`, offset by (dx, dy). The scripts are"
    , "%% added after the glyph so their whiteout is drawn on top of it."
    , "#(define-markup-command (deno-glyph-scripts layout props glyph scripts dx dy)"
    , "   (markup? markup? number? number?)"
    , "   (let* ((g (interpret-markup layout props glyph))"
    , "          (s (interpret-markup layout props scripts))"
    , "          (w (cdr (ly:stencil-extent g X))))"
    , "     (ly:stencil-add g (ly:stencil-translate s (cons (+ w dx) dy)))))"
    , ""
    , "%% Centre `sub` under `glyph` and `super` (may be #f) above it. The scripts"
    , "%% are added last so their whiteout covers the glyph where they touch."
    , "#(define-markup-command (deno-glyph-below layout props glyph sub super dx)"
    , "   (markup? markup? scheme? number?)"
    , "   (let* ((g  (interpret-markup layout props glyph))"
    , "          (gx (ly:stencil-extent g X))"
    , "          (gy (ly:stencil-extent g Y))"
    , "          (cx (+ (interval-center gx) dx))"
    , "          (place"
    , "           (lambda (m above?)"
    , "             (let* ((s  (interpret-markup layout props m))"
    , "                    (sx (ly:stencil-extent s X))"
    , "                    (sy (ly:stencil-extent s Y))"
    , "                    (x  (- cx (interval-center sx)))"
    , "                    (y  (if above?"
    , "                            (- (+ (cdr gy) deno-above-gap) (car sy))"
    , "                            (- (- (car gy) deno-below-gap) (cdr sy)))))"
    , "               (ly:stencil-translate s (cons x y)))))"
    , "          (out (ly:stencil-add g (place sub #f))))"
    , "     (if super (ly:stencil-add out (place super #t)) out)))"
    , ""
    , "#(define (deno-acc-markup entry)"
    , "   (let* ((n (car entry)) (k (cadr entry)) (changed? (caddr entry))"
    , "          (count (abs k))"
    , "          (sub (make-sub-markup"
    , "                (deno-whiteout (number->string n))))"
    , "          (scripts"
    , "           (if (> count 1)"
    , "               (make-left-align-markup"
    , "                (make-overlay-markup"
    , "                 (list (make-center-align-markup"
    , "                        (make-translate-markup (cons 0 deno-super-raise)"
    , "                          (make-super-markup"
    , "                           (deno-whiteout (number->string count)))))"
    , "                       (make-center-align-markup sub))))"
    , "               sub))"
    , "          (dx (+ (if (< n 10) deno-single-digit-dx 0)"
    , "                 (cond ((> k 0) deno-sharp-dx)"
    , "                       ((zero? k) deno-natural-dx)"
    , "                       (else deno-flat-dx))))"
    , "          (m (if (eq? deno-script-layout 'below)"
    , "                 (make-deno-glyph-below-markup"
    , "                  (deno-glyph n k)"
    , "                  (make-fontsize-markup deno-below-script-size"
    , "                    (deno-whiteout (number->string n)))"
    , "                  (and (> count 1)"
    , "                       (make-fontsize-markup deno-below-script-size"
    , "                         (deno-whiteout (number->string count))))"
    , "                  (cond ((> k 0) deno-sharp-below-dx)"
    , "                        ((zero? k) deno-natural-below-dx)"
    , "                        (else deno-flat-below-dx)))"
    , "                 (make-deno-glyph-scripts-markup"
    , "                  (deno-glyph n k) scripts dx deno-scripts-raise))))"
    , "     (if changed? m (make-parenthesize-markup m))))"
    , ""
    , "#(define (deno-accidental-stencil grob)"
    , "   (let* ((ev (event-cause grob))"
    , "          (diff (and ev (ly:event-property ev 'deno-display '()))))"
    , "     (cond"
    , "      ((eq? diff 'natural)"
    , "       (grob-interpret-markup grob (markup #:natural)))"
    , "      ((pair? diff)"
    , "       (grob-interpret-markup grob"
    , "         ;; no vcenter: keep each glyph's native origin so it sits on the"
    , "         ;; note's staff position regardless of the scripts above/below it"
    , "         (markup #:pad-markup deno-stack-padding"
    , "                 #:sans #:fontsize deno-stack-fontsize"
    , "                 (make-line-markup (map deno-acc-markup diff)))))"
    , "      (else (ly:accidental-interface::print grob)))))"
    , ""
    , "%% \\acc #'((7 . 1) (12 . -1)) c4"
    , "acc ="
    , "#(define-music-function (alist note) (list? ly:music?)"
    , "   (set! (ly:music-property note 'deno-accidentals) alist)"
    , "   note)"
    , ""
    , "\\layout {"
    , "  \\context {"
    , "    \\Staff"
    , "    \\consists #Deno_accidental_engraver"
    , "    \\override Accidental.stencil = #deno-accidental-stencil"
    , "    \\override AccidentalCautionary.stencil = #deno-accidental-stencil"
    , "    %% Stack chord accidentals by their actual ink, not bounding boxes."
    , "    \\override Accidental.horizontal-skylines ="
    , "      #ly:grob::horizontal-skylines-from-stencil"
    , "    \\override AccidentalCautionary.horizontal-skylines ="
    , "      #ly:grob::horizontal-skylines-from-stencil"
    , "  }"
    , "}"
    , ""
    ]

footer :: String
footer =
  unlines
    [ ""
    , "\\paper {"
    , "ragged-last = ##t"
    , "}"
    ]

