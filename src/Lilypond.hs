{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : GHC
--
-------------------------------------------------------------------------------------
module Lilypond where

import Control.Lens (Traversal', (%~), unsafePartsOf)
import Data.Maybe
import Control.Arrow ((<<<), (***), first)
import Data.Char
import Data.Functor.Foldable (cata, embed, project)
import Data.Functor.Foldable.TH
import Data.Music.Lilypond.Dynamics (Dynamics)
import Data.Music.Lilypond.Pitch
import Data.Ratio
import Data.String
import Text.PrettyPrint.HughesPJClass hiding (Mode, (<>), first)
import Data.List (minimumBy, unsnoc)
import Data.Ord (comparing)

instance Pretty Pitch where
    pPrint (Pitch (c,a,o)) = text $ pc c ++ acc a ++ oct (o-4)
        where
            pc C = "c" ; pc D = "d" ; pc E = "e" ; pc F = "f"
            pc G = "g" ; pc A = "a" ; pc B = "b"
            acc n | n <  0  =  concat $ replicate (negate n) "es"
                  | n == 0  =  ""
                  | n >  0  =  concat $ replicate (n) "is"
                  | otherwise = error "impossible"
            oct n | n <  0  =  concat $ replicate (negate n) ","
                  | n == 0  =  ""
                  | n >  0  =  concat $ replicate n "'"
                  | otherwise = error "impossible"

instance Pretty Mode where
    pPrint Major = "\\major"
    pPrint Minor = "\\minor"

instance Pretty Dynamics where
    pPrint = text . ("\\" ++) . fmap toLower . show

-- | A Lilypond music expression.
--
--   Use the 'Pretty' instance to convert into Lilypond syntax.
--
data Music
    = Rest Rational [PostEvent]             -- ^ Single rest.
    | Note Note Rational [PostEvent]        -- ^ Single note.
    | Chord [(Note, [PostEvent])] Rational [PostEvent]     -- ^ Single chord.
    | Sequential   [Music]                          -- ^ Sequential composition.
    | Simultaneous Bool [Music]                     -- ^ Parallel composition (split voices?).
    | Repeat Bool Int Music (Maybe (Music, Music))  -- ^ Repetition (unfold?, times, music, alternative).
    | Tremolo Int Music                             -- ^ Tremolo (multiplier).
    | Tuplet Rational (Maybe Int) Music                          -- ^ Stretch music (multiplier).
    | Transpose Pitch Pitch Music                   -- ^ Transpose music (from to).
    | Relative Pitch Music                          -- ^ Use relative octave (octave).
    | Clef Clef                                     -- ^ Clef.
    | Key Pitch Mode                                -- ^ Key signature.
    | Time Integer Integer                          -- ^ Time signature.
    | Tempo (Maybe String) (Maybe (Duration,Integer)) -- ^ Tempo mark.
    | New String (Maybe String) Music               -- ^ New expression.
    | Context String (Maybe String) Music           -- ^ Context expression.
    | Revert String
    deriving stock (Eq, Show)


-- | Articulations. These include ornaments.
data Articulation
    = Accent
    | Marcato
    | Staccatissimo
    | Espressivo
    | Staccato
    | Tenuto
    | Portato
    | Upbow
    | Downbow
    | Flageolet
    | Thumb
    | LeftHeel
    | RightHeel
    | LeftToe
    | RightToe
    | Open
    | Stopped
    | Turn
    | ReverseTurn
    | Trill
    | Prall
    | Mordent
    | PrallPrall
    | PrallMordent
    | UpPrall
    | DownPrall
    | UpMordent
    | DownMordent
    | PrallDown
    | PrallUp
    | LinePrall
    | SignumCongruentiae
    | ShortFermata
    | Fermata
    | LongFermata
    | VeryLongFermata
    | Segno
    | Coda
    | VarCoda
    deriving stock (Eq, Show)

newtype Duration  = Duration Rational
  deriving newtype (Eq, Ord, Num, Enum, Fractional, Real, RealFrac, Show)

data Direction
    = Above
    | Default
    | Below
    deriving stock (Eq, Ord, Show)

data Markup
    = MarkupText String
    | MarkupList [Markup]
    | Bold Markup
    | Box Markup
    | Caps Markup
    | DynamicsFont Markup
    | FingeringFont Markup
    | Fontsize Double Markup
    | Huge Markup
    | Italic Markup
    | Large Markup
    | Larger Markup
    | Magnify Markup
    | Medium Markup
    | Roman Markup
    | Sans Markup
    | Sub Markup
    | Super Markup
    | TextFont Markup
    | Tiny Markup
    | TypewriterFont Markup
    | Upright Markup
    deriving stock (Eq, Show)

data Note
    = NotePitch Pitch
    | DrumNotePitch (Maybe Duration)
    deriving stock (Eq, Show)


data Clef
    = Treble
    | Alto
    | Tenor
    | Bass
    | French
    | Soprano
    | MezzoSoprano
    | Baritone
    | VarBaritone
    | SubBass
    | Percussion
    | Tab
    deriving stock (Eq, Show)

data ChordPostEvent
  = Harmonic
  deriving stock (Eq, Show)

data PostEvent
    = Articulation Direction Articulation
    | Dynamics Direction Dynamics
    | Tie
    | Glissando
    | BeginBeam
    | EndBeam
    | BeginSlur
    | EndSlur
    | BeginPhraseSlur
    | EndPhraseSlur
    | BeginCresc
    | BeginDim
    | EndCrescDim
    | Arpeggio
    | Text Direction String
    | Markup Direction Markup
    deriving stock (Eq, Show)


instance Pretty Music where
    pPrint (Rest d p)       = "r" <> pPrint (Duration d) <> pPrintList prettyNormal p

    pPrint (Note n d p)     = pPrint n <> pPrint (Duration d) <> pPrintList prettyNormal p

    pPrint (Chord ns d p)   = "<" <> nest 4 (sepByS "" $ fmap (uncurry (<>) <<< pPrint *** pPrint) ns) <> char '>'
                                  <> pPrint (Duration d) <> pPrintList prettyNormal p

    pPrint (Sequential xs)  = "{" <+> nest 4 ((hsep . fmap pPrint) xs) <+> "}"

    pPrint (Simultaneous False xs) = "<<" $$ nest 4 ((vcat . fmap pPrint) xs)           $$ ">>"
    pPrint (Simultaneous True xs)  = "<<" $$ nest 4 ((sepByS " \\\\" . fmap pPrint) xs) $$ ">>"

    pPrint (Repeat unfold times x alts) =
        "\\repeat" <+> unf unfold <+> int times <+> pPrint x <+> alt alts
        where
            unf p = if p then "unfold" else "volta"
            alt Nothing      = empty
            alt (Just (z,y)) = "\\alternative" <> pPrint z <> pPrint y

    pPrint (Tremolo n x) =
        "\\repeat tremolo" <+> pPrint n <+> pPrint x

    pPrint (Tuplet n z x) =
        "\\tuplet" <+> frac n <+> maybe "" pPrint z <+> pPrint x
        where
            frac m = pPrint (numerator m) <> "/" <> pPrint (denominator m)

    pPrint (Transpose from to x) =
        "\\transpose" <+> pPrint from <+> pPrint to <+> pPrint x

    pPrint (Relative p x) =
        "\\relative" <+> pPrint p <+> pPrint x

    pPrint (Clef c) = "\\clef" <+> pPrint c

    pPrint (Key p m) = "\\key" <+> pPrint p <+> pPrint m

    pPrint (Time m n) = "\\time" <+> (pPrint m <> "/" <> pPrint n)

    pPrint (Tempo Nothing Nothing)           = mempty
    pPrint (Tempo (Just t) Nothing)          = "\\time" <+> pPrint t
    pPrint (Tempo Nothing (Just (d,bpm)))    = "\\time" <+> pPrint d <+> "=" <+> pPrint bpm
    pPrint (Tempo (Just t) (Just (d,bpm)))   = "\\time" <+> pPrint t <+> pPrint d <+> "=" <+> pPrint bpm

    -- TODO metronome
    -- TODO tempo

    pPrint (New typ name x) =
        "\\new" <+> text typ <+> maybe "" pPrint name <+> pPrint x

    pPrint (Context typ name x) =
        "\\context" <+> text typ <+> pPrint name <+> pPrint x

    pPrint (Revert name) =
        "\\revert" <+> text name

    pPrintList _                    = hsep . fmap pPrint


instance Pretty Note where
    pPrint (NotePitch p)         = pPrint p
    pPrint (DrumNotePitch _)       = error "Non-standard pitch"
    pPrintList _                   = hsep . fmap pPrint

instance Pretty Clef where
    pPrint Treble       = "treble"
    pPrint Alto         = "alto"
    pPrint Tenor        = "tenor"
    pPrint Bass         = "bass"
    pPrint French       = "french"
    pPrint Soprano      = "soprano"
    pPrint MezzoSoprano = "mezzosoprano"
    pPrint Baritone     = "baritone"
    pPrint VarBaritone  = "varbaritone"
    pPrint SubBass      = "subbass"
    pPrint Percussion   = "percussion"
    pPrint Tab          = "tab"

instance Pretty ChordPostEvent where
    pPrint Harmonic = "\\harmonic"

instance Pretty PostEvent where
    pPrint (Articulation d a)   = pPrint d <> pPrint a
    pPrint (Dynamics d a)       = pPrint d <> pPrint a
    pPrint Tie                  = "~"
    pPrint Glissando            = "\\glissando"
    pPrint BeginBeam            = "["
    pPrint EndBeam              = "]"
    pPrint BeginSlur            = "("
    pPrint EndSlur              = ")"
    pPrint BeginPhraseSlur      = "\\("
    pPrint EndPhraseSlur        = "\\)"
    pPrint BeginCresc           = "\\<"
    pPrint BeginDim             = "\\>"
    pPrint EndCrescDim          = "\\!"
    pPrint Arpeggio             = "\\arpeggio"
    pPrint (Text d s)           = pPrint d <> (text . show) s -- add quotes
    pPrint (Markup d m)         = pPrint d <> ("\\markup" <+> pPrint m)
    pPrintList _                = hcat . fmap pPrint

instance IsString Markup where
    fromString = MarkupText

instance Pretty Markup where
    pPrint (MarkupText s)       = (text . show) s
    pPrint (MarkupList as)      = "{" <+> hsep (fmap pPrint as) <+> "}"
    pPrint (Bold a)             = "\\bold" <+> pPrint a
    pPrint (Box a)              = "\\box" <+> pPrint a
    pPrint (Caps a)             = "\\caps" <+> pPrint a
    pPrint (DynamicsFont a)     = "\\dynamics" <+> pPrint a
    pPrint (FingeringFont a)    = "\\fingering" <+> pPrint a
    pPrint (Fontsize n a)       = "\\fontsize" <+> ("#" <> pPrint n) <+> pPrint a
    pPrint (Huge a)             = "\\huge" <+> pPrint a
    pPrint (Italic a)           = "\\italic" <+> pPrint a
    pPrint (Large a)            = "\\large" <+> pPrint a
    pPrint (Larger a)           = "\\larger" <+> pPrint a
    pPrint (Magnify a)          = "\\magnify" <+> pPrint a
    pPrint (Medium a)           = "\\medium" <+> pPrint a
    pPrint (Roman a)            = "\\roman" <+> pPrint a
    pPrint (Sans a)             = "\\sans" <+> pPrint a
    pPrint (Sub a)              = "\\sub" <+> pPrint a
    pPrint (Super a)            = "\\super" <+> pPrint a
    pPrint (TextFont a)         = "\\text" <+> pPrint a
    pPrint (Tiny a)             = "\\tiny" <+> pPrint a
    pPrint (TypewriterFont a)   = "\\typewriter" <+> pPrint a
    pPrint (Upright a)          = "\\upright" <+> pPrint a


instance Pretty Articulation where
    pPrint Accent             = ">"
    pPrint Marcato            = "^"
    pPrint Staccatissimo      = "|"
    pPrint Espressivo         = "\\espressivo"
    pPrint Staccato           = "."
    pPrint Tenuto             = "-"
    pPrint Portato            = "_"
    pPrint Upbow              = "\\upbow"
    pPrint Downbow            = "\\downbow"
    pPrint Flageolet          = "\\flageolet"
    pPrint Thumb              = "\\thumb"
    pPrint LeftHeel           = "\\leftheel"
    pPrint RightHeel          = "\\rightheel"
    pPrint LeftToe            = "\\lefttoe"
    pPrint RightToe           = "\\righttoe"
    pPrint Open               = "\\open"
    pPrint Stopped            = "+"
    pPrint Turn               = "\\turn"
    pPrint ReverseTurn        = "\\reverseturn"
    pPrint Trill              = "\\trill"
    pPrint Prall              = "\\prall"
    pPrint Mordent            = "\\mordent"
    pPrint PrallPrall         = "\\prallprall"
    pPrint PrallMordent       = "\\prallmordent"
    pPrint UpPrall            = "\\upprall"
    pPrint DownPrall          = "\\downprall"
    pPrint UpMordent          = "\\upmordent"
    pPrint DownMordent        = "\\downmordent"
    pPrint PrallDown          = "\\pralldown"
    pPrint PrallUp            = "\\prallup"
    pPrint LinePrall          = "\\lineprall"
    pPrint SignumCongruentiae = "\\signumCongruentiae"
    pPrint ShortFermata       = "\\shortfermata"
    pPrint Fermata            = "\\fermata"
    pPrint LongFermata        = "\\longfermata"
    pPrint VeryLongFermata    = "\\verylongfermata"
    pPrint Segno              = "\\segno"
    pPrint Coda               = "\\coda"
    pPrint VarCoda            = "\\varcoda"
    pPrintList _            = hcat . fmap pPrint

instance Pretty Direction where
    pPrint Above              = "^"
    pPrint Default            = "-"
    pPrint Below              = "_"


-- | Notated time in fractions, in @[2^^i | i <- [-10..3]]@.

instance Pretty Duration where
    pPrint a =
      case separateDots a of
        Just (nv, ds) -> text $ pnv (toRational nv) ++ pds ds
        Nothing -> error $ "can't handle duration " <> show a
        where
            pnv 4 = "\\longa"
            pnv 2 = "\\breve"
            pnv n = show (denominator n)
            pds n = concat $ replicate n "."


sequential :: Music -> Music -> Music
Sequential as `sequential` Sequential bs = Sequential (as <> bs)
Sequential as `sequential` b             = Sequential (as <> [b])
a `sequential` Sequential bs             = Sequential ([a] <> bs)
a `sequential` b                         = Sequential ([a,b])

simultaneous :: Music -> Music -> Music
Simultaneous _ as `simultaneous` Simultaneous _ bs = Simultaneous True (as <> bs)
Simultaneous s as `simultaneous` b                 = Simultaneous s (as <> [b])
a `simultaneous` Simultaneous t bs                 = Simultaneous t ([a] <> bs)
a `simultaneous` b                                 = Simultaneous True ([a,b])






sepByS :: Doc -> [Doc] -> Doc
sepByS d = sep . punctuate d




separateDots :: Duration -> Maybe (Duration, Int)
separateDots = separateDots' [2/3, 6/7, 14/15, 30/31, 62/63]

separateDots' :: [Duration] -> Duration -> Maybe (Duration, Int)
separateDots' [] _ = Nothing
separateDots' (d:divs) nv
    | isDivisibleBy @Double 2 nv = Just (nv,  0)
    | otherwise                  = fmap (fmap (+ 1)) $ separateDots' divs (nv*d)

logBaseR :: forall a . (RealFloat a) => Rational -> Rational -> a
logBaseR k n
    | isInfinite (fromRational n :: a)      = logBaseR k (n/k) + 1
logBaseR k n
    | isDenormalized (fromRational n :: a)  = logBaseR k (n*k) - 1
logBaseR k n                         = logBase (fromRational k) (fromRational n)

isDivisibleBy :: (Real a, Real b) => a -> b -> Bool
isDivisibleBy n = (== 0.0) . snd . properFraction @Double @Integer . logBaseR (toRational n) . toRational



$(makeBaseFunctor [''Music])



removeSingleChords :: Music -> Music
removeSingleChords = cata $ \case
  ChordF [(n,_)] d p -> Note n d p
  x -> embed x


-- | A tree for parsing tuplets into.
data TupletTree a
  = One Rational a
  | Many [TupletTree a]
  | TupletTree Rational (TupletTree a)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Semigroup (TupletTree a) where
  Many t1 <> Many t2 = Many (t1 <> t2)
  Many t1 <> t2 = Many (t1 <> (pure t2))
  t1 <> Many t2 = Many (t1 : t2)
  t1 <> t2 = Many [t1, t2]

instance Monoid (TupletTree a) where
  mempty = Many mempty


-- | Fold over a piece of music, turning series of non-standard-duration notes
-- into tuplets.
makeTuplets :: Music -> Music
makeTuplets = cata $ \case
  SequentialF xs
    | any (not . isDivisibleBy @Double 2 . fromMaybe 2 . getDuration) xs
    -> Sequential $ foldTupletTree $ parseTupletTree $ mapMaybe (\x -> fmap (, x) $ getDuration x) xs
  x -> embed x


-- | For pieces of music that are tupletizable, get their durations.
getDuration :: Music -> Maybe Rational
getDuration = \case
    Note _ b _ -> Just b
    Rest b _ -> Just b
    Chord _ b _ -> Just b
    _ -> Nothing


-- | doesn't target the post events inside of chords
groupedPostEvents :: Traversal' Music [PostEvent]
groupedPostEvents f =
  \case
    RestF a b -> Rest a <$> f b
    NoteF a b c -> Note a b <$> f c
    ChordF a b c -> Chord a b <$> f c
    z -> fmap embed $ traverse (groupedPostEvents f) z
  . project


-- | Fold a tuplet tree back into pieces of sequential music. This essentially
-- compiles it back down to combinations of notes, chords, sequentials and
-- tuplets.
foldTupletTree :: TupletTree Music -> [Music]
foldTupletTree (One d (Chord a _ c)) = pure $ Chord a d c
foldTupletTree (One d (Note a _ c)) = pure $ Note a d c
foldTupletTree (One d (Rest _ c)) = pure $ Rest d c
foldTupletTree One{} = error "foldTupletTree: impossible"
foldTupletTree (Many as) = foldTupletTree =<< as
foldTupletTree (TupletTree d as) = pure $ Tuplet (1 / d) Nothing $ Sequential $ foldTupletTree as

addPhrase :: Music -> Music
addPhrase = unsafePartsOf groupedPostEvents %~ wrapping BeginPhraseSlur EndPhraseSlur

addSlur :: Music -> Music
addSlur = unsafePartsOf groupedPostEvents %~ wrapping BeginSlur EndSlur

addArticulation :: Articulation -> Music -> Music
addArticulation a = groupedPostEvents %~ (Articulation Default a :)

wrapping :: a -> a -> [[a]] -> [[a]]
wrapping _ _ [] = []
wrapping lo hi as
  | Just (as', alast) <- unsnoc as
  = case as' of
      [] -> pure $ lo : hi : alast
      (a0 : as'') -> (lo : a0) : as'' ++ [hi : alast]
  | otherwise = fmap (\xs -> lo : hi : xs) as


-- | Parse a series of notes into a tuplet tree, recursively.
parseTupletTree :: [(Rational, a)] -> TupletTree a
parseTupletTree [] = mempty
parseTupletTree ((d, a) : xs)
  | isDivisibleBy @Double 2 $ denominator d = One d a <> parseTupletTree xs
  | otherwise =
      let (this, that) = partitionIntoPowerOfTwo $ (d, a) : xs
          (mult, this') = tupletize this
       in TupletTree mult (parseTupletTree this') <> parseTupletTree that


-- | Given a token stream, split it into two groups. The first group has the
-- property that its durations sum to a power of two.
partitionIntoPowerOfTwo :: [(Rational, a)] -> ([(Rational, a)], [(Rational, a)])
partitionIntoPowerOfTwo = go 0
  where
    go _ [] = ([], [])
    go total (da@(d, _) : as) =
      case isDivisibleBy @Double 2 $ total + d of
        True -> ([da], as)
        False ->
          let (outa, outb) = go (total + d) as
           in (da : outa, outb)


-- | An algorithm to generate tuplets, as given in
-- https://lists.gnu.org/archive/html/lilypond-user/2008-06/msg00070.html.
tupletize :: [(Rational, a)] -> (Rational, [(Rational, a)])
tupletize rs = do
  let smallest = minimumBy (comparing (isDivisibleBy @Double 2 . denominator) <> comparing denominator) $ fmap fst rs
      power = 2 ^ (floor @Double @Int (logBase 2 (fromIntegral $ denominator smallest) ))
      multiplier = smallest * power
   in (multiplier, fmap (first (/ multiplier)) rs)

