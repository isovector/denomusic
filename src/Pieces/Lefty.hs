{-# LANGUAGE OverloadedLists #-}

module Pieces.Lefty where

import Data.Foldable
import Data.List (inits)
import Data.Map qualified as M
import Data.Function.Step.Discrete.Open (SF(..))
import Data.Map.Monoidal qualified as MM
import DenoMusic.NotationBackend (toPitch)
import Data.Music.Lilypond.Pitch qualified as P
import Data.Set qualified as S
import DenoMusic
import DenoMusic.Notation
import DenoMusic.Types
import Data.Lilypond (Score(..), ChordMod(..))
import DenoMusic.Harmony (ChordType(..), chordName, TriadQuality(..))


type M = (T '[3, 7, 12])

h :: Music () ()
h = note (1/3) ()

q :: Music () ()
q = note (1/6) ()

e :: Music () ()
e = note (1/12) ()

bar :: M -> M -> Music () M -> Music VoicePurpose M
bar root top melody = fromVoices $ \case
  RootVoice -> line
    [ note (duration e * 6) $ [0, -7, 0] <> root
    ]
  HarmonicVoice -> line
    [ rest $ duration e
    , [1, -7, 0] <$ e
    , [2, -7, 0] <$ e
    , top <$ e
    , [2, -7, 0] <$ e
    , [1, -7, 0] <$ e
    ]
  MelodicVoice -> melody
  _ -> rest 0

liner :: (Finite v, Monoid a) => [Either a (Music v a)] -> Music v a
liner [] = mempty
liner (Left a : as) = fmap (a <>) $ liner as
liner (Right a : as) = a ## liner as

vl3in7' :: T '[3, 7, 12]
vl3in7' = extend (pow vl3in7 (-1)) -- <> sink (vl7in12) -- 0 :> (-1) :> 0 :> mempty

harmony :: [T '[3, 7, 12]]
harmony = fmap fold $ inits $
  [ invert vl3in7'
  , sink $ aeolian diatonic
  , invert vl3in7'
  , invert vl3in7'
  , invert vl3in7'
  , sink $ invert $ aeolian diatonic
  , sink $ lydian diatonic
  ]

music :: Music VoicePurpose M
music = liner
  [ Right $ bar mempty mempty $
      line
        [ rest $ duration e * 3
        , [0, 7, 0] <$ q
        , [0, 7, 0] <$ e
        ]
  , Right $ bar mempty mempty $
      line
        [ rest $ duration e * 3
        , [0, 7, 0] <$ q
        , [0, 6, 0] <$ e
        ]
  , Left $ invert $ vl3in7'
  , Right $ bar mempty mempty $
      line
        [ rest $ duration e * 3
        , [0, 7, 0] <$ q
        , [0, 7, 0] <$ e
        ]
  , Right $ bar mempty mempty $
      line
        [ rest $ duration e * 3
        , [0, 7, 0] <$ q
        , [0, 6, 0] <$ e
        ]
  , Left $ sink $ aeolian diatonic
  , Right $ bar [0, -7, 0] mempty $
      line
      [ rest $ duration e
      , [0, 7, 0] <$ q
      , [0, 8, 0] <$ e
      , [0, 9, 0] <$ q
      ]
  , Right $ bar mempty mempty mempty
  , Left $ invert $ vl3in7'
  , Right $ bar mempty mempty $ delay (- duration e) $
      line
      [ [0, 8, 0] <$ e
      , [0, 9, 0] <$ q
      ]
  , Right $ bar mempty mempty mempty
  , Left $ invert $ vl3in7'
  , Right $ bar mempty mempty $ line
      [ [0, 9, 0] <$ q
      , [0, 8, 0] <$ q
      , [0, 7, 0] <$ q
      ]
  , Right $ bar mempty mempty $ line
      [ [0, 8, 0] <$ e
      , [0, 7, 0] <$ e
      , [0, 6, 1] <$ q
      ]
  , Left $ invert $ vl3in7'
  , Right $ bar mempty mempty $ line
      [ [0, 7, 0] <$ dotted q
      , [0, 8, 0] <$ q
      ]
  , Right $ bar mempty mempty $ line
      [ [0, 5, 0] <$ h
      ]
  , Left $ invert $ sink $ aeolian diatonic
  , Right $ bar mempty mempty $ line
      [ rest $ duration e * 3
      , [0, 7, 0] <$ q
      , [0, 7, 0] <$ e
      ]
  , Right $ bar mempty mempty $ line
      [ rest $ duration e * 3
      , [0, 7, 0] <$ q
      , [0, 7, 0] <$ e
      ]
  , Left $ sink $ lydian diatonic
  , Right $ bar mempty mempty $ line
      [ rest $ duration e * 3
      , [0, 7, 0] <$ q
      , [0, 7, 0] <$ e
      ]
  , Right $ bar mempty mempty $ line
      [ rest $ duration e * 3
      , [0, 8, 0] <$ q
      , [0, 9, 0] <$ e
      ]
  ]

chords :: PitchClass -> Music () (T '[3, 7, 12]) -> Score
chords pc (Music d m) =
  let (Voice (SF mts _)) = m MM.! ()
      ts = M.toList mts
   in ChordMode $ do
        ((start, Just t), (end, _)) <- zip ts $ drop 1 ts ++ [(d, error "oops")]
        let ChordType root quality _ = chordName standardFlat pc t
        pure
          ( end - start
          , toPitch $ Reg 4 root
          , case quality of
              Major -> MajorChord
              Minor -> MinorChord
              Augmented -> AugChord
              Diminished -> DimChord
          )

withChords
  :: Finite v
  => Reg PitchClass
  -- ^ root note
  -> Music () (T '[3, 7, 12])
  -- ^ chords
  -> Music v (T '[3, 7, 12])
  -> IO ()
withChords pc ch m = do
  let sc = fmap (S.singleton . elim standardFlat pc) m
  toPdf' $
    Simultaneous False
      [ chords (unReg pc) ch
      , toLilypondScore sc
      ]
  play sc




main :: IO ()
main = do
  let root = (Reg 4 C)
      m = stretch (6/4) $ trimStart 0 music
  let sc = fmap (S.singleton . elim standardFlat root) m
  toPdf' $
    Simultaneous False
      [ chords C $ line $ fmap (note 1.5) harmony
      , toLilypondScore sc
      ]
  play sc

