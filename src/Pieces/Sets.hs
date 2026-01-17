module Pieces.Sets where

import DenoMusic.Utils
import Control.Monad.State
import Data.Functor
import Data.Set qualified as S
import DenoMusic
import Data.Traversable

transpose :: (Ord a, Semigroup a) => a -> Set a -> Set a
transpose a = S.map $ (<>) a

harmonize :: Finite v => Music () H -> Music v M -> Music v M
harmonize h m = (\t -> S.map $ (<>) t) <$> everyone h <*> m

type H = T '[3, 7, 12]
type M = Set H

fonte :: Music () H
fonte = line
  [ note 1 $ scaleStep 1
  , note 0.5 $ scaleStep (1 - 3) <> chordStep 1
  , note 0.5 $ scaleStep (1 - 3 - 4) <> chordStep (1 + 1)
  , note 1 $ mempty
  , note 0.5 $ scaleStep (-3) <> chordStep 1
  , note 0.5 $ scaleStep (-3 - 4) <> chordStep (1 + 1)
  ]

harmony :: Music () H
harmony = line
  -- [ fonte
  [ fmap (<> (sink $ mixolydian diatonic)) fonte
  ]

chordStep :: Int -> H
chordStep i = i :> 0 :> 0 :> Nil

scaleStep :: Int -> H
scaleStep i = 0 :> i :> 0 :> Nil

chromaStep :: Int -> H
chromaStep i = 0 :> 0 :> i :> Nil

-- TODO(sandy): export me
manifest :: Music () H -> Music () M
manifest = fmap $ \t -> S.fromList
  [ t
  , t <> chordStep 1
  , t <> chordStep 2
  ]

idea1 :: Music () M
idea1 =
  accum (scaleStep 7)
      (repeatFor 2 $ line
        [ vienneseWaltz
        , alternating eighth
        ]
      )
      $ take 100 $ cycle
          [ chordStep 1
          , chordStep 1
          , scaleStep (1)
          , scaleStep (-1)
          , chordStep (-1)
          , mempty
          , scaleStep (-1)
          , scaleStep (1)
          ]

idea2 :: Music () M
idea2 = stamp (repeatFor 2 $ alternating eighth) $
  accum (scaleStep 7)
      (repeatFor 2 $ line
        [ alternating eighth
        , alternating quarter
        ]
      )
      $ take 100 $ cycle
          [ scaleStep (1)
          , scaleStep (1)
          , scaleStep (1)
          , mempty
          , chordStep (-1)
          ]

cadence1 :: Music () M
cadence1 =
  accum (scaleStep 7)
    (repeatFor 2 $ line
        [ vienneseWaltz
        , alternating eighth
        ]
      )
    [ chordStep 1
    , chordStep 1
    , chordStep (-1)
    , scaleStep (1)
    , scaleStep (-1)
    , mempty
    ]

alberti :: Music () M
alberti = repeatFor 100 $ fmap S.singleton $ line
  [ mempty <$ dotted quarter
  , chordStep 1 <$ eighth
  , chordStep 2 <$ dotted quarter
  , chordStep 1 <$ eighth
  ]

music1 :: Music VoicePurpose M
music1 = transpose <$> everyone (stretch 2 harmony) <*> mconcat
  [ voice RootVoice $ fmap (transpose (scaleStep (-7))) $ fmap S.singleton $
      line
        [ note 1 mempty
        , note 1 mempty
        , note 1 mempty
        , note 1 mempty -- A
        , note 1 (chordStep (-1))
        , note 1 mempty
        , note 1 (chordStep (-1))
        , note 1 (chordStep 1)
        ]
  , voice HarmonicVoice $ alberti
  , voice MelodicVoice $ idea1 ## idea1 ## idea2 ## cadence1
  ]

-- TODO(sandy): export me
accum :: (Ord a, Semigroup a) => a -> Music () () -> [a] -> Music () (Set a)
accum start rh as0 = flip evalState (start, as0) $ for rh $ const $ do
  (cur, as) <- get
  case as of
    [] -> pure mempty
    (a : as') -> do
      let cur' = cur <> a
      put (cur', as')
      pure $ S.singleton cur'


main :: IO ()
main = dmain (Reg 4 C) $ trimStart 0 $ music1

dmain
  :: Finite v
  => Reg PitchClass
  -- ^ "Home" pitch
  -> Music v (Set H)
  -> IO ()
dmain root m = do
  let score =
        fmap (S.map $ \t -> elim standardFlat root t) m
  toPdf score
  play score
