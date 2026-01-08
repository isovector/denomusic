{-# LANGUAGE OverloadedLists #-}

module Pieces.Contour where

import DenoMusic
import DenoMusic.Utils

scalar :: RealFrac a => a -> Maybe (T '[3, 7, 12])
scalar = Just . pow [0, 1, 0] . round

chordal :: RealFrac a => a -> Maybe (T '[3, 7, 12])
chordal = Just . pow [1, 0, 0] . round

motif1 :: Music () (T '[3, 7, 12])
motif1 =
  line
    [ [0, 0, 0] <$ quarter
    , [0, 1, 0] <$ quarter
    , [0, 0, 0] <$ eighth
    , [0, -1, 0] <$ eighth
    , [0, -3, 0] <$ eighth
    , [0, -5, 0] <$ eighth
    ]

main :: IO ()
main = defaultMain (Reg 4 C) $ trimStart 0 $ liftA2 (<>) (everyone $ note 5 mempty ## note 100 (sink $ dorian @_ @12 diatonic)) $ fromVoices $ \case
  MelodicVoice -> fmap (<> [0, 14, 0]) $
    line
      [ motif1
      , [0, -5, 0] <$ note (6 / 8) ()
      , [0, -6, 0] <$ quarter
      , [0, -4, 0] <$ dotted quarter
      , [0, -5, 0] <$ eighth
      , [0, -5, 0] <$ quarter
      , rest 0.25
      -- 4
      , fmap ((<> [0, -4, 0])) motif1
      , [0, -10, 0] <$ quarter
      , [0, -9, 0] <$ quarter
      , neighbor 1 (<> [0, 0, -1]) $ [0, -5, 0] <$ quarter
      , [0, -3, 0] <$ eighth
      , [0, -4, 0] <$ eighth
      , [0, -5, 0] <$ eighth
      , [0, -6, 0] <$ eighth
      , [1, -7, 0] <$ eighth
      , [1, -8, 0] <$ eighth
      , [1, -9, 0] <$ eighth
      , [1, -10, 0] <$ eighth
      , [2, -11, 0] <$ eighth
      , [2, -12, 0] <$ eighth
      , [2, -13, 0] <$ eighth
      , [2, -14, 0] <$ eighth
      , [2, -14, 0] <$ quarter
      , [2, -15, 1] <$ quarter
      , [4, -15, 1] <$ quarter
      , [4, -16, 0] <$ quarter
      , rest 0.5
      , [2, -15, 0] <$ quarter
      , [2, -17, 1] <$ quarter
      , [4, -15, 1] <$ quarter
      , [4, -16, 0] <$ quarter
      , rest 0.5
      , [2, -16, 0] <$ quarter
      , [2, -17, 0] <$ quarter
      , [0, -14, 0] <$ whole
      -- , [4, -16, 0] <$ dotted quarter
      -- , [4, -17, -1] <$ eighth
      -- , [4, -17, 0] <$ half
      ]
  HarmonicVoice -> fmap (<> [1, 0, 0]) $ line
    [ [1, 0, 0] <$ whole
    , [1, -4, 0] <$ eighth
    , [2, -4, 0] <$ eighth
    , [1, -3, 0] <$ quarter
    , [2, -4, 0] <$ eighth
    , [3, -4, 0] <$ eighth
    , [2, -3, 0] <$ quarter
    , rest 1
    , rest 1
    , [0, -3, 0] <$ eighth
    , [0, -3, 0] <$ eighth
    , [0, -4, 0] <$ eighth
    , [0, -4, 0] <$ eighth
    , [0, -5, 0] <$ quarter
    , [0, -7, 0] <$ quarter
    -- 6
    , [-3, 0, 0] <$ whole
    , [0, 0, 0] <$ half
    , [-1, 0, 0] <$ half
    , [-1, 0, 0] <$ whole
    -- 8
    , rest 1
    , rest 1
    , [-3, 0, 0] <$ whole
    ]
  RootVoice -> line
    [ [0, 0, 0] <$ quarter
    , [0, -3, 0] <$ eighth
    , [0, -3, 0] <$ eighth
    , [0, -2, 0] <$ quarter
    , [0, -5, 0] <$ eighth
    , [0, -5, 0] <$ eighth
    -- 2
    , [0, -4, 0] <$ quarter
    , [0, -6, 0] <$ quarter
    , [0, -2, 0] <$ eighth
    , [0, -2, 0] <$ eighth
    , [0, -3, 0] <$ quarter
    , [0, -2, 0] <$ quarter
    , [0, 1, 0] <$ quarter
    , [0, 0, 0] <$ half
    -- 4
    , [0, -2, 0] <$ quarter
    , [0, -5, 0] <$ quarter
    , [0, -6, 0] <$ quarter
    , [0, -7, 0] <$ quarter
    -- 5
    , [-1, -7, 0] <$ half
    , [-1, -5, 0] <$ half
    -- 6
    , [0, -7, 0] <$ whole
    , [2, -7, 0] <$ half
    , [1, -7, 0] <$ half
    , [-1, -7, 0] <$ whole
    , [0, -6, 0] <$ half
    , [-1, -7, -1] <$ half
    -- 7
    , [-1, -8, 0] <$ half
    , [-1, -7, 0] <$ half
    -- 8
    , [0, -14, 0] <$ whole
    ]
  _ -> mempty

-- contour
--   ( \(fromRational -> (* 2) -> t) ->
--       mconcat
--         -- [ scalar $ (* 3) $
--         --     ( cos (fromRational t)
--         --     )
--         [ scalar $ (t * t * t * t - (4 * t * t * t) + 2 * t + 7)
--         ]

--   )
--   (21/16)
--   (1/8) ## note (3/8) [0, -9, 0]
