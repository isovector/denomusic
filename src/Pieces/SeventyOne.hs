{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DerivingStrategies #-}

module Pieces.SeventyOne where

import Pieces.SeventyTwo (waldstein, cadences)
import Data.Semigroup
import Data.Group
import Data.Foldable
import Music
import qualified Data.Set as S

cchord :: Music
cchord = simul
  [ reharmonize (register (-2)) $ voice 0 $ simul
      [ note 1 mempty
      , note 1 $ chordTone 1
      , note 1 $ chordTone 2
      ]
  , reharmonize (register (-1)) $ voice 2 $ waldstein
  ]

score :: Music
score =  do
  let t = (T (4) (-2) 0 0)
  stimes 3 $ mconcat
    [
      mconcat
        [ move $ register 1
        , cchord
        , move t
        , cchord
        , move t
        , simul
            [ reharmonize (register (-1)) $ voice 1 $ stretchTo 1 $ cadences
            , cchord
            ]
        , move t
        , cchord
        , move $ T 2 (-1) 0 0
        , cchord
        , move $ T 2 (-1) 0 0
        , cchord
        , move $ T 2 (-1) 0 0
        , cchord
        ]
    , move $ T (-7) 1 0 1
    ]


main :: IO ()
main = do
  toPdf score
  play score

