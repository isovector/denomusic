module Pieces.SeventyTwo where

import Score
import APitch2
import APitch hiding (APitch)
import Data.Music.Lilypond.Pitch
import Control.Monad.State


without :: Int -> [a] -> [a]
without _ [] = []
without 0 (_ : as) = as
without n (a : as) = a : without (n - 1) as

score :: Score Pitch
score = scale 0.25 $ mconcat $ do
  tone <- [C, B, E, A, D]
  idx <- [0, 4, 2, 1]
  let sc = Scale major (Degree (Scale major $ pure tone) idx)
      ch =
        [ Degree sc 0
        , Degree sc 2
        , Degree sc 4
        , Degree sc 6
        ]

  (i, which) <- zip [0..] [1,3,1,2]
  pure $ fmap eval $
    mconcat
      [ chord $ fmap (fmap $ \pn -> Pitch (pn, 0, 2)) $ take 1 $ drop i ch
      , delay (-1)
      , chord $ fmap (fmap $ \pn -> Pitch (pn, 0, 3)) $ without which ch
      , delay (-1)
      , chord $ fmap (fmap $ \pn -> Pitch (pn, 0, 4)) $ take 1 $ drop which ch
      ]

main :: IO ()
main = playScore score
