module Pieces.Two where

import Data.Semigroup
import DenoMusic.Harmony
import Data.Set qualified as S
import Music2

data V = VS | VA | VT | VB
  deriving stock (Eq, Ord, Show, Enum, Bounded)

permute :: V -> V
permute VB = VA
permute VA = VS
permute VS = VT
permute VT = VB


music :: Music V T
music = fromVoices . fmap line $ \case
  VS -> replicate 4 $ note 0.25 mempty
  VA -> replicate 2 $ note 0.5 mempty
  VT -> replicate 3 $ note (1/3) mempty
  VB -> replicate 1 $ note 1 mempty


main :: IO ()
main = do
  toPdf score
  play score

score =
  harmonize
  $ line
  $ take 5
  $ iterate (lmap permute) music


harmonize :: Music V T -> Music V (Set (Reg PitchClass))
harmonize =
  quadruple
    (S.fromList [A, Af, B, Bf, C, D, Df, E, Ef, F, G, Gf])
    (\case
      VB -> Reg 3 C
      VT -> Reg 4 C
      VA -> Reg 4 E
      VS -> Reg 4 G
    )
    ( line
      [ note 2 mempty
      , note 1 $ T (-9) 2 0 0
      , note (1/3) $ stimes 2 $ T (-9) 2 0 0
      , note (1/3) $ stimes 3 $ T (-9) 2 0 0
      , note (1/3) $ stimes 4 $ T (-9) 2 0 0
      , note 1 $ stimes 4 $ T (-9) 2 0 0
      ]
    )
    (pure $ S.fromList [C, D, E, F, G, A, B]
    )
    ( line
      [ note 1 mempty
      , note 1 $ T 5 (-2) 0 0
      , note 1 $ stimes 2 $ T 5 (-2) 0 0
      , note 1 $ stimes 2 $ T 5 (-2) 0 0
      , note 1 $ stimes 3 $ T 5 (-2) 0 0
      ]
    )

