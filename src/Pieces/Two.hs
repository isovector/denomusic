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
  VS ->
    [ note 0.25 $ T 0 0 0 0
    , rest 0.50
    , note 0.25 $ T 0 0 0 0
    ]
  VA ->
    [ rest 0.25
    , note 0.25 $ T 0 0 0 0
    , note 0.25 $ T 1 0 0 0
    , note 0.25 $ T 0 0 0 0
    ]
  VT ->
    [ note (1/3) $ T 0 2 0 0
    , note (1/3) $ T 0 1 0 0
    , note (1/3) $ T 0 0 0 0
    ]
  VB -> [ note 1 $ mempty ]


main :: IO ()
main = do
  toPdf score
  play score

score =
  harmonize
  $ line
  $ take 6
  $ iterate (lmap permute)
  $ fmap S.singleton
  $ music


harmonize :: Music V (Set T) -> Music V (Set (Reg PitchClass))
harmonize =
  quadruple
    (S.fromList [A, Af, B, Bf, C, D, Df, E, Ef, F, G, Gf])
    (\case
      VB -> Reg 3 C
      VT -> Reg 4 C
      VA -> Reg 4 E
      VS -> Reg 4 G
    )
    (pure mempty)
    (pure $ S.fromList [C, D, E, F, G, A, B]
    )
    (pure mempty)

