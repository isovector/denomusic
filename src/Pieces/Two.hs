module Pieces.Two where

import Data.Set qualified as S
import Music2

data V = VS | VA | VT | VB
  deriving stock (Eq, Ord, Show, Enum, Bounded)

permute :: V -> V
permute VB = VA
permute VA = VS
permute VS = VT
permute VT = VB


music :: Music V (Set PitchClass)
music = fromVoices . fmap line $ \case
  VS -> replicate 4 $ note 0.25 $ S.singleton C
  VA -> replicate 2 $ note 0.5 $ S.singleton E
  VT -> replicate 3 $ note (1/3) $ S.singleton G
  VB -> replicate 1 $ note 1 $ S.singleton C


main :: IO ()
main = toPdf @V $ (harmonize <*>) $ mapWithVoice (\v -> S.map (Reg $ vreg v)) $
  line $ take 5 $ iterate (lmap permute) music


harmonize :: Music V (Set (Reg PitchClass) -> Set (Reg PitchClass))
harmonize = everyone $ line
  [ note 1 id
  , note 1 $ S.map $ fmap succ
  , note 0.5 id
  , note 0.5 $ S.map $ fmap succ
  , note (1/3) $ S.map $ fmap pred
  , note (2/3) $ S.map $ fmap succ
  , note 1 $ S.map $ fmap $ pred . pred
  , note 1 id
  ]


vreg :: V -> Int
vreg VS = 5
vreg VA = 4
vreg VT = 3
vreg VB = 2
