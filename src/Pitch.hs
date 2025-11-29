{-# LANGUAGE DerivingStrategies #-}

module Pitch where

data PitchName = C | D | E | F | G | A | B
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data Pitch = Pitch
  { p_register :: Int
  , p_name :: PitchName
  , p_accidental :: Int
  }

instance Ord Pitch where
  compare (Pitch r1 pc1 a1) (Pitch r2 pc2 a2) =
    mconcat
      [ compare r1 r2
      , compare (normalize pc1 a1) (normalize pc2 a2)
      ]

data PitchClass = PC | PCs | PD | PDs | PE | PF | PFs | PG | PGs | PA | PAs | PB
  deriving stock (Eq, Ord, Show, Enum, Bounded)


enumMod :: forall a. (Enum a, Bounded a) => Int -> a -> a
enumMod a pn = toEnum $ mod (fromEnum pn + a) (fromEnum (maxBound @a) + 1)

normalize :: PitchName -> Int -> PitchClass
normalize pn a = enumMod a $ toPc pn

toPc :: PitchName -> PitchClass
toPc C = PC
toPc D = PD
toPc E = PE
toPc F = PF
toPc G = PG
toPc A = PA
toPc B = PB
