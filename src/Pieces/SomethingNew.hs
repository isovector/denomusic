{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE OverloadedLists #-}

module Pieces.SomethingNew where

import Data.Map qualified as M
import Data.Map (Map)
import Unsafe.Coerce
import Data.Kind
import GHC.TypeLits
import Data.Set qualified as S
import DenoMusic
import DenoMusic.Types
import DenoMusic.Harmony
import DenoMusic.Utils


data SATB = VS | VA | VT | VB
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type W = T '[4, 7, 12]


type Setting :: [Nat] -> Type
data Setting t = Setting
  { sch :: MetaScales t PitchClass
  , sro :: Reg PitchClass
  }

unset :: Setting t -> Music SATB (T t) -> Music SATB (Set (Reg PitchClass))
unset (Setting ms ro) = fmap $ S.singleton . flip (elim ms) ro

seventh :: MetaScale 4
seventh = UnsafeMetaScale $ S.fromList [0, 2, 4, 6]

mixo :: MetaScale 7
mixo = UnsafeMetaScale $ S.fromList [0, 2, 4, 5, 7, 9, 10]

vl :: W
vl = [-4, 3, 0]

bars2 :: Music SATB W
bars2 = fromVoices $ \case
    VB ->
      line
        [ note (3/4) [-4, 0, 0]
        , note (1/4) [-3, 0, 0]
        -- bar
        , note 1 [-4, 0, 0]
        ]
    VA ->
      line
        [ note (1/8) [3, 0, 0]
        , note (1/8) [2, 0, 0]
        , note (1/8) [2, 1, 0]
        , note (1/8) [1, 1, 0]
        , note (1/8) [2, 0, 0]
        , note (1/8) [1, 0, 0]
        , note (1/4) [0, 0, 0]
        -- bar
        , note (5/8) [-1, 0, 0]
        , note (1/8) [1, 0, 0]
        , note (1/4) [0, 0, 0]
        ]
    _ -> mempty


bits :: Music SATB W
bits = fromVoices $ \case
  VS -> line
    [ note (1/4) [5, 0, 0]
    , note (1/4) [1, 1, 0]
    , note (1/4) [4, 1, 0]
    , note (1/4) [2, 0, 0]
    , note (1/4) [1, 0, 0]
    , note (1/4) [2, 0, 0]
    , note (1/4) [4, 0, 0]
    , note (1/4) [2, 0, 0]
    ]
  -- VB -> line
  --   [ rest 1
  --   , note (1/4) mempty
  --   ]
  _ -> mempty

notRoot :: Music SATB W -> Music SATB W
notRoot m = fromVoices $ \v ->
  case v of
    VB -> mempty
    _ -> voiceV () $ getVoices m v

cool1 :: Music SATB (Set (Reg PitchClass))
cool1 = line
  [ unset
      (Setting
        (MSCons seventh $ MSCons mixo spelledFlat)
        (Reg 4 C)
      ) $ line
        [ bars2
        , (bars2 <> bits)
        , liftA2 (<>) (notRoot (everyone $ pure [1, 0, 0]) <> voice VB (pure [-1, 0, 0])) (bars2 <> bits)
        ]
  , unset
      (Setting
        (MSCons triad $ MSCons mixo spelledFlat)
        (Reg 4 A)
      ) $ fmap (mappend [-2, 0, 0] . unsafeCoerce) $ line
        [ bars2
        , (bars2 <> bits)
        , liftA2 (<>) (notRoot (everyone $ pure [1, 0, 0]) <> voice VB (pure [-1, 0, 0])) (bars2 <> bits)
        ]
  ]

killMe :: T '[3, 7, 12] -> T '[3, 7, 12]
killMe = sink . kill standard

chordy :: Music SATB (Set (T '[3, 7, 12]))
chordy = fmap (S.map killMe) $ fromVoices $ \case
  VB -> line
    [ note 1 $ S.fromList [[-6, 0, 0], [-6, 0, 7]]
    , note 1 $ S.fromList [[-6, 1, 0], [-6, 1, 7]]
    , note 1 $ S.fromList [[-6, 2, 0], [-6, 2, 7]]
    -- , note 1 $ S.fromList [[-7, 1, 0], [-7, 1, 7]
    ]
  VT -> line
    [ note 1 $ S.singleton [0, (-1), 0]
    , note 1 $ S.singleton [(-1), (-1), 1]
    , note 1 $ S.singleton [(-1), (-1), 1]
    -- , note 1 $ S.singleton [(-2), 0, 0]
    ]
  VA -> line
    [ note 1 $ S.singleton [1, 0, 0]
    , note 1 $ S.singleton [(-1), 1, 0]
    , note 1 $ S.singleton [0, -1, 0]
    -- , note 1 $ S.singleton [0, -3, 1]
    ]
  VS -> line
    [ note 1 $ S.singleton [2, 0, 0]
    , note 1 $ S.singleton [0, 1, 0]
    , note 1 $ S.singleton [0, 1, 1]
    -- , note 1 $ S.singleton [0, 0, 1]
    ]
  _ -> mempty

swapper :: SATB -> SATB
swapper VB = VB
swapper VS = VT
swapper VA = VS
swapper VT = VA

-- T seems to be a "within the harmony" index
-- and perhaps completely irrelevant for moving between harmony

main :: IO ()
main = do
  let s = liftA2 const (line
            [ fmap (S.map $ \t -> elim (MSCons triad $ MSCons diatonic $ spelledSharp) t (Reg 4 G)) $ chordy
            , fmap (S.map $ \t -> elim (MSCons triad $ MSCons diatonic $ spelledSharp) t (Reg 4 E)) $ chordy
            , fmap (S.map $ \t -> elim (MSCons triad $ MSCons diatonic $ spelledSharp) t (Reg 5 Cs)) $ chordy
            , fmap (S.map $ \t -> elim (MSCons triad $ MSCons diatonic $ spelledSharp) t (Reg 4 A)) $ chordy
            , fmap (S.map $ \t -> elim (MSCons triad $ MSCons diatonic $ spelledSharp) t (Reg 4 F)) $ chordy
            ]) $ (everyone $ pure id)
        -- $ (##) chordy
        -- $ flip withVoice (lmap swapper chordy) $ \case
        --     VB -> fmap $ S.fromList . zipWith (<>) [[0, -2, 0], [0, -2, 0]]  . S.toList
        --     VT -> fmap (S.map ([0, -7, 0] <>))
        --     VA -> fmap (S.map ([0, -4, 1] <>))
        --     VS -> fmap (S.map ([0, -2, 1] <>))
  toPdf s
  play s
