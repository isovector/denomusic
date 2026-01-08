{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE OverloadedLists #-}

module Pieces.NoChoices where

import Data.Map.Monoidal qualified as MM
import Data.Function.Step.Discrete.Open qualified as SF
import Data.Set qualified as S
import GHC.TypeLits
import DenoMusic
import DenoMusic.Types
import DenoMusic.Harmony

data V = VS | VA | VT | VB
  deriving stock (Eq, Ord, Show, Bounded, Enum)

instance HasPurpose V where
  purpose VS = MelodicVoice
  purpose VB = RootVoice
  purpose VA = HarmonicVoice
  purpose VT = HarmonicVoice

seventh :: MetaScale 4
seventh = UnsafeMetaScale $ [0, 2, 4, 6]

type M = T '[4, 7, 12]


longer :: (Finite v, Semigroup a) => Music v a -> Music v a -> Music v a
longer m1 m2 =
  let d = max (duration m1) (duration m2)
   in liftA2 (<>) (repeatFor d m1) (repeatFor d m2)

stronger :: (Finite v, Semigroup a) => Music v a -> Music v a -> Music v a
stronger m1 m2 =
  let d = max (duration m1) (duration m2)
   in (<>) (repeatFor d m1) (repeatFor d m2)

data Above = Above | Below
  deriving stock (Eq, Ord, Show)

discrete :: [(Rational, a)] -> Music () a
discrete as = Music (sum $ fmap fst as) $ MM.singleton () $ Voice $ SF.fromList ((0, Nothing) : fiddle as) Nothing

fiddle :: [(Rational, a)] -> [(Rational, Maybe a)]
fiddle = go 0
  where
    go _ [] = []
    go t ((d, a) : as) = (t + d, Just a) : go (t + d) as


abovely :: Music () Above
abovely = discrete $ take 100 $ cycle [(1, Above), (1, Below)]

scalely :: Music () Int
scalely = discrete $ take 100 $ fmap (1, ) [7,6.. -7]

harmonic :: Music V M
harmonic = constrainBass $
  ( \v ab s ->
      case ab of
        Above ->
          case v of
            VS -> [3, s, 0]
            VB -> [2, s - 7, 0]
            VA -> [1, s, 0]
            VT -> [0, s, 0]
        Below ->
          case v of
            VA -> [3, s, 0]
            VT -> [2, s, 0]
            VS -> [1, s + 7, 0]
            VB -> [0, s, 0]
  ) <$> setDuration 100 voices <*> everyone abovely <*> everyone scalely

reregister
  :: forall s c ns
  . (KnownNat s, Monoid (T ns))
  => MetaScale c
  -> (Int, Int)
  -> T (c ': s ': ns) -> T (c ': s ': ns)
reregister ms (lo, hi) t =
  let sd :> _ = kill ms t
      ssize = fromIntegral (natVal @s undefined)
    in mconcat
        [ t
        , extend $ sink (clampMod ssize (lo, hi) sd :> Nil)
        ]
clampMod :: Integral n => n -> (n, n) -> n -> n
clampMod n (lo, hi) x
  | x >= lo && x <= hi = 0
  | otherwise = lo + ((x - lo) `mod` n) - x

constrainBass :: Music V M -> Music V M
constrainBass = withVoice $ \case
  VB -> fmap $ reregister seventh (-12, -3)
  VT -> fmap $ reregister seventh (-5, 2)
  VA -> fmap $ reregister seventh (0, 7)
  VS -> fmap $ reregister seventh (8, 14)


setDuration :: Rational -> Music v a -> Music v a
setDuration d (Music _ m) = Music d m


waldstein :: Music () M
waldstein =
  line
    [ [0, 0, 0] <$ eighth
    , [1, 0, 0] <$ eighth
    , [2, 0, 0] <$ eighth
    , [2, -1, 0] <$ eighth
    , [2, 0, 0] <$ eighth
    , [0, 0, 0] <$ eighth
    , [1, 0, 0] <$ eighth
    , [0, 0, 0] <$ eighth
    ]

music :: Music V M
music =
  longer harmonic $ foldr1 @[] stronger
    [ voice VA $ line
        [ neighbor 1 (<> [1, 0, 0])  $ stretch 4 $ fmap invert waldstein
        ]
    , voice VT $ line
        [ fmap (<> [-1, 0, 0]) $ stretch 2 waldstein
        ]
    , voice VB $ line
        [ waldstein
        , fmap invert waldstein
        ]
    , voice VS $ line
      [ [0, 3, 0] <$ quarter
      , [0, 2, 0] <$ quarter
      , [0, 1, 0] <$ quarter
      , [0, 0, 0] <$ quarter
      ]
    -- , voice VT $ line
    --     [ rest 0.25
    --     , mempty <$ eighth
    --     , [-1, 0, 0] <$ eighth
    --     , rest 0.25
    --     , rest 0.25
    --     ]

    ]

--   fromVoices $ \case
--     VA -> alternating $ repeatFor 1 $ alternating $ mempty <$ quarter
--     VT -> repeatFor 2 $ line
--       [ neighbor 1 (<> [-1, 0, 0]) $ mempty <$ quarter
--       , rest 0.25
--       , neighbor 1 (<> [-1, 0, 0]) $ mempty <$ eighth
--       ]


-- | Generate sheet music and play the given 'Music'.
main
  :: IO ()
main = do
  let root = Reg 4 C
  let score = fmap (S.singleton . elim (MSCons seventh $ MSCons diatonic spelledFlat) root) $ music
  toPdf score
  play score


