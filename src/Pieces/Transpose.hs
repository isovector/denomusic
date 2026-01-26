{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE OverloadedLists #-}

module Pieces.Transpose where

import Data.Proxy
import Data.Kind
import GHC.TypeLits
import Data.Set qualified as S
import Data.Set (Set)
import DenoMusic hiding (triad)
import DenoMusic.Utils
import Data.Monoid
import Music qualified as M
import DenoMusic.Harmony (toT, VoiceLeading(..))


asc3 :: Music () (Sum Int)
asc3 = line $ do
  i <- [0..7]
  j <- [0..3]
  pure $ note 0.25 $ Sum i

desc8 :: [ScaleDegree 7]
desc8 = do
  i <- [0..7]
  pure $ SD (-i)

data Three = One | Two | Three
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type ScaleDegree :: Nat -> Type
newtype ScaleDegree n = SD Int
  deriving stock (Eq, Ord, Show)


fauxbourdon :: forall n. (KnownNat n, n ~ 7) => ScaleDegree n -> ScaleDegree n
fauxbourdon (SD i) =
  SD $ case mod i (fromIntegral $ natVal $ Proxy @n) of
    0 -> 0
    4 -> 4
    x -> x - 2

fauxbourdonRoot :: forall n. (KnownNat n, n ~ 7) => ScaleDegree n -> ScaleDegree n
fauxbourdonRoot (SD i) =
  SD $ case mod i (fromIntegral $ natVal $ Proxy @n) of
    0 -> 0
    3 -> 3
    4 -> 4
    x -> x - 2

triad :: ScaleDegree n -> Set (T '[n])
triad (SD x) = S.fromList
  [ x :> Nil
  , (x + 2) :> Nil
  , (x + 4) :> Nil
  ]

voiceLeadings :: forall c s. (KnownNat s, KnownNat c) => [ScaleDegree s] -> [T '[c, s]]
voiceLeadings [] = []
voiceLeadings (r@(SD x) : rs) = (0 :> x :> Nil) : go mempty r rs
  where
    go :: T [c, s] -> ScaleDegree s -> [ScaleDegree s] -> [T '[c, s]]
    go t _ [] = []
    go t (SD z) (sd@(SD n) : zs) =
      let dt = toT (VoiceLeading (n - z))
       in (t <> dt) : go  (t <> dt) sd zs

romanesca :: [ScaleDegree 7]
romanesca = [SD 2, SD 6, SD 0, SD 4, SD 2, SD 6, SD 0, SD 4, SD 0]

asVoices :: [Set (T '[7])] -> Music Three (T '[7, 12])
asVoices = line . fmap \s -> do
  let [x, y, z] = S.toList s
  mconcat
    [ voice One $ note 1 $ extend $ x
    , voice Two $ note 1 $ extend $ y
    , voice Three $ note 1 $ extend $ z
    ]

main' :: IO ()
main' = do
  let root = Reg 4 C
      rs = romanesca
      vls = voiceLeadings @3 @7 rs
      score = stretch (1/2)  $
        fmap (S.singleton . elim standardFlat root) $ mconcat
          [ voice One $ line $ fmap (note 1) $ fmap extend vls
          , voice Two $ line $ fmap (note 1 . (<> [1, 0, 0])) $ fmap extend vls
          , voice Three $ line $ fmap (note 1 . (<> [2, 0, 0])) $ fmap extend vls
          ]


  toPdf score
  play score

main :: IO ()
main = defaultMain (Reg 4 C) $ line $ do
  x <- [0..12]
  pure $ line
    [ note 0.25 $ pow (extend vl3in7) (-0 * x) <> pow (sink vl7in12) (7 * x)
    , note 0.25 $ pow (extend vl3in7) (-3 * x) <> pow (sink vl7in12) (7 * x) <> [1, 0, 0]
    , note 0.25 $ pow (extend vl3in7) (-3 * x) <> pow (sink vl7in12) (7 * x) <> [2, 0, 0]
    , note 0.25 $ pow (extend vl3in7) (-3 * x) <> pow (sink vl7in12) (7 * x) <> [3, 0, 0]
    ]

