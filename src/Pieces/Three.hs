{-# LANGUAGE DataKinds #-}

module Pieces.Three where

import Data.Group
import Data.Semigroup
import DenoMusic.Harmony (play)
import DenoMusic.Harmony2
import Data.Set qualified as S
import Music2 hiding (T)

data V = VS | VA | VT | VB
  deriving stock (Eq, Ord, Show, Enum, Bounded)

permute :: V -> V
permute VB = VA
permute VA = VS
permute VS = VT
permute VT = VB


music :: Music V (T '[3])
music = fromVoices . fmap line $ \case
  VS ->
    [ note 0.25 $ 0 :> Nil
    , rest 0.50
    , note 0.25 $ 0 :> Nil
    ]
  VA ->
    [ rest 0.25
    , note 0.25 $ 0 :> Nil
    , note 0.25 $ 1 :> Nil
    , note 0.25 $ 0 :> Nil
    ]
  VT ->
    [ note (1/3) $ 2 :> Nil
    , note (1/3) $ 1 :> Nil
    , note (1/3) $ 0 :> Nil
    ]
  VB -> [ note 1 $ 0 :> Nil ]


main :: IO ()
main = do
  toPdf score
  play score

score
  = fmap (S.singleton)
  $ harmonize
  $ line
  $ take 12
  $ repeat
  -- $ iterate (lmap permute)
  $ music


harmonize :: Music V (T '[3]) -> Music V (Reg PitchClass)
harmonize =
  quad
    standard
    (Reg 4 C)
    ( line
        [ note 2 vl7in12
        , note 2 $ mempty
        , note 2 $ vl7in12
        , note 2 $ mempty
        ]
    )
    ( line $ fmap (\(n, d) -> note d $ stimes n (invert vl3in7)) $ zip [1..]
        [ 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        ]
    )
    . mappend (fromVoices $ pure . \case
        VS -> 4 :> Nil
        VA -> 2 :> Nil
        VT -> 0 :> Nil
        VB -> (-3) :> Nil
      )


quad
  :: Ord a
  => MetaScales '[3, 7, 12] a
  -- ^ "Scales" defining the chord shape, diatonic collection, and chromatic
  -- collection.
  -> Reg a
  -- ^ The root note
  -> Music () (T '[7, 12])
  -- ^ Key changes
  -> Music () (T '[3, 7])
  -- ^ Harmonic changes
  -> Music v (T '[3])
  -- ^ Voices moving within the chord
  -> Music v (Reg a)
  -- ^ Voices mapped to the chromatic collection
quad ms root scaleProg chordProg voices =
  (\sc ch v ->
    elim ms (extend v <> extend ch <> sink sc) root
  )
    <$> everyone scaleProg
    <*> everyone chordProg
    <*> voices

