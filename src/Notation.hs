{-# LANGUAGE DeriveAnyClass                       #-}
{-# LANGUAGE DerivingStrategies                   #-}
{-# LANGUAGE LambdaCase                           #-}
{-# LANGUAGE TypeFamilies                         #-}
{-# OPTIONS_GHC -fno-warn-deprecations            #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fno-warn-orphans                 #-}

module Notation where

import Data.Bool
import Data.Ratio
import Control.Lens ((&), (.~), (%~), (+~), (<>~), _2)
import Control.Lens qualified as L
import GHC.Generics
import Data.Ord
import Data.MemoTrie
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty(..))
import Theory.Chords
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Set.Internal (Set(..))
import Data.Set (Set)
import Debug.Trace
import System.Cmd (rawSystem)
import Text.Pretty  (pretty)
import Data.Containers.ListUtils (nubOrd)
import Control.Exception
import Data.Bifunctor
import Data.Sequence qualified as Seq
import Data.Sequence (Seq(..))
import Data.IntervalMap.FingerTree (IntervalMap, Interval(..))
import Data.IntervalMap.FingerTree qualified as IM
import Data.Maybe
import Data.Foldable
import Data.Music.Lilypond hiding (chord)
import Euterpea qualified as E
-- import Text.Pretty (pretty)
import Score hiding (im)
import Data.Tree.DUAL
import GHC.Real


toPitch :: E.Pitch -> Pitch
toPitch (pc, o) =
  let (pn, a) = pitchClassToPitchAndAccidental pc
   in  Pitch (pn, a, o + 1)

pitchClassToPitchAndAccidental :: E.PitchClass -> (PitchName, Int)
pitchClassToPitchAndAccidental E.C   = (C, 0)
pitchClassToPitchAndAccidental E.Cff = (C, -2)
pitchClassToPitchAndAccidental E.Cf  = (C, -1)
pitchClassToPitchAndAccidental E.Cs  = (C, 1)
pitchClassToPitchAndAccidental E.Css = (C, 2)
pitchClassToPitchAndAccidental E.D   = (D, 0)
pitchClassToPitchAndAccidental E.Dff = (D, -2)
pitchClassToPitchAndAccidental E.Df  = (D, -1)
pitchClassToPitchAndAccidental E.Ds  = (D, 1)
pitchClassToPitchAndAccidental E.Dss = (D, 2)
pitchClassToPitchAndAccidental E.E   = (E, 0)
pitchClassToPitchAndAccidental E.Eff = (E, -2)
pitchClassToPitchAndAccidental E.Ef  = (E, -1)
pitchClassToPitchAndAccidental E.Es  = (E, 1)
pitchClassToPitchAndAccidental E.Ess = (E, 2)
pitchClassToPitchAndAccidental E.F   = (F, 0)
pitchClassToPitchAndAccidental E.Fff = (F, -2)
pitchClassToPitchAndAccidental E.Ff  = (F, -1)
pitchClassToPitchAndAccidental E.Fs  = (F, 1)
pitchClassToPitchAndAccidental E.Fss = (F, 2)
pitchClassToPitchAndAccidental E.G   = (G, 0)
pitchClassToPitchAndAccidental E.Gff = (G, -2)
pitchClassToPitchAndAccidental E.Gf  = (G, -1)
pitchClassToPitchAndAccidental E.Gs  = (G, 1)
pitchClassToPitchAndAccidental E.Gss = (G, 2)
pitchClassToPitchAndAccidental E.A   = (A, 0)
pitchClassToPitchAndAccidental E.Aff = (A, -2)
pitchClassToPitchAndAccidental E.Af  = (A, -1)
pitchClassToPitchAndAccidental E.As  = (A, 1)
pitchClassToPitchAndAccidental E.Ass = (A, 2)
pitchClassToPitchAndAccidental E.B   = (B, 0)
pitchClassToPitchAndAccidental E.Bff = (B, -2)
pitchClassToPitchAndAccidental E.Bf  = (B, -1)
pitchClassToPitchAndAccidental E.Bs  = (B, 1)
pitchClassToPitchAndAccidental E.Bss = (B, 2)

imToSeq :: Ord v => IntervalMap v a -> Seq (Interval v, a)
imToSeq im =
  case IM.bounds im of
    Just b -> Seq.fromList $ IM.intersections b im
    Nothing -> Seq.empty

seqToIm :: Ord v => Seq (Interval v, a) -> IntervalMap v a
seqToIm = foldMap (uncurry IM.singleton)

overIM
  :: Ord v
  => (Seq (Interval v, a) -> Seq (Interval v, b))
  -> IntervalMap v a -> IntervalMap v b
overIM f = seqToIm . f . imToSeq

everywhere
  :: PostEvent
  -> Seq (Interval v, ([PostEvent], a))
  -> Seq (Interval v, ([PostEvent], a))
everywhere pe = fmap $ fmap $ first (pe :)

around
  :: PostEvent
  -> PostEvent
  -> Seq (Interval v, ([PostEvent], a))
  -> Seq (Interval v, ([PostEvent], a))
around _ _ Empty = Empty
around s e ((i, (pes, a)) :<| Empty) = (i, (s : e : pes, a)) :<| Empty
around s e ((i1, (pes1, a1)) :<| (xs :|> (i2, (pes2, a2)))) =
  (i1, (s : pes1, a1)) :<| (xs :|> (i2, (e : pes2, a2)))

scoreToIM :: Score a -> IntervalMap Rational ([PostEvent], a)
scoreToIM = fromMaybe mempty .
  foldDUAL
    (\(Envelope s o) a ->
      let lo = min o (o + s)
          hi = max o (o + s)
       in IM.singleton (Interval lo hi) (mempty, a))
    mempty
    fold
    (const id)
    (\ann ->
      case ann of
        Phrase -> overIM $ around BeginPhraseSlur EndPhraseSlur
    )
    . unScore

deriving stock instance Ord Markup
deriving stock instance Ord Articulation
deriving stock instance Ord PostEvent

imsToLilypond :: [IntervalMap Rational ([PostEvent], E.Pitch)] -> Music
imsToLilypond = foldr1 simultaneous .  fmap imToLilypond

imToLilypond :: IntervalMap Rational ([PostEvent], E.Pitch) -> Music
imToLilypond im =
  case IM.leastView im of
    Nothing -> rest
    Just ((i, xs), im') -> do
      let start = IM.low i
          concurrent = IM.search start im'
          notes = xs : fmap snd concurrent
      let remaining = imToLilypond im'
      sequential
          -- (sequential (Rest (Just $ Duration start) []) $
          (Chord
            (fmap ((, []) . (\p -> NotePitch p Nothing) . toPitch . snd) notes)
            (Just $ Duration $ IM.high i - IM.low i)
            $ nubOrd $ foldMap fst notes) remaining


toLilypond :: Score E.Pitch -> String
toLilypond = show . pretty . imsToLilypond . dp (E.absPitch . snd) . traversalOrder . scoreToIM

main :: IO ()
main = do
  let lp = read @String $ show $ pretty $ toLilypond $ chord [(E.C, 3), (E.C, 5)] <> tile 1 (E.D, 5)
  writeFile "/tmp/out.lily" $ "\\absolute\n" <> lp
  _ <- rawSystem "lilypond" ["-o", "/tmp/song", "/tmp/out.lily"]
  -- rawSystem "evince" ["/tmp/song.pdf"]
  pure ()

data Terminal a = Start a | Stop a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

traversalOrder :: Ord v => IntervalMap v a -> [(Interval v, a)]
traversalOrder = sortOn (\((Interval lo hi), _) -> (lo, hi)) . toList . imToSeq

data Timed a
  = Timed a :+: Timed a
  | Rest Rational
  | Notes (NonEmpty a) Rational
  | End
  deriving stock Generic

deriving stock instance Generic (Ratio a)

instance HasTrie a => HasTrie (Ratio a) where
  newtype Ratio a :->: x = RatioTrie { unRatioTrie :: Reg (Ratio a) :->: x }
  trie = trieGeneric RatioTrie
  untrie = untrieGeneric unRatioTrie
  enumerate = enumerateGeneric unRatioTrie

instance HasTrie a => HasTrie (Interval a) where
  newtype Interval a :->: x = IntervalTrie { unIntervalTrie :: Reg (Interval a) :->: x }
  trie = trieGeneric IntervalTrie
  untrie = untrieGeneric unIntervalTrie
  enumerate = enumerateGeneric unIntervalTrie

deriving stock instance Generic (Set a)

instance HasTrie a => HasTrie (Set a) where
  newtype Set a :->: x = SetTrie { unSetTrie :: Reg (Set a) :->: x }
  trie = trieGeneric SetTrie
  untrie = untrieGeneric unSetTrie
  enumerate = enumerateGeneric unSetTrie

type Cost = Int

data Valid = Valid | Invalid
  deriving (Eq, Ord, Show)



-- | Let's do some dynamic programming to figure out how the hell to break an
-- interval map into musical voices.
dp :: forall a. (a -> E.AbsPitch) -> [(Interval Rational, a)] -> [IntervalMap Rational a]
dp f intervals = fst $ go 0 [] []
  where
    im = M.fromList $ zip [0..] intervals

    go
      :: Int
      -- \^ The index into @im@ we're currently looking at
      -> [Interval Rational]
      -- \^ The last interval inserted into the given voice. Used to make sure
      -- we don't overlap, and that we can merge into chords if the durations
      -- are identical.
      -> [Set E.AbsPitch]
      -- \^ The last pitch(es) we inserted into the given voice. Used to
      -- compute ongoing costs.
      -> ([IntervalMap Rational a], Cost)
    go = memo3 $
      \ix lastintervals lastchords ->
        case (M.lookup ix im) of
          Nothing ->
            -- In the base case, just make sure we have sufficient empty voices.
            (replicate (length lastintervals) mempty, 0)
          Just (i, a) -> do
            let a_set = S.singleton $ f a

            -- Otherwise compute the minimum of:
            minimumBy (comparing snd) $ mconcat
              [ -- We can always add a new voice if the problem is otherwise
                -- intractable.
                pure $
                  let (res, cost) =
                        go (ix + 1)
                           (lastintervals ++ [i])
                           (lastchords ++ [a_set])
                  in ( res & L.ix (length lastintervals) %~ IM.insert i a
                      , cost + 100
                      )
              , do
                  -- Or, for each existing voice...
                  (vix, vint) <- zip [0..] lastintervals
                  case (vint == i, IM.high vint <= IM.low i) of
                    -- Check if this interval is the same as the last one we
                    -- inserted. If so, we can add this note as part of
                    -- a chord.
                    (True, _) -> pure $
                      let (res, cost) =
                            go (ix + 1)
                               lastintervals
                               (lastchords & L.ix vix <>~ a_set)
                          chordCost = bool 0 10000 $ any ((> 12) . abs . subtract (f a)) $ lastchords !! vix
                      in ( res & L.ix vix %~ IM.insert i a
                          , cost + chordCost
                          )
                    -- Otherwise, ensure that this interval occurs AFTER the
                    -- last one. Otherwise it overlaps and we can't use this
                    -- voice.
                    (_, True) -> pure $
                      let noteCost = minimum $ fmap (abs . subtract (f a)) $ S.toList $ lastchords !! vix
                          (res, cost) =
                            go (ix + 1)
                               (lastintervals & L.ix vix .~ i)
                               (lastchords & L.ix vix .~ S.singleton (f a))
                      in ( res & L.ix vix %~ IM.insert i a
                          , cost + noteCost
                          )
                    _ -> mempty
              ]




