module Music where

import Data.List (inits)
import qualified Data.Set as S
import Data.Set (Set)
import Data.Foldable
import Data.Functor
import Data.Maybe
import Data.Monoid
import Data.Monoid.Action
import Data.Tree.DUAL hiding (flatten)
import Data.Tree.DUAL qualified as D
import Data.Void
import Euterpea (PitchClass(..))
import Euterpea qualified as E
import MadMusic (T(..), Reg(..), move1)
import MadMusic qualified as MM

data Envelope = Envelope
  { e_duration :: Rational
  , e_offset :: Rational
  , e_scale :: Last (Set PitchClass)
  , e_root :: Last (Reg PitchClass)
  , e_chord :: Last (Set Int)
  , e_harmony :: T
  , e_voice :: Last Int
  }

instance Semigroup Envelope where
  Envelope a1 b1 c1 d1 e1 f1 g1 <> Envelope a2 b2 c2 d2 e2 f2 g2
    = Envelope (a1 * a2) (a1 * b2 + b1) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2)

instance Monoid Envelope where
  mempty = Envelope 1 0 mempty mempty mempty mempty mempty

instance Action Envelope (Sum Rational) where
  act e (Sum s) = Sum $ e_duration e * s


newtype Music = Music
  { unMusic :: DUALTree Envelope (Sum Rational) Void T
  }


instance Semigroup Music where
  sa@(Music a) <> Music b =
    Music $ a <> applyD (mempty { e_offset = duration sa }) b

instance Monoid Music where
  mempty = Music mempty

withScale :: Set PitchClass -> Music -> Music
withScale sc = addEnv $ mempty { e_scale = pure sc }

withRoot :: Reg PitchClass -> Music -> Music
withRoot t = addEnv $ mempty { e_root = pure t }

voice :: Int -> Music -> Music
voice t = addEnv $ mempty { e_voice = pure t }

withChord
  :: Set Int
  -- ^ scale degrees off of the current root
  -> Music
  -> Music
withChord t = addEnv $ mempty { e_chord = pure t }

duration :: Music -> Rational
duration = getSum . fromMaybe mempty . getU . unMusic

rest :: Rational -> Music
rest d = Music $ leafU $ Sum d

addEnv :: Envelope -> Music -> Music
addEnv e = Music . applyD e . unMusic

stretch :: Rational -> Music -> Music
stretch d = addEnv $ mempty { e_duration = d }

note :: Rational -> T -> Music
note d = stretch d . Music . leaf (Sum 1)

stretchTo :: Rational -> Music -> Music
stretchTo d t =
  case duration t of
    0 -> mempty
    dur -> stretch (d / dur) t

offset :: Rational -> Music -> Music
offset d m = rest d <> m <> rest (-d)

reharmonize :: T -> Music -> Music
reharmonize t = addEnv $ mempty { e_harmony = t }

export
  :: Envelope
  -> T
  -> Reg PitchClass
export e t = do
  let sc = fromMaybe (S.fromList [C, D, E, F, G, A, B]) $ getLast $ e_scale e
      root = fromMaybe (Reg 4 C) $ getLast $ e_root e
      ch = S.map (flip (MM.extrMove sc) root) $ fromMaybe (S.fromList [0, 2, 4])
         $ getLast
         $ e_chord e
      (virtual_v, v)
        = quotRem (fromMaybe 0 $ getLast $ e_voice e)
        $ length ch
  move1 sc ch (e_harmony e <> chordTone virtual_v <> t)
      $ (toList ch !!)
      $ v


simul :: Foldable t => t Music -> Music
simul as =
  foldMap (co . re) as <>
    rest (maximum $ 0 : (fmap duration $ toList as))


toEuterpea
  :: Music
  -> E.Music (PitchClass, Int)
toEuterpea s = do
  foldr (E.:=:) (E.rest 0) $
    (D.flatten $ unMusic s) <&> \(n, e) ->
      E.rest (e_offset e) E.:+: E.note (e_duration e)
        (MM.fromReg $ export e n)

inv :: Music -> Music
inv t = let d = duration t in rest (- d) <> t <> rest (- d)

re :: Music -> Music
re t = t <> rest (- duration t)

co :: Music -> Music
co t = rest (- duration t) <> t


fork :: Music -> Music -> Music
fork a b = re a <> b

join :: Music -> Music -> Music
join a b = a <> co b


chord :: Foldable t => Rational -> t T -> Music
chord d ts = simul $ fmap (note d) $ toList ts

chordTone :: Int -> T
chordTone = T 0

inversion :: Int -> T
inversion = chordTone

scaleTone :: Int -> T
scaleTone = flip T 0


modulate :: [T] -> [Music] -> Music
modulate ts = mconcat . zipWith reharmonize (fmap fold $ inits ts)


play :: Music -> IO ()
play
  = E.playDev 2
  . toEuterpea
