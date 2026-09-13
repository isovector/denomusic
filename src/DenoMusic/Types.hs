module DenoMusic.Types
  ( module DenoMusic.Types
  , Interval (..)
  ) where

import Control.Applicative
import Control.Monad
import Data.Function.Step.Discrete.Open
import Data.Functor.Compose
import Data.IntervalMap.FingerTree (Interval(..))
import Data.Map qualified as M
import Data.Maybe hiding (catMaybes)
import GHC.Generics
import Witherable hiding (filter)
import Data.Map.Monoidal (MonoidalMap)
import Data.Map.Monoidal qualified as MM
import Text.PrettyPrint.HughesPJClass hiding ((<>))


-- | Attach a register to some value.
data Reg a = Reg
  { getReg :: Int
  , unReg :: a
  }
  deriving stock (Eq, Ord, Show, Functor)

instance Applicative Reg where
  pure = Reg 0
  (<*>) = ap

instance Monad Reg where
  Reg r a >>= f = withReg (+ r) $ f a


fromReg :: Reg a -> (a, Int)
fromReg (Reg i a) = (a, i)


withReg :: (Int -> Int) -> Reg a -> Reg a
withReg f (Reg r a) = Reg (f r) a


data PitchClass
  =      C | Cs
  | Df | D | Ds
  | Ef | E
  |      F | Fs
  | Gf | G | Gs
  | Af | A | As
  | Bf | B
  deriving stock (Show, Eq, Ord, Read, Enum, Bounded)

instance Pretty PitchClass where
  pPrint = text . \case
    C  -> "C"
    Cs -> "C♯"
    Df -> "D♭"
    D  -> "D"
    Ds -> "D♯"
    Ef -> "E♭"
    E  -> "E"
    F  -> "F"
    Fs -> "F♯"
    Gf -> "G♭"
    G  -> "G"
    Gs -> "G♯"
    Af -> "A♭"
    A  -> "A"
    As -> "A♯"
    Bf -> "B♭"
    B  -> "B"


-- -- | Fold a 'Voice' down into its underlying intervals of sound.
-- flatten :: Voice a -> [(Interval Rational, a)]
-- flatten (Voice (SF m _)) = do
--   let m' = filter ((>= 0) . fst) $ M.toList m
--   ((lo, _), (hi, ma)) <- zip ((0, error "bad") : m') m'
--   guard $ lo /= hi
--   a <- maybeToList ma
--   pure (Interval lo hi, a)

