module DenoMusic.Types
  ( module DenoMusic.Types
  , Interval (..)
  ) where

import Control.Monad
import Data.IntervalMap.FingerTree (Interval(..))
import GHC.Generics
import Text.PrettyPrint.HughesPJClass hiding ((<>))


-- | Attach a register to some value.
data Reg a = Reg
  { getReg :: Int
  , unReg :: a
  }
  deriving stock (Eq, Ord, Show, Functor, Generic)

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
  deriving stock (Show, Eq, Ord, Read, Enum, Bounded, Generic)

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

