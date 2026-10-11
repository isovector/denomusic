module Music.Types
  ( module Music.Types
  , module Music.Types.Note
  , module Music.Types.Scale
  , module Music.Types.Vertical
  ) where

import Control.Monad.Cont
import Data.Coerce
import Data.Set qualified as S
import DenoMusic.Types (Reg(..))
import FRP
import Music.Harmony
import Music.Types.Note
import Music.Types.Scale
import Music.Types.Vertical


data Dyad = Lo | Hi
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data Triad = Root | Third | Fifth
  deriving stock (Eq, Ord, Show, Enum, Bounded)


divModNote :: forall n. KnownNat n => Note n -> Reg (Note n)
divModNote (Note d as)
  = uncurry Reg
  . fmap (flip Note as)
  $ coerce (flip divMod $ scaleSize @n) d


undivModNote :: forall n. KnownNat n => Reg (Note n) -> Note n
undivModNote (Reg n d) = d + fromIntegral (n * scaleSize @n)

data Timed a = Timed
  { tduration :: Time
  , unTimed :: a
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype H i o a = Horizontal
  { unHorizontal :: Cont (SF i (Event (Timed o))) a
  }
  deriving newtype (Functor, Applicative, Monad)

timedToNotes :: Timed a -> Notes a
timedToNotes (Timed t a) = Notes $ S.singleton (t, a)


toHorizontal :: SF i (Event (Timed o), Event a) -> H i o a
toHorizontal = Horizontal . cont . switch

switchHorizontal :: H i o a -> (a -> SF i (Event (Timed o))) -> SF i (Event (Timed o))
switchHorizontal = runCont . unHorizontal

getHorizontal :: H i o a -> SF i (Event (Timed o))
getHorizontal = flip switchHorizontal $ const $ arr $ const NoEvent

rest :: Time -> H i a ()
rest t = toHorizontal $ proc i -> do
  e <- at t () -< i
  returnA -< (NoEvent, e)

hit :: Time -> a -> H i a ()
hit t a = toHorizontal $ proc i -> do
  n <- now (Timed t a) -< i
  e <- at t () -< i
  returnA -< (n, e)

