module Music.Types
  ( module Music.Types
  , module Music.Types.Note
  , module Music.Types.Scale
  , module Music.Types.Vertical
  ) where

import Control.Applicative
import Control.Arrow
import Control.Arrow.Transformer.Static
import Control.Category
import Control.Lens hiding (at)
import Control.Monad.Cont
import Data.Coerce
import Data.Generics.Labels ()
import Data.Generics.Product (field')
import Data.Set qualified as S
import DenoMusic.Types (Reg(..))
import FRP
import GHC.Generics
import Music.Harmony
import Music.Types.Note
import Music.Types.Scale
import Music.Types.Vertical
import Prelude hiding (id, (.))


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

-- | The applicative instance does parallel composition, while the monoid is
-- sequential.
data Timed a = Timed
  { tduration :: Time
  , unTimed :: a
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
  deriving (Semigroup, Monoid) via Generically (Timed a)

instance Applicative Timed where
  pure = Timed 0
  liftA2 f (Timed ta a) (Timed tb b) = Timed (max ta tb) $ f a b

class HasDuration a where
  duration :: Traversal' a Time

instance HasDuration Time where
  duration = id

instance HasDuration (Timed t) where
  duration = #tduration

instance HasDuration (Horizontal i o) where
  duration = field' @"unHorizontal" . duration

instance HasDuration a => HasDuration (Event a) where
  duration = #_MkEvent . _Just . duration

denote :: HasDuration a => a -> Horizontal t (Event a)
denote = Horizontal . uncurry Timed . (view duration &&& at 0)

newtype Horizontal i o = Horizontal
  { unHorizontal :: Timed (SF i o)
  }
  deriving stock (Generic, Functor)
  deriving (Applicative) via WrappedArrow (StaticArrow Timed SF) i
  deriving (Category, Arrow, ArrowLoop) via StaticArrow Timed SF


instance Semigroup (Horizontal i o) where
  Horizontal (Timed t1 x) <> Horizontal (Timed t2 y) =
    Horizontal $ Timed (t1 + t2) $ switch (x &&& at t1 ()) $ const y

instance Monoid o => Monoid (Horizontal i o) where
  mempty = Horizontal $ pure mempty


resize :: HasDuration o => Rational -> Horizontal i o -> Horizontal i o
resize d (Horizontal (Timed t sf)) =
  Horizontal $ Timed (fromRational d * t) $ fmap (duration *~ fromRational d) $ sf >>> stretch d


newtype H i o a = H
  { unH :: Cont (SF i (Event (Timed o))) a
  }
  deriving newtype (Functor, Applicative, Monad)

timedToNotes :: Timed a -> Notes a
timedToNotes (Timed t a) = Notes $ S.singleton (t, a)


toHorizontal :: SF i (Event (Timed o), Event a) -> H i o a
toHorizontal = H . cont . switch

switchHorizontal :: H i o a -> (a -> SF i (Event (Timed o))) -> SF i (Event (Timed o))
switchHorizontal = runCont . unH

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

