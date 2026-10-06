module Music.Types where

import Data.Set qualified as S
import Control.DeepSeq
import Control.Monad.Cont
import Data.Coerce
import Data.Foldable
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Monoid (Ap(..))
import Data.Profunctor
import DenoMusic.Harmony
import DenoMusic.Types (Reg(..))
import FRP
import GHC.Generics


data Dyad = Lo | Hi
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data Triad = Root | Third | Fifth
  deriving stock (Eq, Ord, Show, Enum, Bounded)



-- | A vertical ("simultaneous") configuration of some sort.
newtype Vertical v a = Vertical
  { getVertical :: v -> a
  }
  deriving stock (Functor, Generic)
  deriving newtype (Applicative, Monad, Profunctor)
  deriving (Num, Semigroup, Monoid) via Ap (Vertical v) a

instance NFData (Vertical v a)

instance (Enum v, Bounded v, Eq a) => Eq (Vertical v a) where
  (==) = on (==) toList

instance (Enum v, Bounded v, Ord a) => Ord (Vertical v a) where
  compare = on compare toList

instance (Enum v, Bounded v, Show v, Show a) => Show (Vertical v a) where
  showsPrec p = showsPrec p . enumerate


enumerate :: (Enum v, Bounded v) => Vertical v a -> [(v, a)]
enumerate (Vertical c) = fmap (id &&& c) $ enumFromTo minBound maxBound

unenumerate :: Eq v => [(v, a)] -> Vertical v a
unenumerate = Vertical . fmap (fromMaybe $ error "traverse @Vertical: impossible") . flip lookup


instance (Enum v, Bounded v) => Foldable (Vertical v) where
  foldMap f = foldMap (f . snd) . enumerate

instance (Eq v, Enum v, Bounded v) => Traversable (Vertical v) where
  traverse f
    = fmap unenumerate
    . traverse (traverse f)
    . enumerate


divModNote :: forall n. KnownNat n => Note n -> Reg (Note n)
divModNote (Note d as)
  = uncurry Reg
  . fmap (flip Note as)
  $ coerce (flip divMod $ scaleSize @n) d


undivModNote :: forall n. KnownNat n => Reg (Note n) -> Note n
undivModNote (Reg n d) = d + fromIntegral (n * scaleSize @n)


embed :: (v' -> (v, a -> b)) -> Vertical v a -> Vertical v' b
embed f' (Vertical f) = Vertical $ \v' ->
  let (v, fab) = f' v'
   in fab $ f v

data Timed a = Timed
  { duration :: Time
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

