module Music.Types where

import Data.Coerce
import DenoMusic.Types (Reg(..))
import Control.Arrow
import Data.Maybe (fromMaybe)
import Data.Profunctor

import DenoMusic.Harmony


data Dyad = Lo | Hi
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data Triad = Root | Third | Fifth
  deriving stock (Eq, Ord, Show, Enum, Bounded)



-- | A vertical ("simultaneous") configuration of some sort.
newtype Vertical v a = Vertical
  { getVertical :: v -> a
  }
  deriving stock (Functor)
  deriving newtype (Semigroup, Applicative, Monad, Monoid, Profunctor)

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


-- | Swap the degrees and modifications of two voices in a vertical
-- configuration. The voices will maintain their register.
swapV
    :: (Eq v, KnownNat n)
    => v
    -> v
    -> Vertical v (Note n)
    -> Vertical v (Note n)
swapV v1 v2 (Vertical f) = Vertical $ do
  let Reg r1 n1 = divModNote $ f v1
      Reg r2 n2 = divModNote $ f v2
      a1' = undivModNote $ Reg r1 n2
      a2' = undivModNote $ Reg r2 n1
  \case
    v | v == v1   -> a1'
      | v == v2   -> a2'
      | otherwise -> f v


embed :: (v' -> (v, a -> b)) -> Vertical v a -> Vertical v' b
embed f' (Vertical f) = Vertical $ \v' ->
  let (v, fab) = f' v'
   in fab $ f v

