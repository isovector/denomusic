module Music where

import Data.Map qualified as M
import Data.Map (Map)
import Data.Map.Monoidal qualified as MM
import Data.Set qualified as S
import Data.Set (Set)
import Data.Group
import DenoMusic.Types (Reg (..), Music (..), Voice (..))
import Data.Kind

data Hierarchy = Subchord | Chord | Scale | Chroma

infixr 4 :>

type T :: [Hierarchy] -> Type
data T h where
  Nil :: T '[]
  (:>) :: Int -> !(T ns) -> T (n ': ns)

deriving stock instance Eq (T ns)
deriving stock instance Ord (T ns)
deriving stock instance Show (T ns)


instance Semigroup (T '[]) where
  _ <> _ = Nil

instance Semigroup (T ns) => Semigroup (T (n ': ns)) where
  (i :> is) <> (j :> js) = i + j :> is <> js

instance Monoid (T '[]) where
  mempty = Nil

instance Monoid (T ns) => Monoid (T (n ': ns)) where
  mempty = 0 :> mempty

instance Group (T '[]) where
  invert _ = Nil

instance Group (T ns) => Group (T (n ': ns)) where
  invert (i :> is) = negate i :> invert is

elim
  :: Set a
  -> Int
  -- ^ chord vector
  -> Reg a
elim s n =
  let (q, r) = divMod n $ length s
   in Reg q $ S.toList s !! r

