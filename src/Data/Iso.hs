module Data.Iso
  ( GenericIso (..)
  , Category (..)
  , Group(..)
  , Iso
  , Iso'
  , iso
  , Endomorphism (..)
  , AnIso'
  , cloneIso
  , SelfDual(..)
  , selfDual
  ) where

import Control.Category
import Control.Lens (Iso, Iso', AnIso', iso, cloneIso, from)
import Prelude hiding (id, (.))
import Data.Group


newtype GenericIso f a b = GenericIso
  { getIso :: AnIso' (f a) (f b)
  }

instance Category (GenericIso f) where
  id = GenericIso id
  GenericIso g . GenericIso f = GenericIso $ cloneIso f . cloneIso g

instance SelfDual (GenericIso f) where
  op (GenericIso f) = GenericIso $ from f

class Category k => SelfDual k where
  op :: k a b -> k b a



newtype Endomorphism k a = Endomorphism {getEndomorphism :: k a a}

instance Category k => Semigroup (Endomorphism k a) where
  Endomorphism f <> Endomorphism g = Endomorphism (g . f)

instance Category k => Monoid (Endomorphism k a) where
  mempty = Endomorphism id

instance SelfDual k => Group (Endomorphism k a) where
  invert (Endomorphism k) = Endomorphism $ op k


selfDual :: (a -> a) -> Iso' a a
selfDual f = iso f f

