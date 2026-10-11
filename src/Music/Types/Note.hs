{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ViewPatterns        #-}

module Music.Types.Note
  ( module Music.Types.Note
  , KnownNat
  ) where

import Control.Category
import Data.Align
import Data.Bool
import Data.Kind
import Data.List (dropWhileEnd)
import Data.Proxy
import Data.These
import GHC.Generics (Generic)
import GHC.TypeLits
import Prelude hiding (id, (.))


type Note :: Nat -> Type
data Note n = MkNote (Deg n) [Int]
  deriving stock Generic

noteDeg :: Note n -> Deg n
noteDeg (Note d _) = d


type Deg :: Nat -> Type
newtype Deg n = Deg
  { getDeg :: Int
  }
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, Num, Enum, Real, Integral)


scaleSize :: forall n. KnownNat n => Int
scaleSize = fromInteger $ natVal $ Proxy @n


pattern Note :: Deg n -> [Int] -> Note n
pattern Note a as <- MkNote a (dropWhileEnd (== 0) -> as)
  where
    Note a = MkNote a . dropWhileEnd (== 0)
{-# COMPLETE Note #-}

infixl 6 /:
(/:) :: Deg n -> [Int] -> Note n
(/:) = MkNote

infixl 6 /+
(/+) :: Note n -> Int -> Note n
a /+ b = a + Note 0 [b]

infixl 6 //+
(//+) :: Note n -> Int -> Note n
a //+ b = a + Note 0 [0, b]

infixl 6 /-
(/-) :: Note n -> Int -> Note n
a /- b = a - Note 0 [b]

infixl 6 //-
(//-) :: Note n -> Int -> Note n
a //- b = a - Note 0 [0, b]


normalizeNote :: forall n. KnownNat n => Note n -> Note n
normalizeNote (Note (Deg a) as) = Note (Deg $ mod a $ scaleSize @n) as


instance Eq (Note n) where
  Note a as == Note b bs = a == b && as == bs

instance Ord (Note n) where
  compare (Note (Deg a) as) (Note (Deg b) bs)
    = foldMap
        (these (flip compare 0) (compare 0) compare)
    $ align (a : as) (b : bs)

instance Num (Note n) where
  fromInteger n = Note (fromInteger n) []
  Note a as + Note b bs = Note (a + b) $ alignWith (these id        id        (+)) as bs
  Note a as - Note b bs = Note (a - b) $ alignWith (these id        negate    (-)) as bs
  Note a as * Note b bs = Note (a * b) $ alignWith (these (const 0) (const 0) (*)) as bs
  abs    (Note a as) = Note (abs a)    $ fmap abs    as
  signum (Note a as) = Note (signum a) $ fmap signum as

instance Show (Note n) where
  showsPrec p (Note n []) = showsPrec p n
  showsPrec p (Note n [a]) = showParen (p >= 11) $ showsPrec 11 n . showString (bool ("/-") ("/+") (a >= 0)) . showsPrec 11 (abs a)
  showsPrec p (Note n [0, a]) = showParen (p >= 11) $ showsPrec 11 n . showString (bool ("//-") ("//+") (a >= 0)) . showsPrec 11 (abs a)
  showsPrec p (Note n ns) = showParen (p >= 11) $ showsPrec 11 n . showString "/:" . showsPrec 11 ns

