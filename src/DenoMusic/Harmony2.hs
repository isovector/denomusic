{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeAbstractions       #-}
{-# LANGUAGE UndecidableInstances   #-}

module DenoMusic.Harmony2 where

import Music2 hiding (T)
import Data.Group
import Data.Set qualified as S
import Music.Harmony (Reg(..), extrMove)
import Data.Kind
import GHC.TypeLits

type T :: [Nat] -> Type
data T ns where
  Nil :: T '[]
  (:>) :: Int -> !(T ns) -> T (n ': ns)

infixr 9 :>

instance Semigroup (T '[]) where
  _ <> _ = Nil

instance Semigroup (T ns) => Semigroup (T (n ': ns)) where
  (i :> is) <> (j :> js) = (i + j) :> (is <> js)

instance Monoid (T '[]) where
  mempty = Nil

instance Monoid (T ns) => Monoid (T (n ': ns)) where
  mempty = 0 :> mempty

instance Group (T '[]) where
  invert _ = Nil

instance Group (T ns) => Group (T (n ': ns)) where
  invert (i :> is) = negate i :> invert is

deriving stock instance Show (T ns)

type MetaScales :: [Nat] -> Type -> Type
data MetaScales ns a where
  Base :: Set a -> MetaScales '[n] a
  MSCons :: MetaScale n -> !(MetaScales ns a) -> MetaScales (n ': ns) a

deriving stock instance Show a => Show (MetaScales ns a)

type MetaScale :: Nat -> Type
newtype MetaScale n = UnsafeMetaScale
  { getMetaScale :: Set Int
  }
  deriving newtype Show

ionian :: MetaScale 7
ionian = UnsafeMetaScale $ S.fromList [0, 2, 4, 5, 7, 9, 11]

triad :: MetaScale 3
triad = UnsafeMetaScale $ S.fromList [0, 2, 4]


elim :: Ord a => MetaScales ns a -> T ns -> Reg a -> Reg a
elim (Base sc) (i :> Nil) r = extrMove sc i r
elim (MSCons ms scs) (i :> j :> js) r = do
  dj <- extrMove (getMetaScale ms) i (Reg 0 0)
  elim scs ((dj + j) :> js) r

standard :: MetaScales '[3, 7, 12] PitchClass
standard = MSCons triad $ MSCons ionian spelledFlat

testix :: T '[3, 7, 12]
testix = 2 :> 2 :> 0 :> Nil

spelledFlat :: MetaScales '[12] PitchClass
spelledFlat = Base (S.fromList [A, Af, B, Bf, C, D, Df, E, Ef, F, G, Gf])

vl3in7 :: T '[3, 7]
vl3in7 = 5 :> (-2) :> mempty

vl7in12 :: T '[7, 12]
vl7in12 = 7 :> (-4) :> mempty


type family (++) xs ys where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)


extend :: forall ns ms. Monoid (T ns) => T ms -> T (ms ++ ns)
extend (x :> xs) = x :> extend @ns xs
extend Nil = mempty

test :: T '[3, 7, 12]
test = extend vl3in7

sink :: T ns -> T (n ': ns)
sink t = 0 :> t

quad
  :: Ord a
  => MetaScales '[3, 7, 12] a
  -> Reg a
  -> Music () (T '[7, 12])
  -> Music () (T '[3, 7])
  -> Music v (T '[3])
  -> Music v (Reg a)
quad ms root scaleProg chordProg voices =
  (\sc ch v ->
    elim ms (extend v <> extend ch <> sink sc) root
  )
    <$> everyone scaleProg
    <*> everyone chordProg
    <*> voices

