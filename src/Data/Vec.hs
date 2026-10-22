{-# LANGUAGE CPP                  #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE NoStarIsType         #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Data.Vec
  ( -- * 'Vec'tor data type
    Vec(Nil,(:>),(:<),Cons)
  , module Data.Vec
  ) where

import Data.Fin
import Data.SNat
#if __GLASGOW_HASKELL__ >= 914
import qualified Control.Lens     as Lens hiding (data (:>), data (:<))
#else
import qualified Control.Lens     as Lens hiding (pattern (:>), pattern (:<))
#endif
import Data.Distributive
import Data.Functor.Rep
import qualified Data.Foldable    as F
import Data.Kind                  (Type)
import Data.Proxy                 (Proxy (..))
import GHC.TypeLits               (KnownNat, Nat, type (+), type (-), type (*),
                                   type (<=), natVal)
import Prelude                    hiding ((++), (!!), concat, concatMap, drop,
                                          foldl, foldl1, foldr, foldr1, head,
                                          init, iterate, last, length, map,
                                          repeat, replicate, reverse, scanl,
                                          scanl1, scanr, scanr1, splitAt, tail,
                                          take, unzip, unzip3, zip, zip3, zipWith,
                                          zipWith3, maximum, minimum)
import qualified Prelude          as P
import Test.QuickCheck
  (Arbitrary(arbitrary, shrink), CoArbitrary(coarbitrary))

#define CONS_PREC 5

infixr CONS_PREC `Cons`

data Vec :: Nat -> Type -> Type where
  Nil  :: Vec 0 a
  Cons :: a -> Vec n a -> Vec (n + 1) a

{-# COMPLETE Nil, (:>) #-}

pattern (:>) :: a -> Vec n a -> Vec (n + 1) a
pattern (:>) x xs <- ((\ys -> (head ys,tail ys)) -> (x,xs))
  where
    (:>) x xs = Cons x xs

infixr CONS_PREC :>

instance Show a => Show (Vec n a) where
  showsPrec n = \case
    Nil -> showString "Nil"
    vs -> showParen (n > CONS_PREC) (go vs)

   where
    go :: Vec m a -> ShowS
    go Nil = showString "Nil"
    go (x `Cons` xs) =
        showsPrec (CONS_PREC + 1) x
      . showString " :> "
      . go xs

instance (KnownNat n, Eq a) => Eq (Vec n a) where
  (==) Nil _            = True
  (==) v1@(Cons _ _) v2 = fold (&&) (zipWith (==) v1 v2)

instance (KnownNat n, Ord a) => Ord (Vec n a) where
  compare x y = foldr f EQ $ zipWith compare x y
    where f EQ   keepGoing = keepGoing
          f done _         = done

instance (KnownNat n, Semigroup a) => Semigroup (Vec n a) where
  (<>) = zipWith (<>)

instance (KnownNat n, Monoid a) => Monoid (Vec n a) where
  mempty = repeat mempty
  mappend = (<>)

instance KnownNat n => Applicative (Vec n) where
  pure      = repeat
  fs <*> xs = zipWith ($) fs xs

instance KnownNat n => F.Foldable (Vec n) where
  foldMap _ Nil      = mempty
  foldMap f z@Cons{} = fold mappend (map f z)

instance Functor (Vec n) where
  fmap = map

instance KnownNat n => Traversable (Vec n) where
  traverse = traverse#

traverse# :: forall a f b n . Applicative f => (a -> f b) -> Vec n a -> f (Vec n b)
traverse# _ Nil           = pure Nil
traverse# f (x `Cons` xs) = Cons <$> f x <*> traverse# f xs

-- | Create a vector of one element
--
-- >>> singleton 5
-- 5 :> Nil
singleton :: a -> Vec 1 a
singleton = (`Cons` Nil)

head :: Vec (n + 1) a -> a
head (x `Cons` _) = x
#if __GLASGOW_HASKELL__ < 912
head xs = unreachable xs
 where
  unreachable :: forall n a. 1 <= n => Vec n a -> a
  unreachable (x `Cons` _) = x
#endif

tail :: Vec (n + 1) a -> Vec n a
tail (_ `Cons` xr) = xr
#if __GLASGOW_HASKELL__ < 912
tail xs = unreachable xs
 where
  unreachable :: forall n a. 1 <= n => Vec n a -> Vec (n - 1) a
  unreachable (_ `Cons` xr) = xr
#endif

last :: Vec (n + 1) a -> a
last (x `Cons` Nil)         = x
last (_ `Cons` y `Cons` xr) = last (y `Cons` xr)
#if __GLASGOW_HASKELL__ < 912
last xs = unreachable xs
 where
  unreachable :: 1 <= n => Vec n a -> a
  unreachable ys@(Cons _ _) = last ys
#endif

init :: Vec (n + 1) a -> Vec n a
init (_ `Cons` Nil)         = Nil
init (x `Cons` y `Cons` xr) = x `Cons` init (y `Cons` xr)
#if __GLASGOW_HASKELL__ < 912
init xs = unreachable xs
 where
  unreachable :: 1 <= n => Vec n a -> Vec (n - 1) a
  unreachable ys@(Cons _ _) = init ys
#endif

infixl 5 :<
pattern (:<) :: Vec n a -> a -> Vec (n+1) a
pattern (:<) xs x <- ((\ys -> (init ys,last ys)) -> (xs,x))
  where
    (:<) xs x = xs ++ singleton x

infixr 5 ++
(++) :: Vec n a -> Vec m a -> Vec (n + m) a
Nil           ++ ys = ys
(x `Cons` xs) ++ ys = x `Cons` xs ++ ys

splitAt :: SNat m -> Vec (m + n) a -> (Vec m a, Vec n a)
splitAt n xs = splitAtU (toUNat n) xs

splitAtU :: UNat m -> Vec (m + n) a -> (Vec m a, Vec n a)
splitAtU UZero     ys = (Nil, ys)
splitAtU (USucc s) ys = let (as, bs) = splitAtU s $ tail ys
                        in  (head ys `Cons` as, bs)

splitAtI :: KnownNat m => Vec (m + n) a -> (Vec m a, Vec n a)
splitAtI = withSNat splitAt

concat :: Vec n (Vec m a) -> Vec (n * m) a
concat Nil           = Nil
concat (x `Cons` xs) = x ++ concat xs

concatMap :: (a -> Vec m b) -> Vec n a -> Vec (n * m) b
concatMap f xs = concat (map f xs)

unconcat :: KnownNat n => SNat m -> Vec (n * m) a -> Vec n (Vec m a)
unconcat n xs = unconcatU (withSNat toUNat) (toUNat n) xs

unconcatU :: UNat n -> UNat m -> Vec (n * m) a -> Vec n (Vec m a)
unconcatU UZero      _ _  = Nil
unconcatU (USucc n') m ys = let (as,bs) = splitAtU m ys
                            in  as `Cons` unconcatU n' m bs

unconcatI :: (KnownNat n, KnownNat m) => Vec (n * m) a -> Vec n (Vec m a)
unconcatI = withSNat unconcat

merge :: Vec n a -> Vec n a -> Vec (2 * n) a
merge x y = concat $ zipWith (\a b -> a :> singleton b) x y

reverse :: Vec n a -> Vec n a
reverse xs = go Nil xs
 where
  go :: i <= n => Vec (n - i) a -> Vec i a -> Vec n a
  go a (y `Cons` ys) = go (y `Cons` a) ys
  go a Nil = a

map :: (a -> b) -> Vec n a -> Vec n b
map _ Nil           = Nil
map f (x `Cons` xs) = f x `Cons` map f xs

imap :: forall n a b . (KnownNat n) => (Fin n -> a -> b) -> Vec n a -> Vec n b
imap f = withSNat @n $ \snat -> withInduction (toUNat snat) $ go 0
  where
    -- NOTE This has a black box called imap_go
    go :: Induction n => Int -> Vec m a -> Vec m b
    go _ Nil           = Nil
    go n (x `Cons` xs) = f (unsafeFromNum n) x `Cons` go (n+1) xs

withInduction :: KnownNat n => UNat n -> (Induction n => r) -> r
withInduction UZero k = k
withInduction (USucc n) k =
  case fromUNat n of
    SNat -> withInduction n k


izipWith :: KnownNat n => (Fin n -> a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
izipWith f xs ys = imap (\i -> uncurry (f i)) (zip xs ys)

ifoldr :: KnownNat n => (Fin n -> a -> b -> b) -> b -> Vec n a -> b
ifoldr f z xs = head ws
  where
    ws = izipWith f xs ((tail ws)) :< z

ifoldl :: KnownNat n => (a -> Fin n -> b -> a) -> a -> Vec n b -> a
ifoldl f z xs = last ws
  where
    ws = z `Cons` izipWith (\i b a -> f a i b) xs (init ws)

indices :: KnownNat n => SNat n -> Vec n (Fin n)
indices _ = indicesI

indicesI :: KnownNat n => Vec n (Fin n)
indicesI = imap const (repeat ())

findIndex :: KnownNat n => (a -> Bool) -> Vec n a -> Maybe (Fin n)
findIndex f = ifoldr (\i a b -> if f a then Just i else b) Nothing

elemIndex :: (KnownNat n, Eq a) => a -> Vec n a -> Maybe (Fin n)
elemIndex x = findIndex (x ==)

zipWith :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
zipWith _ Nil           _  = Nil
zipWith f (x `Cons` xs) ys = f x (head ys) `Cons` zipWith f xs (tail ys)

zipWith3 :: (a -> b -> c -> d) -> Vec n a -> Vec n b -> Vec n c -> Vec n d
zipWith3 f us vs ws = zipWith (\a (b,c) -> f a b c) us (zip vs ws)

foldr :: (a -> b -> b) -> b -> Vec n a -> b
foldr _ z Nil           = z
foldr f z (x `Cons` xs) = f x (foldr f z xs)

foldl :: forall b a n . (b -> a -> b) -> b -> Vec n a -> b
foldl f z0 xs0 = go z0 xs0
 where
  go :: forall m. b -> Vec m a -> b
  go z Nil = z
  go z (Cons x xs) =
    let z1 = f z x
    in z1 `seq` go z1 xs

foldr1 :: (a -> a -> a) -> Vec (n + 1) a -> a
foldr1 f xs = foldr f (last xs) (init xs)

foldl1 :: (a -> a -> a) -> Vec (n + 1) a -> a
foldl1 f xs = foldl f (head xs) (tail xs)

fold :: forall n a . (a -> a -> a) -> Vec (n + 1) a -> a
fold f vs = fold' (toList vs)
  where
    fold' [x] = x
    fold' xs  = fold' ys `f` fold' zs
      where
        (ys,zs) = P.splitAt (P.length xs `div` 2) xs

scanl :: (b -> a -> b) -> b -> Vec n a -> Vec (n + 1) b
scanl f z xs = ws
  where
    ws = z `Cons` zipWith (flip f) xs (init ws)

scanl1 :: forall n a. (a -> a -> a) -> Vec (n+1) a -> Vec (n+1) a
scanl1 op vs = scanl op (head vs) (tail vs)

scanr1 :: forall n a. (a -> a -> a) -> Vec (n+1) a -> Vec (n+1) a
scanr1 op vs = scanr op (last vs) (init vs)

postscanl :: (b -> a -> b) -> b -> Vec n a -> Vec n b
postscanl f z xs = tail (scanl f z xs)

scanr :: (a -> b -> b) -> b -> Vec n a -> Vec (n + 1) b
scanr f z xs = ws
  where
    ws = zipWith f xs ((tail ws)) :< z

postscanr :: (a -> b -> b) -> b -> Vec n a -> Vec n b
postscanr f z xs = init (scanr f z xs)

mapAccumL :: (acc -> x -> (acc,y)) -> acc -> Vec n x -> (acc,Vec n y)
mapAccumL f acc xs = (acc',ys)
  where
    accs        = acc `Cons` accs'
    ws          = zipWith (flip f) xs (init accs)
    (accs', ys) = unzip ws
    acc'        = last accs

mapAccumR :: (acc -> x -> (acc,y)) -> acc -> Vec n x -> (acc, Vec n y)
mapAccumR f acc xs = (acc',ys)
  where
    accs        = accs' :< acc
    ws          = zipWith (flip f) xs (tail accs)
    (accs', ys) = unzip ws
    acc'        = head accs

zip :: Vec n a -> Vec n b -> Vec n (a,b)
zip = zipWith (,)

zip3 :: Vec n a -> Vec n b -> Vec n c -> Vec n (a,b,c)
zip3 = zipWith3 (,,)

unzip :: Vec n (a,b) -> (Vec n a, Vec n b)
unzip Nil = (Nil, Nil)
unzip (~(a,b) `Cons` rest) =
  let (as, bs) = unzip rest
  in (a `Cons` as, b `Cons` bs)

unzip3 :: Vec n (a,b,c) -> (Vec n a, Vec n b, Vec n c)
unzip3 Nil = (Nil, Nil, Nil)
unzip3 (~(a,b,c) `Cons` rest) =
  let (as, bs, cs) = unzip3 rest
  in (a `Cons` as, b `Cons` bs, c `Cons` cs)


index_int :: Vec n a -> Int -> a
index_int xs i
  | i < 0 = error "Clash.Sized.Vector.(!!): negative index"
  | otherwise          = sub xs i
  where
    sub :: Vec m a -> Int -> a
    sub Nil     _ = error (P.concat [ "Clash.Sized.Vector.(!!): index "
                                    , show i
                                    , " is larger than maximum index."
                                    ])
    sub (y `Cons` (!ys)) n = if (n == 0)
                                then y
                                else sub ys (n - 1)

(!!) :: Vec n a -> Fin n -> a
xs !! i = Lens.view (voice i) xs

voice :: Fin n -> Lens.Lens' (Vec n a) a
voice i f xs = replace_int xs (finToInt i) <$> f (index_int xs (finToInt i))

length :: KnownNat n => Vec n a -> Int
length = fromInteger . natVal . asNatProxy

replace_int :: Vec n a -> Int -> a -> Vec n a
replace_int xs i a
  | i < 0 = error "Clash.Sized.Vector.replace: negative index"
  | otherwise          = sub xs i a
  where
    sub :: Vec m b -> Int -> b -> Vec m b
    sub Nil     _ _ = error (P.concat [ "Clash.Sized.Vector.replace: index "
                                      , show i
                                      , " is out of bounds: "
                                      ]
                            )
    sub (y `Cons` (!ys)) n b = if n == 0
                                 then b `Cons` ys
                                 else y `Cons` sub ys (n - 1) b

take :: SNat m -> Vec (m + n) a -> Vec m a
take n = fst . splitAt n

takeI :: KnownNat m => Vec (m + n) a -> Vec m a
takeI = withSNat take

drop :: SNat m -> Vec (m + n) a -> Vec n a
drop n = snd . splitAt n

dropI :: KnownNat m => Vec (m + n) a -> Vec n a
dropI = withSNat drop

at :: SNat m -> Vec (m + (n + 1)) a -> a
at n xs = head $ snd $ splitAt n xs

replicate :: SNat n -> a -> Vec n a
replicate n a = replicateU (toUNat n) a

replicateU :: UNat n -> a -> Vec n a
replicateU UZero     _ = Nil
replicateU (USucc s) x = x `Cons` replicateU s x

repeat :: KnownNat n => a -> Vec n a
repeat = withSNat replicate

iterate :: SNat n -> (a -> a) -> a -> Vec n a
iterate SNat = iterateI

iterateI :: forall n a. KnownNat n => (a -> a) -> a -> Vec n a
iterateI f = iterateU (toUNat (SNat @n))
 where
  iterateU :: forall m. UNat m -> a -> Vec m a
  iterateU UZero _ = Nil
  iterateU (USucc s) a = a `Cons` iterateU s (f a)

unfoldr :: SNat n -> (s -> (a,s)) -> s -> Vec n a
unfoldr SNat = unfoldrI

unfoldrI :: KnownNat n => (s -> (a,s)) -> s -> Vec n a
unfoldrI f s0 = map fst xs
 where
  xs = init (f s0 `Cons` ws)
  ws = map (f . snd) (lazyV xs)

generate :: SNat n -> (a -> a) -> a -> Vec n a
generate SNat f a = iterateI f (f a)

generateI :: KnownNat n => (a -> a) -> a -> Vec n a
generateI f a = iterateI f (f a)

transpose :: KnownNat n => Vec m (Vec n a) -> Vec n (Vec m a)
transpose = traverse# id

toList :: Vec n a -> [a]
toList = foldr (:) []

fromList :: forall n a. (KnownNat n) => [a] -> Maybe (Vec n a)
fromList xs
  | exactLength (natToInteger @n) xs = Just (unsafeFromList xs)
  | otherwise = Nothing
 where
  exactLength 0 acc = null acc
  exactLength _ []  = False
  exactLength i (_:ys) = exactLength (i - 1) ys

unsafeFromList :: forall n a. (KnownNat n) => [a] -> Vec n a
unsafeFromList = unfoldr SNat go
 where
  go :: [a] -> (a, [a])
  go (x:xs) = (x, xs)
  go [] =
    let item = error "Clash.Sized.Vector.unsafeFromList: vector larger than list"
     in (item, [])

-- | 'Vec'tor as a 'Proxy' for 'Nat'
asNatProxy :: Vec n a -> Proxy n
asNatProxy _ = Proxy

-- | Length of a 'Vec'tor as an 'SNat' value
lengthS :: KnownNat n => Vec n a -> SNat n
lengthS _ = SNat

lazyV :: KnownNat n
      => Vec n a
      -> Vec n a
lazyV = lazyV' (repeat ())
  where
    lazyV' :: Vec n () -> Vec n a -> Vec n a
    lazyV' Nil           _  = Nil
    lazyV' (_ `Cons` xs) ys = head ys `Cons` lazyV' xs (tail ys)

-- | The largest element of a non-empty vector
maximum ::
  Ord a =>
  Vec (n + 1) a ->
  a
maximum = fold (\x y -> if x >= y then x else y)

-- | The least element of a non-empty vector
minimum ::
  Ord a =>
  Vec (n + 1) a ->
  a
minimum = fold (\x y -> if x <= y then x else y)

-- | Evaluate all elements of a vector to WHNF, returning the second argument
seqV
  :: Vec n a
  -> b
  -> b
seqV v b =
  let s () e = seq e () in
  foldl s () v `seq` b
infixr 0 `seqV`

-- | Evaluate all elements of a vector to WHNF
forceV
  :: Vec n a
  -> Vec n a
forceV v =
  v `seqV` v

instance (KnownNat n, Arbitrary a) => Arbitrary (Vec n a) where
  arbitrary = traverse# id $ repeat arbitrary
  shrink    = traverse# id . fmap shrink

instance CoArbitrary a => CoArbitrary (Vec n a) where
  coarbitrary = coarbitrary . toList

type instance Lens.Index   (Vec n a) = Fin n
type instance Lens.IxValue (Vec n a) = a
instance KnownNat n => Lens.Ixed (Vec n a) where
  ix i f xs = replace_int xs (finToInt i) <$> f (index_int xs (finToInt i))

finToInt :: Fin n -> Int
finToInt FZ = 0
finToInt (FS n) = 1 + finToInt n

instance KnownNat n => Distributive (Vec n) where
    distribute fxs = tabulate $ \i -> fmap (!! i) fxs

instance KnownNat n => Representable (Vec n) where
    type Rep (Vec n) = Fin n
    tabulate f = map f indicesI
    index = (!!)

