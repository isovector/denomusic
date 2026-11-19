module Data.MVec where

import Data.Bifunctor (first)
import Data.Fin
import Data.Foldable
import Data.Kind
import Data.Map (Map)
import Data.Map qualified as M
import Data.SNat
import Data.Typeable
import Data.Vec (Vec (..))
import Data.Vec qualified as V
import GHC.TypeLits hiding (SNat)
import Music.Types.Deg (scaleSize)

-- | @MVec m n a@ is a container with @m@ slots, of which exactly @n@ are
-- filled (with values of @a@.)
type MVec :: Nat -> Nat -> Type -> Type
data MVec m n a where
  MNil :: MVec 0 0 a
  MCons :: a -> MVec m n a -> MVec (m + 1) (n + 1) a
  MSkip :: MVec m n a -> MVec (m + 1) n a

deriving stock instance Eq a => Eq (MVec m n a)
deriving stock instance Ord a => Ord (MVec m n a)
deriving stock instance Show a => Show (MVec m n a)
deriving stock instance Functor (MVec m n)
deriving stock instance Foldable (MVec m n)

instance (m ~ n, KnownNat n) => Applicative (MVec m n) where
  pure a =
    case toUNat $ SNat @n of
      UZero -> MNil
      USucc _ -> MCons a $ pure a
  liftA2 _ MNil MNil = MNil
  liftA2 _ MNil _ = error "impossible"
  liftA2 f (MCons a as) (MCons b bs) =
    MCons (f a b) $ liftA2 f as bs
  liftA2 _ MCons{} _ = error "impossible"
  liftA2 _ MSkip{} _ = error "impossible"


-- | Lookup @n@, returning the value and the corresponding index.
mlookup :: MVec m n a -> Fin n -> (Fin m, a)
mlookup MNil a = case a of
mlookup (MCons a _) FZ = (FZ, a)
mlookup (MCons _ mn) (FS n) =
  case mlookup mn n of
    (m, a) -> (FS m, a)
mlookup (MSkip mn) n =
  case mlookup mn n of
    (m, a) -> (FS m, a)


-- | Try to find @m@, returning a value and index (in @n@) if found. Nothing
-- otherwise.
msearch :: MVec m n a -> Fin m -> Maybe (Fin n, a)
msearch MNil a = case a of
msearch (MCons a _) FZ = Just (FZ, a)
msearch (MCons _ as) (FS n) = fmap (first FS) $ msearch as n
msearch (MSkip _) FZ = Nothing
msearch (MSkip as) (FS n) = msearch as n

embedMVec :: KnownNat n => MVec m n a -> Vec n a
embedMVec = V.unsafeFromList . toList

projectMVec :: MVec m n a -> Vec m (Maybe a)
projectMVec MNil = Nil
projectMVec (MCons a as) = Cons (Just a) $ projectMVec as
projectMVec (MSkip as) = Cons Nothing $ projectMVec as

-- | Composition of 'MVec's by combining the contents with the given function.
-- The idea here is that we "filter out" @b@ entries from @c@ by watching the
-- first 'MVec's cons cells. These correspond to the slots in the @MVec b a@.
-- If both @MVec@s simultaneously have a cons cell, they get combined.
malign :: (x -> y -> z) -> MVec c b x -> MVec b a y -> MVec c a z
malign _ MNil MNil = MNil
malign _ MNil _ = error "impossible"
malign f (MSkip bc) ab = MSkip $ malign f bc ab
malign f (MCons x bc) (MCons y ab) = MCons (f x y) $ malign f bc ab
malign _ MCons {} MNil = error "impossible"
malign f (MCons _ bc) (MSkip ab) = MSkip $ malign f bc ab

data SomeMVec s a where
  SomeMVec :: KnownNat c => MVec s c a -> SomeMVec s a

predMap :: Map (Fin (s + 1)) a -> Map (Fin s) a
predMap m = M.fromList $ do
  (k, a) <- M.toList m
  Just k' <- pure $ isMin k
  pure (k', a)

makeSomeMVec :: forall s a. KnownNat s => Map (Fin s) a -> SomeMVec s a
makeSomeMVec m = knownInduction @s $
  case toUNat (SNat @s) of
    UZero -> SomeMVec MNil
    USucc _ -> do
      case makeSomeMVec $ predMap m of
        SomeMVec rest -> do
          case M.lookup FZ m of
            Nothing -> SomeMVec $ MSkip rest
            Just a -> SomeMVec $ MCons a rest



unsafeMakeMVec :: forall c s a. (KnownNat s, KnownNat c) => Map (Fin s) a -> MVec s c a
unsafeMakeMVec m =
  case makeSomeMVec m of
    SomeMVec (mv :: MVec s c' a) ->
      case eqT @c @c' of
        Just Refl -> mv
        Nothing ->
          error $ unwords
            [ "unsafeMakeMVec: declared as chord size"
            , show $ scaleSize @c
            , "but given"
            , show $ scaleSize @c'
            , "notes."
            ]

