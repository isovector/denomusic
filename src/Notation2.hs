{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Notation2 where

import Euterpea qualified as E
import Data.Foldable
import Score
import Data.Tree.DUAL
import Data.IntervalMap.FingerTree (IntervalMap, Interval(..))
import Data.IntervalMap.FingerTree qualified as IM
import Data.Music.Lilypond hiding (chord)
import Notation (traversalOrder)

newtype AnnTree n a = AnnTree
  { getAnnTree :: IntervalMap Rational (AnnNode n a)
  }
  deriving stock (Functor, Foldable, Traversable)
  deriving newtype (Semigroup, Monoid)

bounds :: AnnTree n a -> Maybe (Interval Rational)
bounds = IM.bounds . getAnnTree

data AnnNode n a
  = AnnNode n (AnnTree n a)
  | Leaf a
  deriving stock (Functor, Foldable, Traversable)


treeToMusic :: AnnTree Ann E.Pitch -> Music
treeToMusic (AnnTree t) = flip evalState 0 .
  let t' = traversalOrder t
   in _

nodeToMusic :: AnnNode Ann E.Pitch -> Music
nodeToMusic (Leaf a) = undefined
nodeToMusic (AnnNode n a) = treeToMusic a


foldScore :: Score a -> AnnTree Ann a
foldScore = fold .
  foldDUAL
    (\(Envelope s o) a ->
      let lo = min o (o + s)
          hi = max o (o + s)
       in AnnTree $ IM.singleton (Interval lo hi) (Leaf a))
    mempty
    fold
    (const id)
    (\n t ->
      case bounds t of
        Just b -> AnnTree $ IM.singleton b $ AnnNode n t
        Nothing -> t
    )
    . unScore

