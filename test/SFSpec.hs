{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans      #-}

module SFSpec where

import Data.Containers.ListUtils (nubOrd)
import Data.List (sort)
import Prelude hiding (id, (.))
import Control.Category
import FRP
import Harness
import Test.Hspec
import Test.QuickCheck.Classes ()
import Test.QuickCheck
import Test.QuickCheck.Checkers

data SFMaker a b where
  Id :: SFMaker a a
  Arr :: Fun a b -> SFMaker a b
  TakeE :: Int -> SFMaker (Event a) (Event a)
  DropE :: Int -> SFMaker (Event a) (Event a)
  FilterE :: (Show a, Function a, CoArbitrary a) => Fun a Bool -> SFMaker (Event a) (Event a)
  NotYet :: SFMaker (Event a) (Event a)
  Offset :: Time -> SFMaker a a
  Hold :: a -> SFMaker (Event a) a
  Switch :: (Arbitrary (SFMaker a b), Arbitrary (SFMaker a (b, Event Bool)), Show b) => SFMaker a (b, Event Bool) -> Fun Bool (SFMaker a b) -> SFMaker a b
  Fork :: (Show b, Show c, Arbitrary (SFMaker a b), Arbitrary (SFMaker a c)) => SFMaker a b -> SFMaker a c -> SFMaker a (b, c)
  (:>>>) :: (Arbitrary (SFMaker a b), Arbitrary (SFMaker b c), Show b) => SFMaker a b -> SFMaker b c -> SFMaker a c
  At :: (Show b, Arbitrary b) => Time -> b -> SFMaker a (Event b)
  Never :: SFMaker a (Event b)
  Steps :: Ord a => a -> [(Time, a)] -> SFMaker x a
  Discrete :: (Show a, Ord a, Arbitrary a) => [(Time, a)] -> SFMaker x (Event a)
  Fst :: SFMaker (a, b) a

shrinkSFMaker :: (Function a, CoArbitrary a, Arbitrary b)  => SFMaker a b -> [SFMaker a b]
shrinkSFMaker Id = []
shrinkSFMaker (Arr f) = Arr <$> shrink f
shrinkSFMaker (TakeE n) = Never : Id : (TakeE <$> shrink n)
shrinkSFMaker (DropE n) = Never : Id : (DropE <$> shrink n)
shrinkSFMaker (FilterE f) = Never : Id : (FilterE <$> shrink f)
shrinkSFMaker NotYet = [Never]
shrinkSFMaker (Offset n) = Offset <$> shrink n
shrinkSFMaker (Hold a) = Hold <$> shrink a
shrinkSFMaker (Switch f g) = mconcat
  [ Switch <$> shrink f <*> pure g
  , Switch f <$> shrink g
  ]
shrinkSFMaker (Fork f g) = mconcat
  [ Fork <$> shrink f <*> pure g
  , Fork f <$> shrink g
  ]
shrinkSFMaker (f :>>> g) = mconcat
  [ (:>>>) <$> shrink f <*> pure g
  , (:>>>) f <$> shrink g
  ]
shrinkSFMaker (At t b) = mconcat
  [ pure Never
  , At <$> shrink t <*> pure b
  , At t <$> shrink b
  ]
shrinkSFMaker Fst = []
shrinkSFMaker Never = []
shrinkSFMaker (Discrete as) = Never : fmap Discrete (fmap (sort . nubOrd) $ shrink as)
shrinkSFMaker (Steps a as) = mconcat
  [ Steps <$> shrink a <*> pure as
  , Steps a <$> fmap (sort . nubOrd) (shrink as)
  ]

infixr 1 :>>>

deriving stock instance (Show a, Show b) => Show (SFMaker a b)

instance Arbitrary (SFMaker (a, b) a) where
  arbitrary = pure Fst

toSF :: SFMaker a b -> SF a b
toSF Id = id
toSF (Arr (Fn f)) = arr f
toSF (TakeE n) = takeE n
toSF (DropE n) = dropE n
toSF (FilterE (Fn f)) = filterE f
toSF NotYet = notYet
toSF (Offset t) = offset t
toSF (Hold h) = hold h
toSF (Switch sf (Fn k)) = switch (toSF sf) $ toSF . k
toSF (Fork f g) = toSF f &&& toSF g
toSF (f :>>> g) = toSF g <<< toSF f
toSF (At t b) = at t b
toSF (Steps a as) = steps a as
toSF (Discrete as) = discrete as
toSF Fst = arr fst
toSF Never = never

instance (Arbitrary (SFMaker a b), Arbitrary (SFMaker a c), Function a, CoArbitrary a, Arbitrary b, Arbitrary c, Show b, Show c) => Arbitrary (SFMaker a (b, c)) where
  arbitrary = oneof
    [ Arr <$> arbitrary
    , Fork <$> arbitrary <*> arbitrary
    ]
  shrink = shrinkSFMaker


instance (Ord a, Function a, CoArbitrary a, Arbitrary a, Arbitrary (SFMaker a Int), Arbitrary (SFMaker Int a), Arbitrary (SFMaker a (Event Bool)), Show a, Arbitrary (SFMaker a (Event Int))) => Arbitrary (SFMaker a a) where
  arbitrary = oneof
    [ Arr <$> arbitrary
    , Offset <$> arbitrary
    , (:>>>) @_ @Int <$> arbitrary <*> arbitrary
    , Switch <$> arbitrary <*> arbitrary
    , Steps <$> arbitrary <*> (zip <$> genTimes <*> arbitrary)
    ]
  shrink = shrinkSFMaker

instance Function a => Function (Event a) where

instance (Ord b, CoArbitrary a, Arbitrary b, Function a, Show b) => Arbitrary (SFMaker (Event a) (Event b)) where
  arbitrary = oneof
    [ Arr <$> arbitrary
    , At <$> arbitrary <*> arbitrary
    , Discrete <$> (zip <$> genTimes <*> arbitrary)
    ]
  shrink = shrinkSFMaker

instance {-# OVERLAPPING #-} (Ord a, CoArbitrary a, Arbitrary a, Function a, Show a) => Arbitrary (SFMaker (Event a) (Event a)) where
  arbitrary = frequency
    [ (10,) $ Arr <$> arbitrary
    , (1,) $ TakeE <$> arbitrary
    , (1,) $ DropE <$> arbitrary
    , (1,) $ pure NotYet
    , (1,) $ FilterE <$> arbitrary
    , (1,) $ Offset <$> arbitrary
    , (10,) $ Switch <$> arbitrary <*> arbitrary
    , (3,) $ Discrete <$> (zip <$> genTimes <*> arbitrary)
    ]
  shrink = shrinkSFMaker


instance (Show a, EqProp (Signal b), Arbitrary (Signal a)) => EqProp (SF a b) where
  sf1 =-= sf2 = property $ \s -> runSF sf1 s =-= runSF sf2 s

spec :: Spec
spec =
  batchSpec $ arrow @SFMaker @(Event Int) @(Event Int) @(Event Int) @(Event Int) undefined

arrow :: forall a b c d e.
         ( a ~ SFMaker
         , Show (a d e), Show (a c d), Show (a b c)
         , Arbitrary (a d e), Arbitrary (a c d), Arbitrary (a b c)
         , Arbitrary c, Arbitrary d, Arbitrary e
         , CoArbitrary b, CoArbitrary c, CoArbitrary d
         , EqProp (Signal (c, e))
         , EqProp (Signal c)
         , EqProp (Signal (d, e))
         , EqProp (Signal (c, d))
         , Arbitrary (Signal (b, d))
         , Arbitrary (Signal (b, e))
         , Arbitrary (Signal b)
         , EqProp (Signal d)
         , EqProp (Signal e)
         , Show b
         , Show d
         , Show e
         ) =>
         a b (c,d,e) -> TestBatch
arrow = const ("arrow laws"
              , [ ("associativity"           , property assocP)
                , ("arr distributes"         , property arrDistributesP)
                 , ("first works as funs"    , property firstAsFunP)
                 , ("first keeps composition", property firstKeepCompP)
                 , ("first works as fst"     , property firstIsFstP)
                 , ("second can move"        , property secondMovesP)
                 ]
              )
  where
    assocP :: a b c -> a c d -> a d e -> Property
    assocP (toSF -> f) (toSF -> g) (toSF -> h) = ((f >>> g) >>> h) =-= (f >>> (g >>> h))

    arrDistributesP :: (b -> c) -> (c -> d) -> Property
    arrDistributesP f g = ((arr (f >>> g)) :: SF b d) =-= (arr f >>> arr g)

    firstAsFunP :: (b -> c) -> Property
    firstAsFunP f = (first (arr f) :: SF (b,d) (c,d)) =-= arr (first f)

    firstKeepCompP :: a b c -> a c d -> Property
    firstKeepCompP (toSF -> f) (toSF -> g) =
      ((first (f >>> g)) :: (SF (b,e) (d,e))) =-= (first f >>> first g)

    firstIsFstP :: a b c -> Property
    firstIsFstP (toSF -> f) = ((first f :: SF (b,d) (c,d)) >>> arr fst)
                      =-= (arr fst >>> f)

    secondMovesP :: (a b c) -> (d -> e) -> Property
    secondMovesP (toSF -> f) g = (first f >>> second (arr g))
                         =-= ((second (arr g)) >>> first f)

  -- 1) arrow laws first keeps composition
  --      Falsified (after 11 tests and 24 shrinks):
  --        NotYet
  --        Switch (Fork (Arr {_->Nothing}) (Arr {Nothing->Just False, _->Nothing})) {_->Arr {_->Just 0}}
  --        [((-7) % 1,(Nothing,Nothing)),((-14) % 3,(Nothing,Nothing)),(1 % 4,(Nothing,Nothing)),(4 % 3,(Nothing,Nothing))]
  --        0 % 1
  --        Nothing /= Just 0

  -- To rerun use: --match "/arrow laws/first keeps composition/" --seed 322437866
