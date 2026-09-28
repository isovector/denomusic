{-# OPTIONS_GHC -Wno-orphans #-}

module FRP
  ( module Control.Arrow
  , module FRP
  , module FRP.Types
  , module FRP.Beat
  , Alternative (..)
  , Interval(..)
  ) where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Monad.Cont
import Data.Bool
import Data.Maybe
import Data.Monoid
import Data.Ratio
import FRP.Beat
import FRP.Types
import Prelude hiding (id, (.))


every :: Time -> a -> SF x (Event a)
every dur a = discrete $ zip (iterate (+ dur) 0) $ repeat a

at :: Time -> a -> SF x (Event a)
at t' a = discrete $ pure (t', a)

-- | Stretch time by the given amount.
stretch :: Rational -> SF a a
stretch r = SF $ invmapTime (* r) (/ r)

now :: a -> SF x (Event a)
now = at 0

switch :: SF a (b, Event c) -> (c -> SF a b) -> SF a b
switch (SF f) k = SF $ \sig -> do
  let sig' = f sig
      sig'1 = fmap fst sig'
  case listToMaybe $ eventsTerminating $ fmap snd sig' of
    Just (t0, a) ->
      spliceAt sig'1 t0 $ runSF (k a) sig
    Nothing -> sig'1

offset :: Time -> SF a a
offset dt = SF $ invmapTime (+ dt) (subtract dt)

localTime :: SF x Time
localTime = SF $ const $ Signal id mempty

replace :: [a] -> SF (Event b) (Event (b, a))
replace as = ev2ev $ \bs -> zipWith (\(t, b) a -> (t, (b, a))) bs as

partitionEvents :: (a -> Either b c) -> SF (Event a) (Event b, Event c)
partitionEvents f = proc eva -> do
  evb <- mapMaybeE (either Just (const Nothing) . f) -< eva
  evc <- mapMaybeE (either (const Nothing) Just . f) -< eva
  returnA -< (evb, evc)


filterE :: (a -> Bool) -> SF (Event a) (Event a)
filterE f = ev2ev $ filter (f . snd)

filterTimeE :: (Time -> Bool) -> SF (Event a) (Event a)
filterTimeE f = ev2ev $ filter (f . fst)

mapMaybeE :: (a -> Maybe b) -> SF (Event a) (Event b)
mapMaybeE = ev2ev . mapMaybe . traverse


gate :: Bool -> Event a -> Event a
gate False _ = NoEvent
gate True e = e

notYet :: SF (Event a) (Event a)
notYet = filterTimeE (> 0)

once :: SF (Event a) (Event a)
once = takeE 1

takeE :: Int -> SF (Event a) (Event a)
takeE = ev2ev . take

dropE :: Int -> SF (Event a) (Event a)
dropE = ev2ev . drop

accum :: a -> SF (Event (a -> a)) (Event a)
accum a0 = ev2ev $ drop 1 . scanl (\(_, a) (t', f) -> (t', f a)) (undefined, a0)

onlyEvery :: Int -> SF (Event a) (Event a)
onlyEvery n = proc ev -> do
  x <- hold 0 <<< accum 0 -< (+1) <$ ev
  returnA -< bool NoEvent ev $ mod x n == 0

subdiv :: Int -> SF (Event Beat) (Event Beat)
subdiv n = ev2ev $ \bs -> do
  (t, Beat d s) <- bs
  let d' = d / fromIntegral n
  take n $ zip (iterate (+ d') t) $ Beat d' s : repeat (Beat d' $ succ s)

newtype Seq i o a = Seq
  { unSeq :: Cont (SF i o) a
  }
  deriving newtype (Functor, Applicative, Monad)


toSeq :: SF i (Event o, Event a) -> Seq i (Event o) a
toSeq = Seq . cont . switch

switchSeq :: Seq i o a -> (a -> SF i o) -> SF i o
switchSeq = runCont . unSeq

getSeq :: Seq i (Event o) a -> SF i (Event o)
getSeq = flip switchSeq $ const $ arr $ const NoEvent

rest :: Time -> Seq i (Event a) ()
rest t = toSeq $ proc i -> do
  e <- at t () -< i
  returnA -< (NoEvent, e)

hit :: Time -> a -> Seq i (Event a) ()
hit t a = toSeq $ proc i -> do
  n <- now a -< i
  e <- at t () -< i
  returnA -< (n, e)

beat :: Time -> Priority -> Seq i (Event Beat) ()
beat t p = hit t $ Beat t p


test :: SF i (Event Char)
test =
  switch
    (liftA2 (,) (getSeq $ replicateM 4 $ hit 0.25 'a') (at 0.5 ()))
    $ const $ getSeq $ replicateM 4 $ hit 1 'b'

