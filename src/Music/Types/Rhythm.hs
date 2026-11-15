module Music.Types.Rhythm where

import Data.Foldable
import Music.Beat
import FRP

unit :: Meter Rational
unit = Pulse 1

(<.>) :: Meter Rational -> Meter Rational -> Meter Rational
m1 <.> m2 = Group [fmap (/ 2) m1 , fmap (/ 2) m2]

lfan :: Int -> Meter Rational
lfan 0 = unit
lfan n = unit <.> lfan (n - 1)

rfan :: Int -> Meter Rational
rfan 0 = unit
rfan n = rfan (n - 1) <.> unit

lstack :: Int -> Meter Rational
lstack 0 = unit
lstack n = unit <.> rstack (n - 1)

rstack :: Int -> Meter Rational
rstack 0 = unit
rstack n = lstack (n - 1) <.> unit

tortoise :: Int -> Meter Rational
tortoise 0 = unit
tortoise n = tortoise (n - 1) <.> tortoise (n - 1)

allShapes :: Int -> [Meter Rational]
allShapes 0 = []
allShapes 1 = [unit]
allShapes n = do
  k <- [1 .. n - 1]
  l <- allShapes k
  r <- allShapes $ n - k
  pure $ l <.> r

determineDepth :: Int -> Rational -> Int
determineDepth desired r
  = floor (log (realToFrac r) / log 2) + desired


mop
    :: (a -> a -> (b, Meter Rational))
    -> Signal (Event a)
    -> Signal (Event b)
mop f = runSF $ evByEv $ \(t1, a1) (t2, a2) -> do
  let (b, sh) = f a1 a2
      dur = t2 - t1
  fmap (, b) $ scanl (\t d -> (t + dur * Time d)) t1 $ toList sh

