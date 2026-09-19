{-# LANGUAGE BlockArguments  #-}

module FRP.TimeSig
  ( beatsOf
  , subdivide
  , swing
  , time2'2
  , time2'4
  , time3'4
  , time3'8
  , time4'4
  , time6'8
  , time9'8
  , time12'8
  ) where

import Data.Coerce
import Data.Foldable
import Data.Functor.Foldable
import FRP.Types


subdivide :: Int -> Time -> Meter Time
subdivide n t = Group $ replicate n $ Pulse $ (t / fromIntegral n)

swing :: Rational -> Time -> Meter Time
swing r t = Group
  [ Pulse $ t * r
  , Pulse $ t * (1 - r)
  ]


depth :: Meter a -> Int
depth = cata $ \case
  PulseF _   -> 0
  GroupF rs  -> 1 + maximum (0 : rs)

weight :: Meter a -> Meter (a, Priority)
weight m =
  let d = depth m
   in fmap (fmap $ coerce (d -)) $ weight' m

weight' :: Meter a -> Meter (a, Priority)
weight' m = flip ana (depth m, m) $ \case
  (w, Pulse a)  -> PulseF (a, P w)
  (w, Group cs) -> GroupF $ do
    (i, c) <- zip [0 ..] cs
    pure $ (, c) $ case i == 0 of
      True -> w
      False -> depth c



time2'2 :: Meter Time
time2'2 = pure (2/2) >>= subdivide 2 >>= subdivide 2

time2'4 :: Meter Time
time2'4 = pure (2/4) >>= subdivide 2 >>= subdivide 2

time4'4 :: Meter Time
time4'4 = pure (4/4) >>= subdivide 2 >>= subdivide 2 >>= subdivide 2

time3'4 :: Meter Time
time3'4 = pure (3/4) >>= subdivide 3 >>= subdivide 2

time3'8 :: Meter Time
time3'8 = pure (3/8) >>= subdivide 3 >>= subdivide 2

time6'8 :: Meter Time
time6'8 = pure (6/8) >>= subdivide 2 >>= subdivide 3

time9'8 :: Meter Time
time9'8 = pure (9/8) >>= subdivide 3 >>= subdivide 3

time12'8 :: Meter Time
time12'8 = pure (12/8) >>= subdivide 2 >>= subdivide 2 >>= subdivide 3

beatsOf :: Meter Time -> SF x (Event Beat)
beatsOf m = do
  let bs = toList $ weight m
      dur = sum $ fmap fst bs
      ts = init $ scanl (+) 0 $ fmap fst bs
      clk' = ts <> fmap (+ dur) clk'
      ws = zip ts $ fmap (uncurry Beat) bs
  SF $ \_ ->
    Signal (Clock clk') $ \t -> do
      let t' = t - fromIntegral (floor $ t / dur)
      MkEvent $ lookup t' ws

