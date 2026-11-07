{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RoleAnnotations #-}

module Music.Types
  ( module Music.Types
  , module Music.Types.Note
  , module Music.Types.Scale
  , module Music.Types.Vertical
  ) where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad.Cont
import Data.Bifunctor
import Data.Bool (bool)
import Data.Coerce
import Data.Functor
import Data.Kind
import Data.Maybe (catMaybes)
import Data.Monoid (Ap(..))
import Data.Semigroup
import DenoMusic.Types (Reg(..))
import FRP
import GHC.Generics
import Music.Harmony
import Music.Types.Note
import Music.Types.Scale
import Music.Types.Vertical
import Prelude hiding (id, (.))


divModNote :: forall n. KnownNat n => Note n -> Reg (Note n)
divModNote (Note d as)
  = uncurry Reg
  . fmap (flip Note as)
  $ coerce (flip divMod $ scaleSize @n @Int) d


undivModNote :: forall n. KnownNat n => Reg (Note n) -> Note n
undivModNote (Reg n d) = d + fromIntegral (n * scaleSize @n)


-- | Add a duration for each event that lasts until the next clocked time
-- (regardless of whether that clocked time is 'Event' or 'NoEvent'.)
spanning :: SF (Event (), Event a) (Event (Span a))
spanning = proc (end, a) ->
  ev2ev
    (\es -> catMaybes
      $ zipWith
          (\(t1, ema) (t2, _) -> case ema of
            Just a -> Just (t1, Span (t2 - t1) a)
            Nothing -> Nothing
          )
          es
      $ drop 1 es
    ) -< fmap Just a <|> Nothing <$ end


latestOf :: SF (Event a) (Event a)
latestOf = ev2ev $ \case
  [] -> []
  xs -> [last xs]

dropShorterThan :: Time -> SF (Event a) (Event a)
dropShorterThan dt = evByEv $ \(t, a) (t', _) ->
  case t' - t >= dt of
    True -> pure (t, a)
    False -> mempty

clampOctave :: KnownNat s => (Note s, Note s) -> Time -> SF (Event (Note s)) (Event (Note s))
clampOctave (lo, hi) dt = proc e -> do
  t <- localTime -< ()
  ef <- futureE -< e
  rec
    eprev <- hold 0 <<< offset epsilon -< e'
    oprev <- hold mempty <<< offset epsilon -< fmap fst eo
    let eo = ef <&> \(n, fut) -> do
          let window = fmap snd $ takeWhile ((<= t + dt) . fst) fut
              n0 = addOctave oprev n
              tryJump o' ok =
                -- Only jump if no upcoming input note (within dt) would
                -- cross back over the opposite bound under the new octave.
                bool oprev o' $ all (ok . addOctave o') window
              o | eprev <= hi && n0 > hi = tryJump (oprev <> Octave (-1)) (>= lo)
                | eprev >= lo && n0 < lo = tryJump (oprev <> Octave 1)    (<= hi)
                | otherwise              = oprev
          (o, addOctave o n)
        e' = fmap snd eo
  returnA -< e'

-- | Annotate each event with every input event strictly after it (lazily).
-- Purely a reverse-time fold over the input, so it never depends on feedback.
futureE :: SF (Event a) (Event (a, [(Time, a)]))
futureE = proc e -> do
  t <- localTime -< ()
  rec
    fut <- fhold [] -< fmap (\a -> (t, a) : fut) e
  returnA -< fmap (, fut) e


data HState a = HState
  { hs_out :: a
  , hs_starts :: Event ()
  , hs_stops :: Event ()
  }
  deriving stock (Functor, Generic)
  deriving (Semigroup, Monoid) via Generically (HState a)


data Restful = HasRests

type role Horizontal nominal representational representational
type Horizontal :: Restful -> Type -> Type -> Type
newtype Horizontal r o a = Horizontal
  { unHorizontal :: forall b. (o -> b) -> Cont (Signal (HState b)) a
  }

deriving stock instance Functor (Horizontal r o)

instance Applicative (Horizontal r o) where
  pure a = Horizontal $ const $ pure a
  liftA2 f (Horizontal a) (Horizontal b) = Horizontal $ \k -> liftA2 f (a k) (b k)

instance Monad (Horizontal r o) where
  Horizontal m >>= f = Horizontal $ \k -> m k >>= \a -> unHorizontal (f a) k

deriving via Ap (Horizontal r o) a instance Semigroup a => Semigroup (Horizontal r o a)
deriving via Ap (Horizontal r o) a instance Monoid a => Monoid (Horizontal r o a)

mkHorizontal :: Signal o -> Time -> a -> Horizontal r o a
mkHorizontal so t a = Horizontal $ \k -> cont $ switchS $ do
  o <- so
  start <- atS 0 ()
  stop <- atS t ()
  ea <- atS t a
  return (HState (k o) start stop, ea)

fill :: Time -> a -> Horizontal r a a
fill t a = mkHorizontal (pure a) t a

hit :: Time -> a -> Horizontal r a ()
hit t = void . fill t

rest :: Time -> Horizontal HasRests a ()
rest t = Horizontal $ const $ cont $ switchS $ do
  stop <- atS 0 ()
  ea <- atS t ()
  return (HState (error "impossible: rest can't be sampled") NoEvent stop, ea)


unsafeRunHorizontal :: Horizontal r o a -> (a -> Signal o) -> Signal (HState o)
unsafeRunHorizontal (Horizontal f) k = runCont (f id) $ fmap (\o -> HState o NoEvent NoEvent) . k

switchHorizontal :: (forall r. Horizontal r o a) -> (a -> Signal o) -> Signal (HState o)
switchHorizontal h = unsafeRunHorizontal h

getHorizontal :: Horizontal r o a -> Signal (Event o)
getHorizontal h
  = flip runSF (unsafeRunHorizontal h $ const $ pure $ error "impossible: getHorizontal sampled too late")
  $ proc (HState o starts _) -> returnA -< o <$ starts

horizontal :: Horizontal r o a -> Signal (Event (Span o))
horizontal h
  = flip runSF (unsafeRunHorizontal h $ const $ pure $ error "impossible: horizontal sampled too late")
  $ proc (HState o starts stops) -> spanning -< (stops, o <$ starts)

instance Bifunctor (Horizontal r) where
  bimap f g (Horizontal y) = Horizontal $ \k -> fmap g $ y $ k . f

mapOut :: (o -> o') -> Horizontal r o a -> Horizontal r o' a
mapOut f = bimap f id


newtype HM r o = HM { unHM :: Horizontal r o () }
  deriving newtype (Semigroup, Monoid)

instance Functor (HM r) where
  fmap f = HM . bimap f id . unHM

