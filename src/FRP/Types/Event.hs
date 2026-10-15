module FRP.Types.Event
  ( Event (Event, NoEvent)
  , eventToMaybe
  , event
  ) where

import Control.Applicative
import Data.Generics.Labels ()
import GHC.Generics

newtype Event a = MkEvent
  { eventToMaybe :: Maybe a
  }
  deriving stock (Foldable, Traversable, Generic)
  deriving newtype (Functor, Applicative, Monad, Eq, Ord, Show, Alternative)

{-# COMPLETE Event, NoEvent #-}
pattern Event :: a -> Event a
pattern Event a = MkEvent (Just a)

pattern NoEvent :: Event a
pattern NoEvent = MkEvent Nothing

event :: b -> (a -> b) -> Event a -> b
event b f = maybe b f . eventToMaybe

instance Semigroup a => Semigroup (Event a) where
  NoEvent <> a = a
  Event a <> NoEvent = Event a
  Event a <> Event b = Event (a <> b)

instance Semigroup a => Monoid (Event a) where
  mempty = NoEvent

