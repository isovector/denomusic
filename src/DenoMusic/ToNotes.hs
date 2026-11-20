{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Flatten the structured music types of this library into a Euterpea
-- 'E.Music' of @'Note' n@. This is the single source of truth for both
-- notation and playback ("DenoMusic.Play" goes via @'Note' 12@). It stops at
-- scale degrees rather than
-- collapsing everything into absolute MIDI pitches, so that staves like
-- 'DenoMusic.NotationBackend.IntrinsicStaff' can see the scale structure.
module DenoMusic.ToNotes
  ( ToNotes (..)
  , toNotesMusic
  ) where

import Data.Foldable
import Data.Set (Set)
import Euterpea qualified as E
import FRP (stretchS)
import FRP.Types
import Music.Harmony
import Music.Types


-- | Things which can be notated as music in an @n@-note scale.
class ToNotes n a where
  toNotes :: E.Music a -> E.Music (Note n)

-- | Wrap a single value as a whole note and flatten it.
toNotesMusic :: forall n a. ToNotes n a => a -> E.Music (Note n)
toNotesMusic = toNotes @n . E.note 1


bindMusic :: (E.Dur -> a -> E.Music b) -> E.Music a -> E.Music b
bindMusic f =
  E.mFold
    (\case
      E.Note dur a -> f dur a
      E.Rest dur -> E.rest dur
    )
    (E.:+:)
    (E.:=:)
    E.Modify

noteAt :: forall n a. ToNotes n a => E.Dur -> a -> E.Music (Note n)
noteAt dur = toNotes @n . E.note dur


------------------------------------------------------------------------------
-- Leaves

instance ToNotes n (Note n) where
  toNotes = id

instance ToNotes n (Deg n) where
  toNotes = fmap $ \d -> Note d mempty

instance ToNotes 12 (Note 7) where
  toNotes = fmap $ applyScale diatonic

instance ToNotes 12 (Note 3) where
  toNotes = fmap $ applyScale diatonic . applyScale triad

instance ToNotes 7 (Note 3) where
  toNotes = fmap $ applyScale triad


------------------------------------------------------------------------------
-- Structure

instance {-# OVERLAPPING #-} ToNotes n a => ToNotes n (Interval Time, a) where
  toNotes = bindMusic $ \dur (Interval lo hi, a) ->
    E.rest (unTime lo) E.:+: noteAt @n (unTime (hi - lo) * dur) a

instance ToNotes n a => ToNotes n (Span a) where
  toNotes = bindMusic $ \dur (Span t a) -> noteAt @n (dur * unTime t) a

instance ToNotes n a => ToNotes n (Set a) where
  toNotes = toNotes @n . fmap toList

instance ToNotes n a => ToNotes n [a] where
  toNotes = bindMusic $ \dur v ->
    case fmap (noteAt @n dur) v of
      [] -> E.Prim $ E.Rest 0
      ms -> foldr1 (E.:=:) ms

instance {-# OVERLAPPABLE #-} (ToNotes n a, ToNotes n b) => ToNotes n (a, b) where
  toNotes = bindMusic $ \dur (a, b) ->
    noteAt @n dur a E.:=: noteAt @n dur b

instance (KnownNat v, ToNotes n a) => ToNotes n (Vertical v a) where
  toNotes = toNotes @n . fmap toList

instance ToNotes n (SF i (Event (Span o))) => ToNotes n (Horizontal r i o a) where
  toNotes = bindMusic $ \dur -> noteAt @n dur . horizontal

instance ToNotes n o => ToNotes n (Signal (Event o)) where
  toNotes = bindMusic $ \dur v ->
    let go _ [] = E.rest 0
        go now ((t, a) : es) =
          let onset = unTime t
              m     = noteAt @n dur a
           in (E.rest (onset - now) E.:+: m) E.:+: go (onset + E.dur m) es
     in go 0 $ events $ stretchS dur v

instance ToNotes n (Signal a) => ToNotes n (SF () a) where
  toNotes = toNotes @n . fmap (flip runSF $ pure ())

instance (ToNotes n (Signal a), ToNotes n (Signal b)) => ToNotes n (Signal (a, b)) where
  toNotes = bindMusic $ \dur sf ->
    noteAt @n dur (fmap fst sf) E.:=: noteAt @n dur (fmap snd sf)

instance (ToNotes n (Signal a), ToNotes n (Signal b), ToNotes n (Signal c))
    => ToNotes n (Signal (a, b, c)) where
  toNotes = bindMusic $ \dur sf ->
    noteAt @n dur (fmap (\(a, _, _) -> a) sf)
      E.:=: noteAt @n dur (fmap (\(_, b, _) -> b) sf)
      E.:=: noteAt @n dur (fmap (\(_, _, c) -> c) sf)
