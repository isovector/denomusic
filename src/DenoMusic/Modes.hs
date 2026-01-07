{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
-- We want the size constraints on the modes, but GHC doesn't think they're
-- necessary.
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module DenoMusic.Modes where

import Data.Group
import DenoMusic.Harmony
import GHC.TypeLits


-- | Cancel out a change in @s@ with a corresponding (opposite) shift in @c@.
-- Eg, cancel out scale movement with chromatic movement, which is what we need
-- to do in order to switch modes.
countervail :: forall s c. KnownNat c => MetaScale s -> T '[s] -> T '[s, c]
countervail ms (s :> Nil) = s :> invert (kill ms $ s :> 0 :> Nil)


-- | Mode shift the given scale into dorian. In 'diatonic', this flattens the
-- 3rd and 7th.
dorian :: (2 <= s, KnownNat c) => MetaScale s -> T '[s, c]
dorian = flip countervail [1]


-- | Mode shift the given scale into aeolian. In 'diatonic', this flattens the
-- 2nd, 3rd, 6th and 7th.
phrygian :: (3 <= s, KnownNat c) => MetaScale s -> T '[s, c]
phrygian = flip countervail [2]


-- | Mode shift the given scale into lydian. In 'diatonic', this sharpens the
-- 4th.
lydian :: (4 <= s, KnownNat c) => MetaScale s -> T '[s, c]
lydian = flip countervail [3]


-- | Mode shift the given scale into mixolydian. In 'diatonic', this flattens
-- the 7th.
mixolydian :: (5 <= s, KnownNat c) => MetaScale s -> T '[s, c]
mixolydian = flip countervail [4]


-- | Mode shift the given scale into aeolian. In 'diatonic', this flattens the
-- 3rd, 6th and 7th.
aeolian :: (6 <= s, KnownNat c) => MetaScale s -> T '[s, c]
aeolian = flip countervail [5]


-- | Mode shift the given scale into locrian. In 'diatonic', this flattens the
-- 2nd, 3rd, 5th, 6th and 7th.
locrian :: (7 <= s, KnownNat c) => MetaScale s -> T '[s, c]
locrian = flip countervail [6]

