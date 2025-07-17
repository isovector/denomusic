module Legacy where


import Control.Monad
import Control.Applicative
import Euterpea
import Euterpea.IO.MIDI.Play


maj :: PitchClass -> Octave -> [Pitch]
maj = curry $ \r ->
  [ r
  , trans 4 r
  , trans 7 r
  ]


minor :: PitchClass -> Octave -> [Pitch]
minor = curry $ \r ->
  [ r
  , trans 3 r
  , trans 7 r
  ]


maj7 :: PitchClass -> Octave -> [Pitch]
maj7 = curry $ \r ->
  [ r
  , trans 4 r
  , trans 7 r
  , trans 11 r
  ]


min7 :: PitchClass -> Octave -> [Pitch]
min7 = curry $ \r ->
  [ r
  , trans 3 r
  , trans 7 r
  , trans 10 r
  ]


dom7 :: PitchClass -> Octave -> [Pitch]
dom7 = curry $ \r ->
  [ r
  , trans 4 r
  , trans 7 r
  , trans 10 r
  ]


musicChord :: Dur -> [Pitch] -> Music Pitch
musicChord d = chord . fmap (note d)


addSemitonesToPitchClass :: PitchClass -> Int -> PitchClass
addSemitonesToPitchClass pc st = fst $ trans st (pc, 4)


progression :: PitchClass -> [[Pitch]]
progression key =
  [ min7 key  4
  , dom7 (addSemitonesToPitchClass key 2) 4
  , maj7 (addSemitonesToPitchClass key 5) 4
  , maj7 (addSemitonesToPitchClass key 10) 4
  ]


leadingProgression :: [[Pitch]] -> [([Pitch], Pitch)]
leadingProgression p = zipWith (\a b -> (a, head b)) p $ tail $ cycle p


data Hand
    = LeftHand Int
    | RightHand Int
    | GraspingHand Int
  deriving (Eq, Ord, Show)


bind :: (Dur -> a -> Music b) -> Music a -> Music b
bind f (Prim (Note dur a)) = f dur a
bind f (Prim (Rest dur))   = Prim $ Rest dur
bind f (ma :+: mb)         = bind f ma :+: bind f mb
bind f (ma :=: mb)         = bind f ma :=: bind f mb
bind f (Modify control ma) = Modify control $ bind f ma


duration :: Music a -> Dur
duration (Prim (Note dur _)) = dur
duration (Prim (Rest dur))   = dur
duration (a :+: b)           = duration a + duration b
duration (a :=: b)           = max (duration a) (duration b)
duration (Modify _ a)        = duration a


myBar :: [Pitch] -> Music Hand -> Music Pitch
myBar chord@((basspitch, bassoctave):remainder) paradiddle = bind f paradiddle
  where
    f dur (LeftHand i)  = note dur (basspitch, bassoctave - (i `mod` 2 + 1))
    f dur (RightHand i) = (!! i) $ iterate invert $  musicChord dur remainder
    f dur (GraspingHand i)  = note dur (fst $ chord !! i, bassoctave + 2)


bassJob :: Music Hand
bassJob = line
  [ note qn $ LeftHand 0
  , note qn $ LeftHand 1
  , chord
    [ line
        [ note qn $ LeftHand 2
        , note qn $ LeftHand 3
        ]
    , line
        [ rest en
        , note en $ GraspingHand 0
        , note en $ GraspingHand 1
        , note en $ GraspingHand 2
        ]
    ]
  ]


maybeAToPrim :: Dur -> Maybe a -> Music a
maybeAToPrim dur Nothing  = rest dur
maybeAToPrim dur (Just a) = note dur a


makeADiddleDoo :: Dur -> [Music Hand]
makeADiddleDoo totalDur
  | totalDur < 1/16 = pure $ rest totalDur
  | otherwise = do
      dur  <- [ totalDur, totalDur / 2 ]
      hand <- [ Just $ RightHand 0
              , Nothing
              ]
      following <- makeADiddleDoo $ totalDur - dur
      pure $ removeZeros $
        line
          [ maybeAToPrim dur hand
          , following
          ]


type Key = (PitchClass, Mode)


-- makeALeadingNote :: Key -> Pitch -> [Pitch]
-- makeALeadingNote k ch = do


theFunk :: Music Pitch
theFunk = line
        . zipWith myBar (cycle $ progression A)
        . fmap (:=: bassJob)
        -- . drop 14
        $ makeADiddleDoo $ 1/2

main = playDev 2 theFunk
