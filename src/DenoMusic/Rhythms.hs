module DenoMusic.Rhythms where

import DenoMusic.Types
import DenoMusic.Utils

quarter :: Music () ()
quarter = note (1 / 4) ()

eighth :: Music () ()
eighth = note (1 / 8) ()

sixteenth :: Music () ()
sixteenth = note (1 / 16) ()

half :: Music () ()
half = note (1 / 2) ()

dotted :: Music v a -> Music v a
dotted = stretch 1.5

-- Waltz (3/4 time, emphasis on 1)
waltz :: Music () ()
waltz = quarter ## eighth ## eighth ## quarter

-- Variants
vienneseWaltz :: Music () ()  -- faster, lighter on 2 and 3
vienneseWaltz = quarter ## eighth ## eighth

-- Two-step (quick-quick-slow pattern)
twoStep :: Music () ()
twoStep = quarter ## quarter ## half

-- Boom-bap (hip-hop, kick on 1 and 3, snare on 2 and 4)
boomBap :: Music () ()
boomBap = quarter ## quarter ## quarter ## quarter  -- emphasize 1,3 vs 2,4 in realization

-- Son clave (3-2 pattern)
sonClave :: Music () ()
sonClave =
  eighth ## eighth ## rest (1 / 8) ## eighth ## rest (1 / 4) ##
  rest (1 / 8) ## eighth ## eighth ## rest (1 / 8)

-- Bossa nova
bossaNova :: Music () ()
bossaNova =
  dotted quarter ## eighth ## quarter ## quarter

-- Tango (habanera rhythm)
tango :: Music () ()
tango = dotted eighth ## sixteenth ## eighth ## quarter

-- Reggae (offbeat emphasis)
reggae :: Music () ()
reggae = rest (1 / 8) ## eighth ## rest (1 / 8) ## eighth ## rest (1 / 8) ## eighth ## rest (1 / 8) ## eighth

-- Samba
samba :: Music () ()
samba = line [dotted eighth, sixteenth, eighth, dotted eighth, sixteenth, eighth]

-- Shuffle (swung eighths)
shuffle :: Music () ()
shuffle = line [note (1 / 6) (), note (1 / 12) (), note (1 / 6) (), note (1 / 12) ()]  -- triplet feel

-- March (strong on 1 and 3)
march :: Music () ()
march = quarter ## eighth ## eighth ## quarter ## eighth ## eighth
