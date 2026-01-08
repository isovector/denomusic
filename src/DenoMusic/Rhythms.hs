module DenoMusic.Rhythms where

import DenoMusic.Types
import DenoMusic.Utils

quarter :: Music () ()
quarter = note (1 / 4) ()

eighth :: Music () ()
eighth = note (1 / 8) ()

sixteenth :: Music () ()
sixteenth = note (1 / 16) ()

whole :: Music () ()
whole = note 1 ()

half :: Music () ()
half = note (1 / 2) ()

third :: Music () ()
third = note (1 / 3) ()

sixth :: Music () ()
sixth = note (1 / 6) ()

dotted :: Music v a -> Music v a
dotted = stretch 1.5

waltz :: Music () ()
waltz = quarter ## eighth ## eighth ## quarter

vienneseWaltz :: Music () ()
vienneseWaltz = quarter ## eighth ## eighth

twoStep :: Music () ()
twoStep = quarter ## quarter ## half

sonClave :: Music () ()
sonClave =
  eighth
    ## eighth
    ## rest (1 / 8)
    ## eighth
    ## rest (1 / 4)
    ## rest (1 / 8)
    ## eighth
    ## eighth
    ## rest (1 / 8)

bossaNova :: Music () ()
bossaNova =
  dotted quarter ## eighth ## quarter ## quarter

tango :: Music () ()
tango = dotted quarter ## eighth ## quarter ## quarter

reggae :: Music () ()
reggae = rest (1 / 8) ## eighth ## rest (1 / 8) ## eighth ## rest (1 / 8) ## eighth ## rest (1 / 8) ## eighth

samba :: Music () ()
samba = line [dotted eighth, sixteenth, eighth, dotted eighth, sixteenth, eighth]

shuffle :: Music () ()
shuffle = line [note (1 / 6) (), note (1 / 12) (), note (1 / 6) (), note (1 / 12) ()]

march :: Music () ()
march = quarter ## eighth ## eighth ## quarter ## eighth ## eighth
