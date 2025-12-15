module Music.Rhythm where

import Data.Foldable
import Music.Primitives
import Music.Types


stretchTo :: Rational -> Music -> Music
stretchTo d t =
  case duration t of
    0 -> mempty
    dur -> stretch (d / dur) t


offset :: Rational -> Music -> Music
offset d m = rest d <> m <> rest (-d)


inv :: Music -> Music
inv t = let d = duration t in rest (- d) <> t <> rest (- d)


re :: Music -> Music
re t = t <> rest (- duration t)


co :: Music -> Music
co t = rest (- duration t) <> t


fork :: Music -> Music -> Music
fork a b = re a <> b


join :: Music -> Music -> Music
join a b = a <> co b


simul :: Foldable t => t Music -> Music
simul as =
  foldMap (co . re) as <>
    rest (maximum $ 0 : (fmap duration $ toList as))
