{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}

module Music.Types.Scale
  ( Scale(.., Scale)
  , Quantization(..)
  , idScale
  , invertScale
  , compScale
  , applyScale
  , mkScale
  , mkSomeScale
  , spelled
  ) where

import Data.SNat
import Data.Typeable
import Data.Constraint
import Data.Fin
import Data.Foldable
import Data.Kind
import Data.MVec
import Data.Map qualified as M
import Data.Ord (Down(..), comparing)
import Data.Set (Set)
import Data.Set qualified as S
import GHC.TypeLits (Nat, type (<=))
import Music.Types.Accidentals
import Music.Types.Deg
import Music.Types.Note


data Quantization
  = Below | Above | Nearest
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type Scale :: Nat -> Nat -> Type
data Scale a b where
  Embed :: (KnownNat a, KnownNat b, 1 <= a, 1 <= b) => MVec b a (Accidentals Int) -> Scale a b
  Project :: (KnownNat a, KnownNat b, 1 <= a, 1 <= b) => Quantization -> MVec a b (Accidentals Int) -> Scale a b
  Compose :: (KnownNat b, 1 <= b) => Scale b c -> Scale a b -> Scale a c

pattern Scale :: () => (KnownNat a, KnownNat b, 1 <= a, 1 <= b) => Scale a b
pattern Scale <- (scaleDict -> Dict)
{-# COMPLETE Scale #-}

deriving stock instance Show (Scale a b)

invertScale :: Quantization -> Scale a b -> Scale b a
invertScale q (Embed slots) = Project q slots
invertScale _ (Project _ slots) = Embed slots
invertScale q (Compose f g) = Compose (invertScale q g) (invertScale q f)

spelled :: forall a b. (KnownNat a, KnownNat b, 1 <= b) => Scale a b -> Fin a -> Note b
spelled (Embed slots) = \fa ->
  let (fb, accs) = mlookup slots fa
   in Note (fromIntegral $ toNatural fb) accs
spelled (Compose g f) = applyScale g . spelled f
spelled (Project q f) = inverting (choosing q) f


idScale :: (KnownNat a, 1 <= a) => Scale a a
idScale = Embed $ pure mempty

scaleDict :: Scale a b -> Dict (KnownNat a, KnownNat b, 1 <= a, 1 <= b)
scaleDict Embed{} = Dict
scaleDict Project{} = Dict
scaleDict (Compose sc1 sc2) =
  case scaleDict sc1 of
    Dict ->
      case scaleDict sc2 of
        Dict -> Dict


compScale :: Scale b c -> Scale a b -> Scale a c
compScale (Embed bc) (Embed ab) = Embed $ malign (<>) bc ab
compScale bc@Scale ab@Scale = Compose bc ab


{-# RULES
  "comp/invert" forall sc q.
    compScale (invertScale q sc) sc = invertScale q sc;
  "comp/invert2" forall sc q.
    compScale sc (invertScale q sc) = sc
  #-}


applyScale
    :: forall m n
     . Scale m n
    -> Note m
    -> Note n
applyScale sc@Scale (Note (Deg dm) o)
  = knownInduction @m
  $ let f = spelled sc
        (oct, pc) = divMod dm $ scaleSize @m
        (Note (Deg dn) o') = f $ toEnum pc
        (o'', od) = decompAccidental @n o
    in Note (od + Deg (dn + oct * scaleSize @n)) $ o' <> o''


mkScale :: forall c s. (KnownNat c, KnownNat s, 1 <= s) => Set (Note s) -> Scale c s
mkScale ss = mkSomeScale ss $ \(s@Scale :: Scale c' s) ->
  case eqT @c @c' of
    Just Refl -> s
    Nothing -> error "mkScale was given the wrong size!"


mkSomeScale :: forall s r. (KnownNat s, 1 <= s) => Set (Note s) -> (forall c. (c <= s) => Scale c s -> r) -> r
mkSomeScale ss k = knownInduction @s $ do
  let pcs = M.fromList $ do
              Note (Deg d) as <- S.toList $ S.map normalizeNote ss
              pure $ (fromIntegral @_ @(Fin s) d, as)
  case makeSomeMVec pcs of
    SomeMVec (mv :: MVec s c (Accidentals Int)) ->
      case mvecSizeC mv of
        Dict ->
          case toUNat $ SNat @c of
            UZero -> k @0 $ error "mkSomeScale was given an empty set"
            USucc _ -> k $ Embed mv



inverting
    :: forall a b
     . (KnownNat a, KnownNat b, 1 <= a)
    => (forall x. (Ord x, KnownNat b) => Fin b -> [(Fin b, x)] -> (Int, x))
       -- ^ Given the target pitch class and the (non-empty) image of the
       -- scale, choose a candidate along with an octave offset to apply to
       -- it. Parametricity in @x@ guarantees the result came from the image.
    -> MVec b a (Accidentals Int)
    -> Fin b -> Note a
inverting choose sc = knownInduction @a $ knownInduction @b $ do
  let img :: [(Fin b, Note a)]
      img = do
        a <- [0 .. maxBound @(Fin a)]
        let (b, accs) = mlookup sc a
        pure (b, Note (fromIntegral a) $ fmap negate accs)
  \b ->
    case lookup b img of
      Just a -> a
      Nothing -> do
        let (ox, chosen) = choose b img
            (b', Note da os) =
              case find ((== chosen) . snd) img of
                Just c -> c
                Nothing -> error "invertScale: impossible; chooser escaped the image"
            -- How far (in steps of @b@) the requested pitch class is from
            -- the chosen candidate, accounting for any octave wraparound.
            diff = fromIntegral b - (fromIntegral b' + ox * scaleSize @b)
        Note (da + Deg (ox * scaleSize @a)) $ os <> accidental @b diff


-- | Build a chooser that prefers candidates satisfying @acceptable@, picking
-- among them with @getBest@. If none are acceptable, wrap around by
-- @octOffset@ octaves and pick from the whole image.
directional
    :: Ord x
    => (Fin b -> Fin b -> Bool)
    -> (forall y. Ord y => [y] -> y)
    -> Int
    -> Fin b
    -> [(Fin b, x)]
    -> (Int, x)
directional acceptable getBest octOffset b img =
  case filter (flip acceptable b . fst) img of
    [] -> (octOffset, snd $ getBest img)
    cs -> (0, snd $ getBest cs)


choosing
  :: forall b
   . Quantization
  -> (forall x. (Ord x, KnownNat b) => Fin b -> [(Fin b, x)] -> (Int, x))
choosing Below = directional (>=) minimum 1
choosing Above = directional (<=) maximum (-1)
choosing Nearest = \b img -> knownInduction @b $ fmap snd $ do
  let key (ox, (cb, n)) =
        let d = fromIntegral b - (fromIntegral cb + ox * scaleSize @b) :: Int
         in (abs d, d < 0, Down n)
  minimumBy (comparing key) $ do
    c <- img
    ox <- [-1, 0, 1]
    pure (ox, c)

