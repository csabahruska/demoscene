{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Knot where

import Data.Foldable
import Data.Ratio
import Data.Traversable
import Control.Applicative
import Control.Monad

--import qualified LambdaCube.GL as LC
import Data.Reflection
import Numeric.AD
import Numeric.AD.Internal.Reverse as Reverse (Reverse(Lift), Tape)
import Numeric.AD.Internal.Forward as Forward (Forward(Lift))
import Numeric.AD.Internal.Type (AD(AD))

--------------------- Linear algebra for 2 and 3 dimensional vectors and matrices

data V2 s = V2 s s
    deriving (Show, Functor, Foldable, Traversable)

instance Applicative V2 where
    pure x = V2 x x
    V2 f0 f1 <*> V2 x0 x1 = V2 (f0 x0) (f1 x1)

instance (Num s) => Num (V2 s) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

data V3 s = V3 s s s
    deriving (Show, Functor, Foldable, Traversable)

instance Applicative V3 where
    pure x = V3 x x x
    V3 f0 f1 f2 <*> V3 x0 x1 x2 = V3 (f0 x0) (f1 x1) (f2 x2)

instance (Num s) => Num (V3 s) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

mulSV3 :: (Num s) => s -> V3 s -> V3 s
mulSV3 = fmap . (*)

mulMV3 :: (Num s) => V3 (V3 s) -> V3 s -> V3 s
mulMV3 m v = Prelude.sum $ toList $ liftA2 mulSV3 v m

normV3 :: (Floating s) => V3 s -> s
normV3 = sqrt . Prelude.sum . toList . join (*)

unitV3 :: (Floating s) => V3 s -> V3 s
unitV3 v = mulSV3 (1 / normV3 v) v

crossV3 :: (Num s) => V3 s -> V3 s -> V3 s
crossV3 (V3 x0 y0 z0) (V3 x1 y1 z1) = V3 x y z
  where
    x = y0 * z1 - z0 * y1
    y = z0 * x1 - x0 * z1
    z = x0 * y1 - y0 * x1

transpose32 :: V3 (V2 s) -> V2 (V3 s)
transpose32 (V3 (V2 x0 y0) (V2 x1 y1) (V2 x2 y2)) = V2 (V3 x0 x1 x2) (V3 y0 y1 y2)

sinCos phi = (sin phi, cos phi)

------------------------------- timed computations

class Floating a => Timed a where
    time :: a

instance Timed a => Timed (AD s a) where
    time = AD time
instance Timed a => Timed (Forward a) where
    time = Forward.Lift time
instance (Timed a, Reifies s Tape) => Timed (Reverse s a) where
    time = Reverse.Lift time

------------------------------- Space transformations

type SpaceTr = forall s . Timed s => V3 s -> V3 s    -- moving frame

translateX :: RealFloat t => t -> SpaceTr
translateX t (V3 x y z) = V3 (realToFrac t + x) y z

translateY :: Float -> SpaceTr
translateY t (V3 x y z) = V3 x (realToFrac t + y) z

translateZ :: Float -> SpaceTr
translateZ t (V3 x y z) = V3 x y (realToFrac t + z)

magnifyX :: Float -> SpaceTr
magnifyX t (V3 x y z) = V3 (realToFrac t * x) y z

magnifyY :: Float -> SpaceTr
magnifyY t (V3 x y z) = V3 x (realToFrac t * y) z

magnifyZ :: Float -> SpaceTr
magnifyZ t (V3 x y z) = V3 x y (realToFrac t * z)

magnify :: Float -> SpaceTr
magnify = mulSV3 . realToFrac

rotateYZ :: Float -> SpaceTr
rotateYZ t (V3 x y z) = V3 x (c * y - s * z) (s * y + c * z)
  where
    (s, c) = sinCos $ realToFrac t

projectionZ :: SpaceTr
projectionZ (V3 x y z) = V3 (z * x) (z * y) z

twistZ :: Float -> SpaceTr
twistZ t (V3 x y z) = V3 (c * x - s * y) (s * x + c * y) z
  where
    (s, c) = sinCos $ (2 * pi * realToFrac t) * z

invPolarXY :: SpaceTr
invPolarXY (V3 x y z) = V3 (x * c) (x * s) z
  where
    (s, c) = sinCos ((2*pi) * y)

tubularNeighbourhood :: Curve -> SpaceTr
tubularNeighbourhood c = \(V3 x y z) -> c z + mulMV3 (frenetFrame c z) (V3 x y 0)

------------------------------- Curves

type CurveS s = s -> V3 s
type Curve = forall s . Timed s => s -> V3 s         -- [0,1] -> R3

lineX :: Curve
lineX x = V3 x 0 0

lineY :: Curve
lineY x = V3 0 x 0

lineZ :: Curve
lineZ x = V3 0 0 x

unKnot :: Curve
unKnot t = V3 (cos w) (sin w) 0
  where
    w = 2 * pi * t

helix :: Timed a => a -> a -> CurveS a
helix rad pitch = cu . (/ len)
  where
    cu t = V3 (rad * c) (rad * s) (b * t)
      where
        (s, c) = sinCos t
    b = pitch / (2*pi)

    len = sqrt (rad ^ 2 + b ^ 2)


-- TODO: generalize to (Curve -> Curve)?
torusKnot :: Integer -> Integer -> Curve
torusKnot p q t = V3 (r * cos p') (r * sin p') (-sin q')
  where
    w = 2 * pi * t
    p' = w * fromInteger p
    q' = w * fromInteger q
    r = cos q' + 2

lissajousKnot :: V3 Integer -> V3 Rational -> Curve
lissajousKnot (V3 nx ny nz) (V3 px py pz) t = V3 x y z
  where
    x = cos $ 2 * pi * (fromInteger nx * t + fromRational px)
    y = cos $ 2 * pi * (fromInteger ny * t + fromRational py)
    z = cos $ 2 * pi * (fromInteger nz * t + fromRational pz)

diagonalCurve :: Patch -> Curve
diagonalCurve patch = patch . pure

-- compute the normal vector bundle of a curve (not normalized)
normalCurve :: Curve -> Curve
normalCurve c = diffF c

------------------------------- Frames

type Frame = forall s . Timed s => s -> V3 (V3 s)    -- moving frame

-- compute the Frenet frame of a curve
-- http://en.wikipedia.org/wiki/Frenet%E2%80%93Serret_formulas
frenetFrame :: Curve -> Frame
frenetFrame c = liftA2 cross (unitV3 . c'') (unitV3 . c')
  where
    c' = normalCurve c
    c'' = normalCurve c'
    cross i k = V3 i (crossV3 k i) k

------------------------------- Patches

type Patch = forall s . Timed s => V2 s -> V3 s      -- [0,1] x [0,1] -> R3

planeXY :: Patch
planeXY (V2 x y) = V3 x y 0

planeYZ :: Patch
planeYZ (V2 x y) = V3 0 x y

planeZY :: Patch
planeZY (V2 x y) = V3 0 y x

planeZX :: Patch
planeZX (V2 x y) = V3 y 0 x

cylinderZ :: Float -> Patch
cylinderZ dia = invPolarXY . translateX dia . planeZY

{-
http://en.wikipedia.org/wiki/Tubular_neighborhood
http://en.wikipedia.org/wiki/Vector_bundle
-}
tubularPatch :: Curve -> Curve -> Patch
tubularPatch path = \mask (V2 t u) -> path t + mulMV3 (frame t) (mask u)
  where
    frame = frenetFrame path

-- compute the normal vector bundle of a patch
normalPatch :: Patch -> Patch
normalPatch patch v = crossV3 du dt
  where
    V2 du dt = transpose32 . jacobian patch $ v

torus :: Patch
torus = tubularPatch (mulSV3 4 . unKnot) unKnot

----------------------------- sampling

sampleCurve :: (Fractional s) => Int -> (s -> a) -> [a]
sampleCurve n curve =
    [ curve (fromIntegral t / fromIntegral n)
    | t <- [0..n] ]

samplePatch :: (Fractional s) => Int -> Int -> (V2 s -> a) -> [a]
samplePatch n m patch =
    [ patch (V2 (fromIntegral t / fromIntegral n) (fromIntegral u / fromIntegral m))
    | t <- [0..n]
    , u <- [0..m] ]



