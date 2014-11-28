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

norm2V2 :: (Floating s) => V2 s -> s
norm2V2 = Prelude.sum . toList . join (*)

normV2 :: (Floating s) => V2 s -> s
normV2 = sqrt . norm2V2

norm2V3 :: (Floating s) => V3 s -> s
norm2V3 = Prelude.sum . toList . join (*)

normV3 :: (Floating s) => V3 s -> s
normV3 = sqrt . norm2V3

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

type SpaceTrS s = V3 s -> V3 s    -- moving frame
type SpaceTr = forall s . Timed s => V3 s -> V3 s    -- moving frame

translateX :: Floating t => t -> SpaceTrS t
translateX t (V3 x y z) = V3 (t + x) y z

translateY :: Floating t => t -> SpaceTrS t
translateY t (V3 x y z) = V3 x (t + y) z

translateZ :: Floating t => t -> SpaceTrS t
translateZ t (V3 x y z) = V3 x y (t + z)

magnifyX :: Floating t => t -> SpaceTrS t
magnifyX t (V3 x y z) = V3 (t * x) y z

magnifyY :: Floating t => t -> SpaceTrS t
magnifyY t (V3 x y z) = V3 x (t * y) z

magnifyZ :: Floating t => t -> SpaceTrS t
magnifyZ t (V3 x y z) = V3 x y (t * z)

magnify :: Floating t => t -> SpaceTrS t
magnify = mulSV3

rotateXY :: Floating s => s -> SpaceTrS s
rotateXY t (V3 x y z) = V3 (c * x - s * y) (s * x + c * y) z
  where
    (s, c) = sinCos t

rotateXZ :: Floating s => s -> SpaceTrS s
rotateXZ t (V3 x y z) = V3 (c * x - s * z) y (s * x + c * z)
  where
    (s, c) = sinCos t

rotateYZ :: Floating t => t -> SpaceTrS t
rotateYZ t (V3 x y z) = V3 x (c * y - s * z) (s * y + c * z)
  where
    (s, c) = sinCos $ t

projectionZ :: SpaceTr
projectionZ (V3 x y z) = V3 (z * x) (z * y) z

twistZ :: Floating t => t -> SpaceTrS t
twistZ t (V3 x y z) = V3 (c * x - s * y) (s * x + c * y) z
  where
    (s, c) = sinCos $ (2 * pi * t) * z

invPolarXY :: SpaceTr
invPolarXY (V3 x y z) = V3 (x * c) (x * s) z
  where
    (s, c) = sinCos ((2*pi) * y)

invPolarXY' :: SpaceTr
invPolarXY' (V3 x y z) = V3 (x * c) (x * s) z
  where
    (s, c) = sinCos y

tubularNeighbourhood :: Curve -> SpaceTr
tubularNeighbourhood c (V3 x y z) = c z + mulMV3 (frenetFrame' c z) (V3 x y 0)

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

-- normalized helix
helix :: Timed a => a -> a -> CurveS a
helix radius pitch = cu . (/ len)
  where
    cu t = invPolarXY' $ V3 radius t (b * t)

    b = pitch / (2*pi)
    len = sqrt (radius ^ 2 + b ^ 2)

-- turn of the helix
helixTurn radius pitch = 1 / sqrt (sqr (2*pi*radius) + sqr pitch)

sqr x = x * x

-- height (z-length) of the helix
helixHeight :: Floating a => a -> a -> a
helixHeight radius pitch = 1 / sqrt (sqr (2*pi*radius/pitch) + 1)

archimedeanSpiral :: Timed a => a -> a -> CurveS a
archimedeanSpiral radius phase = \t -> invPolarXY' $ V3 (radius * t) (t + phase) 0

archimedeanSpiralLength radius t = 0.5 * radius * (t * sqrt (1 + sqr t) + asinh t)

-- slightly normalized
archimedeanSpiralN :: Timed a => a -> a -> CurveS a
archimedeanSpiralN radius phase = archimedeanSpiral radius phase . (+(-1)) . sqrt . (+1) . (/radius)

logarithmicSpiral :: Timed a => a -> a -> CurveS a
logarithmicSpiral a b = \t -> invPolarXY' $ V3 (a * exp (b * t)) t 0

logarithmicSpiralLength :: Floating a => a -> a -> a -> a
logarithmicSpiralLength a b = \t -> a / b * sqrt (1 + sqr b) * exp (b * t)



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

frenetFrame' :: Curve -> Frame
frenetFrame' c = liftA2 cross (unitV3 . c'') c'
  where
    c' = normalCurve c
    c'' = normalCurve c'
    cross i k = V3 (mulSV3 (normV3 k) i) (crossV3 k i) k

------------------------------- Patches

type PatchS s = V2 s -> V3 s      -- [0,1] x [0,1] -> R3
type Patch = forall s . Timed s => V2 s -> V3 s      -- [0,1] x [0,1] -> R3

planeXY :: Patch
planeXY (V2 x y) = V3 x y 0

planeYZ :: Patch
planeYZ (V2 x y) = V3 0 x y

planeZY :: Patch
planeZY (V2 x y) = V3 0 y x

planeZX :: Patch
planeZX (V2 x y) = V3 y 0 x

cylinderZ :: Timed s => s -> PatchS s
cylinderZ dia = invPolarXY . translateX dia . planeZY

{-
http://en.wikipedia.org/wiki/Tubular_neighborhood
http://en.wikipedia.org/wiki/Vector_bundle
-}
tubularPatch :: Curve -> Curve -> Patch
tubularPatch path mask (V2 t u) = path t + mulMV3 (frenetFrame path t) (mask u)

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



