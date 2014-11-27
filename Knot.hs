{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Knot where

import Data.Foldable
import Data.Traversable
import Control.Applicative
import Control.Monad

--import qualified LambdaCube.GL as LC
import Numeric.AD

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

------------------------------- Curves and patches
-- curve : [0,1] -> R3
-- patch : [0,1] x [0,1] -> R3

type Curve s = s -> V3 s
type Patch s = V2 s -> V3 s

type CurveS = forall s . Floating s => Curve s
type PatchS = forall s . Floating s => Patch s

-------- sampling

sampleCurve :: (Fractional s) => Int -> Curve s -> [V3 s]
sampleCurve n curve =
    [ curve (fromIntegral t / fromIntegral n)
    | t <- [0..n] ]

samplePatch :: (Fractional s) => Int -> Int -> Patch s -> [V3 s]
samplePatch n m patch =
    [ patch (V2 (fromIntegral t / fromIntegral n) (fromIntegral u / fromIntegral m))
    | t <- [0..n]
    , u <- [0..m] ]

-------- combinators

-- compute the normal vector bundle of a patch
normalPatch :: PatchS -> PatchS
normalPatch patch v = crossV3 du dt
  where
    V2 du dt = transpose32 $ jacobian patch v

diagonalCurve :: PatchS -> CurveS
diagonalCurve patch = patch . pure

{-
http://en.wikipedia.org/wiki/Tubular_neighborhood
http://en.wikipedia.org/wiki/Vector_bundle
http://en.wikipedia.org/wiki/Frenet%E2%80%93Serret_formulas
-}
tubularPatch :: CurveS -> CurveS -> PatchS
tubularPatch path mask (V2 t u) = d0 + mulMV3 frame v
  where
    dPath = diffF path
    ddPath = diffF dPath
    d0 = path t
    d1 = unitV3 $ dPath t
    d2 = unitV3 $ ddPath t
    i = d2
    j = crossV3 k i
    k = d1
    frame = V3 i j k
    v = mask u

------ predefined curves and patches

unKnot :: CurveS
unKnot t = V3 (cos w) (sin w) 0
  where
    w = 2 * pi * t

-- TODO: generalize to (Curve -> Curve)?
torusKnot :: Integer -> Integer -> CurveS
torusKnot p q t = V3 (r * cos p') (r * sin p') (-sin q')
  where
    w = 2 * pi * t
    p' = w * fromInteger p
    q' = w * fromInteger q
    r = cos q' + 2

torus :: PatchS
torus = tubularPatch (mulSV3 4 . unKnot) unKnot

