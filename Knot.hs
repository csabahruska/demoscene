{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Knot where

import Data.Foldable
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

------------------------------- timed computations

class Floating a => Timed a where
    time :: a

instance Timed a => Timed (AD s a) where
    time = AD time
instance Timed a => Timed (Forward a) where
    time = Forward.Lift time
instance (Timed a, Reifies s Tape) => Timed (Reverse s a) where
    time = Reverse.Lift time

------------------------------- Curves and patches

type Curve = forall s . Timed s => s -> V3 s         -- [0,1] -> R3
type Patch = forall s . Timed s => V2 s -> V3 s      -- [0,1] x [0,1] -> R3
type Frame = forall s . Timed s => s -> V3 (V3 s)    -- moving frame

-------- sampling

sampleCurve :: (Fractional s) => Int -> (s -> a) -> [a]
sampleCurve n curve =
    [ curve (fromIntegral t / fromIntegral n)
    | t <- [0..n] ]

samplePatch :: (Fractional s) => Int -> Int -> (V2 s -> a) -> [a]
samplePatch n m patch =
    [ patch (V2 (fromIntegral t / fromIntegral n) (fromIntegral u / fromIntegral m))
    | t <- [0..n]
    , u <- [0..m] ]

-------- combinators

diagonalCurve :: Patch -> Curve
diagonalCurve patch = patch . pure

-- compute the normal vector bundle of a curve (not normalized)
normalCurve :: Curve -> Curve
normalCurve c = diffF c

-- compute the Frenet frame of a curve
-- http://en.wikipedia.org/wiki/Frenet%E2%80%93Serret_formulas
frenetFrame :: Curve -> Frame
frenetFrame c = liftA2 cross (unitV3 . c'') (unitV3 . c')
  where
    c' = normalCurve c
    c'' = normalCurve c'
    cross i k = V3 i (crossV3 k i) k

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
    V2 du dt = transpose32 $ jacobian patch v


------ predefined curves and patches

unKnot :: Curve
unKnot t = V3 (cos w) (sin w) 0
  where
    w = 2 * pi * t

-- TODO: generalize to (Curve -> Curve)?
torusKnot :: Integer -> Integer -> Curve
torusKnot p q t = V3 (r * cos p') (r * sin p') (-sin q')
  where
    w = 2 * pi * t
    p' = w * fromInteger p
    q' = w * fromInteger q
    r = cos q' + 2

torus :: Patch
torus = tubularPatch (mulSV3 4 . unKnot) unKnot

