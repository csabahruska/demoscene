{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImpredicativeTypes #-}
module KnotsLC where

import Data.Maybe
import Data.Traversable
import Control.Applicative hiding (Const)
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Cont
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.ByteString.Char8 as BS

import Knot
import AD
import Data.Reify.Graph
import Data.Vect

import LambdaCube.GL hiding (Exp, Var, Let, V3, V2)
import qualified LambdaCube.GL as LC

---------------------

{-
wires :: IO (Wire Int ExpV1)
wires = flip evalStateT 0 $ transWire $ WHorizontal ()
    [ WVertical ()
        [ (wire1D 200 $ mulSV3 (sin (3* time) + 1.1) . unKnot) {wDuration = Just 3}
        , wire1D 200 $ mulSV3 1.1 . unKnot
        ]
    , wire2DNorm False 60 16 $ tubularPatch (mulSV3 2 . unKnot) (mulSV3 (0.1 * (sin (4 * time) + 5)) . unKnot)
    , (wire2DNormAlpha True 2000 5 (tubularNeighbourhood (helix 2 0) . translateZ (0.2 * sin (6 * time)) . twistZ 1 . magnifyZ 50 . magnifyX 0.2 . translateY 0.65 . translateX (-0.5) . planeZX) (Just $ const $ V3 0.5 0.5 0.5) Nothing) {wSimpleColor = True}
--    wire2DNorm False 200 20 $ magnifyZ 3 . cylinderZ 0.3
--    wire2DNorm False 200 20 $ twistZ 1 . translateX 0.5 . magnifyZ 3 . cylinderZ 0.1
--    wire1D 100 $ translateZ (-1.5) . helix 0.3 0.5 . (10 *)
--    wire1D 2000 $ translateZ (-1.5) . tubularNeighbourhood (helix 0.3 0.5) . helix 0.1 (0.5/3) . (50*)
--    wire2DNorm False 100 10 $ translateZ (-1.5) . tubularNeighbourhood (helix 0.3 0.5) . cylinderZ 0.08 . (10*)
--    wire1D 10000 $ env . helix (0.1/3) (0.5/9) . (200 *)
--    wire2DNorm False 2000 10 $ env . cylinderZ 0.015 . (50*)
    , WVertical ()
        [ setDuration 20 $ WHorizontal ()
            [ wire1D 10000 $ env3 . helix 0.1 0.2 . (200 *)
            , wire2DNormAlpha True 1000 10 (env3 . magnifyZ 60 . rotateXY time . twistZ 1 . translateY (-0.5) . planeZY)
                                (Just $ \(V2 x y) -> V3 x 1 y) (Just $ \(V2 x y) -> y)
            ]
        , setDuration 10 $ WHorizontal ()
            [ wire1D 10000 $ env2 . helix 0.1 0.2 . (200 *)
            , wire2DNorm False 2000 10 $ env2 . cylinderZ 0.08 . (70*)
            ]
        , setDuration 10 $ WHorizontal ()
            [ wire2DNorm False 2000 10 $ env3 . cylinderZ 0.08 . (60*)
            , wire2DNorm True 2000 10 $ env3 . translateY (-0.5) . magnifyZ 60 . planeZY
            ]
        , setDuration 10 $ WHorizontal ()
            [ wire1D 10000 $ env . helix (0.1/3) (0.5/9) . (200 *)
            , wire2DNorm False 2000 10 $ env . cylinderZ 0.015 . (50*)
            ]
        , setDuration 10 $ WHorizontal ()
            [ wire1D 10000 $ env . helix (0.1/3) (0.5/9) . (200 *)
            , wire2DNorm False 2000 10 $ env . cylinderZ 0.015 . (50*)
            ]
        , setDuration 10 $ WHorizontal ()
            [ wire2DNorm False 200 20 $ magnifyZ 3 . cylinderZ 0.3
            , wire2DNorm False 200 20 $ twistZ 1 . translateX 0.5 . magnifyZ 3 . cylinderZ 0.1
            ]
        , setDuration 10 $ WHorizontal ()
            [ wire1D 100 $ translateZ (-1.5) . helix 0.3 0.5 . (10 *)
            ]
        , setDuration 10 $ WHorizontal ()
            [ wire1D 2000 $ translateZ (-1.5) . tubularNeighbourhood (helix 0.3 0.5) . helix 0.1 (0.5/3) . (50*)
            , wire2DNorm False 100 10 $ translateZ (-1.5) . tubularNeighbourhood (helix 0.3 0.5) . cylinderZ 0.08 . (10*)
            ]
        , WFadeOut () (Just 3)
        ]
--    wire2DNorm False 2000 10 $ env3 . cylinderZ 0.08 . (60*)
--    wire2DNorm True 2000 10 $ env3 . translateY (-0.5) . magnifyZ 60 . planeZY
--    wire2DNorm True 2 2 $ planeXY
--    wire2DNorm False 200 20 $ twistZ 1 . translateX 0.5 . magnifyZ 3 . cylinderZ 0.1
--    wire2DNorm False 50 50 $ magnifyZ 100 . projectionZ . magnifyZ 0.01 . invPolarXY . magnify (2 * pi) . translateX (-1) . planeZY
--    wire2DNorm False 50 50 $
--        magnify 100 . projectionZ . magnify 0.01 . invPolarXY . rotateYZ (- pi / 4) . magnify (8 * pi) . translateX (-2) . magnifyZ 0.1 .
--        magnify 100 . projectionZ . magnify 0.01 . invPolarXY . magnify (2 * pi) . translateX (-1) .
--        planeZY
    , wire2DNormAlpha False 500 10 (tubularPatch (mulSV3 3 . lissajousKnot (V3 3 4 7) (V3 0.1 0.7 0.0)) (mulSV3 0.1 . unKnot))
                (Just $ const $ V3 1 1 1) (Just $ const $ 0.5)
--    wire2DNormAlpha True 20 20 (magnify 3 . translateY (-0.5) . planeYZ) (Just $ sin . normV2)
    ]
  where
    env = magnify 2 . tubularNeighbourhood (helix 0.9 (sin time + 1.5)) . tubularNeighbourhood (helix 0.3 0.5 . (+ 0.5 * sin (2 * time))) . tubularNeighbourhood (helix 0.1 (0.5/3) . (+ 0.03 * sin (10 * time)))
    env2 = magnify 1.5 . tubularNeighbourhood (liftA2 (+) id ((\t -> V3 0 0 t) . (/15) . sin . (*6) . (+ (0.5 * time)) . normV3) . archimedeanSpiralN 0.02 0)
    env3 = magnify 1.5 . tubularNeighbourhood (liftA2 (+) id ((\t -> V3 0 0 t) . (/15) . sin . (*6) . (+ (0.5 * time)) . normV3) . logarithmicSpiral 0.1 0.04)
-}

instance Knot.Timed Float where
  time = error "Can't get the time in Float land"

curveToCameraPath :: (Floating s, Timed s) => Curve -> s -> (V3 s, V3 (V3 s))
curveToCameraPath curve t = (curve t, frenetFrame curve t)

cameraToMat4 :: (V3 Float, V3 (V3 Float)) -> Mat4
cameraToMat4 (origin, V3 columnX columnY columnZ) =
  let
    V3 ox oy oz = origin
    V3 xx xy xz = columnX
    V3 yx yy yz = columnY
    V3 zx zy zz = -columnZ
    rx = Vec4 xx yx zx 0
    ry = Vec4 xy yy zy 0
    rz = Vec4 xz yz zz 0
    rw = Vec4  0  0  0 1
    tx = Vec4 1 0 0 0
    ty = Vec4 0 1 0 0
    tz = Vec4 0 0 1 0
    tw = Vec4 (-ox) (-oy) (-oz) 1
  in Mat4 tx ty tz tw .*. Mat4 rx ry rz rw

---------------------

wires :: IO (Wire Int ExpV1)
wires = flip evalStateT 0 $ transWire $ setDuration 100 $ WHorizontal ()
  [ wire1D 200 $ mulSV3 (sin (3* time) + 1.1) . unKnot
  , wire2DNorm False 60 16 $ tubularPatch (mulSV3 2 . unKnot) (mulSV3 (0.1 * (sin (4 * time) + 5)) . unKnot)
  , (wire2DNormAlpha True 2000 5 (tubularNeighbourhood (helix 2 0) . translateZ (0.2 * sin (6 * time)) . twistZ 1 . magnifyZ 50 . magnifyX 0.2 . translateY 0.65 . translateX (-0.5) . planeZX) (Just $ const $ V3 0.5 0.5 0.5) Nothing) {wSimpleColor = True}
  ]

tt = 300

setDuration d (WHorizontal i ws) = WHorizontal i $ [w { wDuration = Just d } | w <- ws]

---------------------

type ExpV1 = LC.Exp V Float
type ExpV3 = V3 ExpV1

data Wire i e
    = Wire1D
        { wInfo :: i
        , wDuration  :: Maybe Float
        , wXResolution :: Int
        , wVertex1   :: e -> V3 e
        }
    | Wire2D
        { wInfo :: i
        , wDuration  :: Maybe Float
        , wTwosided  :: Bool
        , wSimpleColor  :: Bool
        , wXResolution :: Int
        , wYResolution :: Int
        , wVertex    :: V2 e -> V3 e
        , wNormal    :: Maybe (V2 e -> V3 e)
        , wColor     :: Maybe (V2 e -> V3 e)
        , wAlpha     :: Maybe (V2 e -> e)
        }
    | WHorizontal
        { wInfo :: i
        , wWires :: [Wire i e]
        }
    | WVertical
        { wInfo :: i
        , wWires :: [Wire i e]
        }
    | WFadeOut
        { wInfo :: i
        , wDuration  :: Maybe Float
        }
    -- sprite
    -- color
    -- normal

wire1D = Wire1D () Nothing

wire2DNorm :: Bool -> Int -> Int -> Patch -> Wire () Exp
wire2DNorm t i j v = Wire2D () Nothing t False i j v (Just $ normalPatch v) Nothing Nothing

wire2DNormAlpha :: Bool -> Int -> Int -> Patch -> Maybe (V2 Exp -> V3 Exp) -> Maybe (V2 Exp -> Exp) -> Wire () Exp
wire2DNormAlpha t i j v c a = Wire2D () Nothing t False i j v (Just $ normalPatch v) c a

transWire :: Wire () Exp -> StateT Int IO (Wire Int ExpV1)
transWire (Wire1D info d i f) = newid >>= \id -> Wire1D <$> pure id <*> pure d <*> pure i <*> lift (transFun "t" f)
transWire (Wire2D info d b sc i j v n c a) = newid >>= \id -> Wire2D <$> pure id <*> pure d <*> pure b <*> pure sc <*> pure i <*> pure j <*> lift (transFun2 "t" "s" v) <*> traverse (lift . transFun2 "t" "s") n <*> traverse (lift . transFun2 "t" "s") c <*> (traverse) (lift . transFun2_ "t" "s") a
transWire (WHorizontal info ws) = newid >>= \id -> WHorizontal <$> pure id <*> traverse transWire ws
transWire (WVertical info ws) = newid >>= \id -> WVertical <$> pure id <*> traverse transWire ws
transWire (WFadeOut info ws) = newid >>= \id -> WFadeOut <$> pure id <*> pure ws

newid = do
    st <- get
    put $ st + 1
    return st

transFun :: Traversable f => String -> (Exp -> f Exp) -> IO (ExpV1 -> f ExpV1)
transFun s f = fmap (\e t -> fmap ($ M.singleton s t) e) . traverse transExp $ f $ Var s

transFun2_ :: String -> String -> (V2 Exp -> Exp) -> IO (V2 ExpV1 -> ExpV1)
transFun2_ s1 s2 = fmap (fmap runIdentity) . transFun2 s1 s2 . fmap Identity

transFun2 :: Traversable f => String -> String -> (V2 Exp -> f Exp) -> IO (V2 ExpV1 -> f ExpV1)
transFun2 s1 s2 f = fmap (\e (V2 t1 t2) -> fmap ($ M.fromList [(s1,t1), (s2,t2)]) e) . traverse transExp $ f $ V2 (Var s1) (Var s2)

type ST = IM.IntMap (Either (Exp_ Unique) (LC.Exp V Float))

transExp :: Exp -> IO (M.Map String ExpV1 -> ExpV1)
transExp x = (\(Graph rx x) env -> transExp_ x env (IM.map Left $ IM.fromList rx) const) <$> reify x
   where
    transExp_
        :: Unique
        -> M.Map String ExpV1
        -> ST
        -> (ExpV1 -> ST -> ExpV1)
        -> ExpV1

    transExp_ x env st cont_ = case st IM.! x of
        Right ex -> cont_ ex st
        Left e -> flip (runCont $ traverse (\i -> cont $ \co st -> transExp_ i env st co) e) st $ \xx st ->
          flip LC.Let (\rr -> cont_ rr $ IM.insert x (Right rr) st) $ case xx of
            C_ f        -> Const f
            Var_ s    -> fromMaybe (Uni (IFloat $ BS.pack s)) $ M.lookup s env
            Add_ e f    -> (@+) e f
            Neg_ e      -> neg' e
            Mul_ e f    -> (@*) e f
            Recip_ e    -> Const 1 @/ e
            Abs_ e      -> abs' e
            Signum_ e   -> sign' e
            Sin_ e      -> sin' e
            Cos_ e      -> cos' e
            ASin_ e     -> asin' e
            ACos_ e     -> acos' e
            ATan_ e     -> atan' e
            Sinh_ e     -> sinh' e
            Cosh_ e     -> cosh' e
            ASinh_ e    -> asinh' e
            ACosh_ e    -> acosh' e
            ATanh_ e    -> atanh' e
            Exp_ e    -> exp' e
            Log_ e      -> log' e


instance Timed Exp where
    time = "time"

testComplexity = (normalPatch $ tubularPatch (torusKnot 1 5) (mulSV3 0.1 . unKnot)) $ V2 "x" "y"  :: V3 Exp
testComplexity' = (normalPatch $ tubularPatch (mulSV3 0.1 . unKnot) (mulSV3 0.1 . unKnot)) $ V2 "x" "y"  :: V3 Exp
