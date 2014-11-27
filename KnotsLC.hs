{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module KnotsLC where

import Control.Applicative hiding (Const)
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

import qualified Knot as K
import Knot hiding (V3, V2)
import qualified AD as K
import AD hiding (Exp, Var)

import LambdaCube.GL

--f :: (a -> (b, c, d)) -> (a -> b, a -> c, a -> d)
--f g = (fst3 . g, snd3 . g, thd3 . g)

type ExpV3 = (Exp V Float, Exp V Float, Exp V Float)

data Wire
    = Wire1D Int (Exp V Float -> ExpV3)
    | Wire2D Int Int (Exp V Float -> Exp V Float -> (ExpV3, Maybe ExpV3))
    -- sprite
    -- color
    -- normal


transExp :: K.Exp -> M.Map String (Exp V Float) -> Exp V Float
transExp x t = case x of
    C f -> Const f
    Add e f -> tr e @+ tr f
    Sub e f -> tr e @- tr f
    Mul e f -> tr e @* tr f
    Div e f -> tr e @/ tr f
    Abs e -> abs' $ tr e
    Signum e -> sign' $ tr e
    Sin e -> sin' $ tr e
    Cos e -> cos' $ tr e
    ASin e -> asin' $ tr e
    ACos e -> acos' $ tr e
    K.Exp e -> exp' $ tr e
    Log e -> log' $ tr e
    K.Var s -> fromMaybe (Uni (IFloat $ BS.pack s)) $ M.lookup s t
  where
    tr = flip transExp t

-- use in ghci like ':main -o circle.svg -w 600'
--main = mainWith (dia :: Diagram B R2)
{-
dia' = fromVertices points
  where
    points = map (toP . project') $ sampleCurve 20 $
-}
{-
time :: K.Exp
time = "time"
-}
instance Timed K.Exp where
    time = "time"


wires =
    [ wire1D 200 $ mulSV3 (sin (3* time) + 1.1) . unKnot
    , wire2D 50 50 $ tubularPatch (mulSV3 2 . unKnot) (mulSV3 (0.1 * (sin (4 * time) + 5)) . unKnot)
    , wire2DNorm 200 80 $ tubularPatch (torusKnot 1 5) (mulSV3 0.1 . unKnot)
    ]
  where
    wire1D i ff = Wire1D i $ \t -> let env = M.singleton "t" t in (fx env, fy env, fz env)
      where
        K.V3 fx fy fz = transExp <$> ff "t"
    wire2D i j ff = Wire2D i j $ \t s -> let env = M.fromList [("t",t),("s",s)] in ((fx env, fy env, fz env), Nothing)
      where
        K.V3 fx fy fz = transExp <$> ff (K.V2 "t" "s")

    wire2DNorm :: Int -> Int -> Patch -> Wire
    wire2DNorm i j ff = Wire2D i j $ \t s -> let env = M.fromList [("t",t),("s",s)] in ((fx env, fy env, fz env), Just (nx env, ny env, nz env))
      where
        K.V3 fx fy fz = transExp <$> ff (K.V2 "t" "s")
        K.V3 nx ny nz = transExp <$> (normalPatch ff) (K.V2 "t" "s")

testComplexity = normalPatch $ tubularPatch (torusKnot 1 5) (mulSV3 0.1 . unKnot)
testComplexity' = normalPatch $ tubularPatch (mulSV3 0.1 . unKnot) (mulSV3 0.1 . unKnot)

{-
dia = fromVertices points
  <> mconcat (map (mark red) tpoints)
  where
    points = map (toP . project') $ sampleCurve 140 $ torusKnot 1 7

    tpoints = map (project') $ samplePatch 30 10 $ tubularPatch (mulSV3 2 . unKnot) (mulSV3 0.6 . unKnot)

project' :: V3 Double -> R2
project' (V3 x y z) = r2 (x, y + z / 2)

toP v = origin # translate v

mark c p = circle 0.03 # fc c # lw none # translate p
-}
