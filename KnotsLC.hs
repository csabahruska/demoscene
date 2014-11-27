{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
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

data Wire
    = Wire1D Int (Exp V Float -> Exp V Float, Exp V Float -> Exp V Float, Exp V Float -> Exp V Float)
            -- sprite
    | Wire2D Int Int (Exp V Float -> Exp V Float -> Exp V Float, Exp V Float -> Exp V Float -> Exp V Float, Exp V Float -> Exp V Float -> Exp V Float)
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
    [ Wire1D 200 (fx, fy, fz)
    , Wire2D 50 50 (fx', fy', fz')
    ]
  where
    K.V3 fx fy fz = ((. M.singleton "t") . transExp) <$> (\t -> mulSV3 1 $ mulSV3 (sin (3* time) + 1.1) $ unKnot t) "t"
    K.V3 fx' fy' fz' = ((\v t s -> v $ M.fromList [("t",t),("s",s)]) . transExp) <$> 
            ( tubularPatch (mulSV3 2 . unKnot) (mulSV3 (0.3 * (sin (4 * time) + 1.1)) . unKnot)
            $ K.V2 "t" "s")

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
