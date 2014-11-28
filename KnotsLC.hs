{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
module KnotsLC where

import Data.Maybe
import Data.Traversable
import Control.Applicative hiding (Const)
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Cont
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.ByteString.Char8 as BS

import qualified Knot as K
import Knot hiding (V3, V2)
import qualified AD as K
import AD hiding (Exp, Var, Let)
import Data.Reify.Graph

import LambdaCube.GL

---------------------

wires :: IO [Wire]
wires = execWriterT $ do
    wire1D 200 $ mulSV3 (sin (3* time) + 1.1) . unKnot
    wire2DNorm 50 20 $ tubularPatch (mulSV3 2 . unKnot) (mulSV3 (0.1 * (sin (4 * time) + 5)) . unKnot)
    wire2DNorm 200 20 $ tubularPatch (torusKnot 1 5) (mulSV3 0.1 . unKnot)
--    wire2DNorm 200 20 $ magnifyZ 3 . cylinderZ 0.3
--    wire2DNorm 200 20 $ twistZ 1 . translateX 0.5 . magnifyZ 3 . cylinderZ 0.1
--    wire1D 100 $ translateZ (-1.5) . helix 0.3 0.5 . (10 *)
--    wire1D 2000 $ translateZ (-1.5) . tubularNeighbourhood (helix 0.3 0.5) . helix 0.1 (0.5/3) . (50*)
--    wire2DNorm 100 10 $ translateZ (-1.5) . tubularNeighbourhood (helix 0.3 0.5) . cylinderZ 0.08 . (10*)
    wire1D 10000 $ env . helix (0.1/3) (0.5/9) . (200 *)
    wire2DNorm 2000 10 $ env . cylinderZ 0.015 . (50*)
--    wire2DNorm 200 20 $ twistZ 1 . translateX 0.5 . magnifyZ 3 . cylinderZ 0.1
{-
    wire2DNorm 50 50 $ magnifyZ 100 . projectionZ . magnifyZ 0.01 . invPolarXY . magnify (2 * pi) . translateX (-1) . planeZY
    wire2DNorm 50 50 $
        magnify 100 . projectionZ . magnify 0.01 . invPolarXY . rotateYZ (- pi / 4) . magnify (8 * pi) . translateX (-2) . magnifyZ 0.1 .
        magnify 100 . projectionZ . magnify 0.01 . invPolarXY . magnify (2 * pi) . translateX (-1) .
        planeZY
-}
    wire2DNorm 500 10 $ tubularPatch (mulSV3 3 . lissajousKnot (K.V3 3 4 7) (K.V3 0.1 0.7 0.0)) (mulSV3 0.1 . unKnot)
  where
    env = magnify 2 . tubularNeighbourhood (helix 0.9 (sin time + 1.5)) . tubularNeighbourhood (helix 0.3 0.5 . (+ 0.5 * sin (2 * time))) . tubularNeighbourhood (helix 0.1 (0.5/3) . (+ 0.03 * sin (10 * time)))

tt = 300

---------------------

data Wire
    = Wire1D Int (Exp V Float -> ExpV3)
    | Wire2D Int Int (Exp V Float -> Exp V Float -> (ExpV3, Maybe ExpV3))
    -- sprite
    -- color
    -- normal

type ExpV3 = (Exp V Float, Exp V Float, Exp V Float)

wire1D :: Int -> Curve -> WriterT [Wire] IO ()
wire1D i ff = do
    K.V3 fx fy fz <- lift $ traverse transExp $ ff "t"
    tell [Wire1D i $ \t -> let env = M.singleton "t" t in (fx env, fy env, fz env)]

wire2DNorm :: Int -> Int -> Patch -> WriterT [Wire] IO ()
wire2DNorm i j ff = do
    K.V3 fx fy fz <- lift $ traverse transExp $ ff (K.V2 "t" "s")
    K.V3 nx ny nz <- lift $ traverse transExp $ (normalPatch ff) (K.V2 "t" "s")
    tell [Wire2D i j $ \t s -> let env = M.fromList [("t",t),("s",s)] in ((fx env, fy env, fz env), Just (nx env, ny env, nz env))]


type ST = IM.IntMap (Either (Exp_ Unique) (Exp V Float))

transExp :: K.Exp -> IO (M.Map String (Exp V Float) -> Exp V Float)
transExp x = (\(Graph rx x) env -> transExp_ x env (IM.map Left $ IM.fromList rx) const) <$> reify x
   where
    transExp_
        :: Unique
        -> M.Map String (Exp V Float)
        -> ST
        -> (Exp V Float -> ST -> Exp V Float)
        -> Exp V Float

    transExp_ x env st cont_ = case st IM.! x of
        Right ex -> cont_ ex st
        Left e -> flip (runCont $ traverse (\i -> cont $ \co st -> transExp_ i env st co) e) st $ \xx st ->
          flip Let (\rr -> cont_ rr $ IM.insert x (Right rr) st) $ case xx of
            C_ f        -> Const f
            K.Var_ s    -> fromMaybe (Uni (IFloat $ BS.pack s)) $ M.lookup s env
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
            K.Exp_ e    -> exp' e
            Log_ e      -> log' e


instance Timed K.Exp where
    time = "time"

testComplexity = (normalPatch $ tubularPatch (torusKnot 1 5) (mulSV3 0.1 . unKnot)) $ K.V2 "x" "y"  :: K.V3 K.Exp
testComplexity' = (normalPatch $ tubularPatch (mulSV3 0.1 . unKnot) (mulSV3 0.1 . unKnot)) $ K.V2 "x" "y"  :: K.V3 K.Exp

