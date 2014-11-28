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

type ExpV3 = (Exp V Float, Exp V Float, Exp V Float)

data Wire
    = Wire1D Int (Exp V Float -> ExpV3)
    | Wire2D Int Int (Exp V Float -> Exp V Float -> (ExpV3, Maybe ExpV3))
    -- sprite
    -- color
    -- normal

type ST = IM.IntMap (Either (Exp_ Unique) (Exp V Float))
--type Cont_ = Cont (ST -> Exp V Float)

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
            Sub_ e f    -> (@-) e f
            Mul_ e f    -> (@*) e f
            Div_ e f    -> (@/) e f
            Abs_ e      -> abs' e
            Signum_ e   -> sign' e
            Sin_ e      -> sin' e
            Cos_ e      -> cos' e
            ASin_ e     -> asin' e
            ACos_ e     -> acos' e
            K.Exp_ e    -> exp' e
            Log_ e      -> log' e


instance Timed K.Exp where
    time = "time"

wires :: IO [Wire]
wires = execWriterT $ do
    wire1D 200 $ mulSV3 (sin (3* time) + 1.1) . unKnot
    wire2DNorm 50 50 $ tubularPatch (mulSV3 2 . unKnot) (mulSV3 (0.1 * (sin (4 * time) + 5)) . unKnot)
    wire2DNorm 200 80 $ tubularPatch (torusKnot 1 5) (mulSV3 0.1 . unKnot)
    wire2DNorm 50 50 $ magnifyZ 100 . projectionZ . magnifyZ 0.01 . invPolarXY . magnify (2 * pi) . translateX (-1) . planeZY
{-
    wire2DNorm 50 50 $ 
        magnify 100 . projectionZ . magnify 0.01 . invPolarXY . rotateYZ (- pi / 4) . magnify (8 * pi) . translateX (-2) . magnifyZ 0.1 .
        magnify 100 . projectionZ . magnify 0.01 . invPolarXY . magnify (2 * pi) . translateX (-1) . 
        planeZY
-}
  where
    wire1D :: Int -> Curve -> WriterT [Wire] IO ()
    wire1D i ff = do
        K.V3 fx fy fz <- lift $ traverse transExp $ ff "t"
        tell [Wire1D i $ \t -> let env = M.singleton "t" t in (fx env, fy env, fz env)]
{-
    wire2D i j ff = Wire2D i j $ \t s -> let env = M.fromList [("t",t),("s",s)] in ((fx env, fy env, fz env), Nothing)
      where
        K.V3 fx fy fz = transExp <$> ff (K.V2 "t" "s")
-}

    wire2DNorm :: Int -> Int -> Patch -> WriterT [Wire] IO ()
    wire2DNorm i j ff = do
        K.V3 fx fy fz <- lift $ traverse transExp $ ff (K.V2 "t" "s")
        K.V3 nx ny nz <- lift $ traverse transExp $ (normalPatch ff) (K.V2 "t" "s")
        tell [Wire2D i j $ \t s -> let env = M.fromList [("t",t),("s",s)] in ((fx env, fy env, fz env), Just (nx env, ny env, nz env))]

testComplexity = (normalPatch $ tubularPatch (torusKnot 1 5) (mulSV3 0.1 . unKnot)) $ K.V2 "x" "y"  :: K.V3 K.Exp
testComplexity' = (normalPatch $ tubularPatch (mulSV3 0.1 . unKnot) (mulSV3 0.1 . unKnot)) $ K.V2 "x" "y"  :: K.V3 K.Exp

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
