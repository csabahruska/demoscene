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
import Utility

import LambdaCube.GL hiding (Exp, Var, Let, V3, V2)
import qualified LambdaCube.GL as LC

---------------------

defaultCam :: LC.M33F
defaultCam = LC.V3 (LC.V3 (scale * 0.75) 0 0) (LC.V3 0 scale 0) (LC.V3 ofsX ofsY 1)
  where
    scale = 0.1
    ofsX = -0.3
    ofsY = 0

greetingNames =
  [ ""
  , "s3c"
  , "archee"
  , "alvaro"
  , "shr"
  , "rrrola"
  , "a moiré misszió\n  bemutatja"
  ]

wires :: IO (Wire Int ExpV1)
wires = flip evalStateT 0 $ transWire $ WHorizontal ()
    [ WVertical ()
        [ (wire1D 200 $ mulSV3 (sin (3* time) + 1.1) . unKnot) {wDuration = Just 3}
        , wire1D 200 $ mulSV3 1.1 . unKnot
        ]
    , WVertical ()
      [ WText2D () (Just 2) defaultCam ""
      , WHorizontal ()
        [ WVertical () [ WText2D () (Just 0) defaultCam "", WText2D () (Just 6) defaultCam "lambda presents" ]
        , WVertical () [ WText2D () (Just 8) defaultCam "", WText2D () (Just 8) defaultCam "\nknot theory" ]
        ]
      , WHorizontal ()
        [ WVertical () [ WText2D () (Just 0) defaultCam "", WText2D () (Just 8) defaultCam "who do you think" ]
        , WVertical () [ WText2D () (Just 3) defaultCam "", WText2D () (Just 6) defaultCam "\nis going to win" ]
        , WVertical () [ WText2D () (Just 6) defaultCam "", WText2D () (Just 4) defaultCam "\n\ncapitalism" ]
        ]
      , WHorizontal ()
        [ WVertical () [ WText2D () (Just 0) defaultCam "", WText2D () (Just 8) defaultCam "what is keeping us" ]
        , WVertical () [ WText2D () (Just 3) defaultCam "", WText2D () (Just 6) defaultCam "\nfrom embracing" ]
        , WVertical () [ WText2D () (Just 6) defaultCam "", WText2D () (Just 4) defaultCam "\n\nour brighter future" ]
        ]
      , WHorizontal ()
        [ WVertical () [ WText2D () (Just 0) defaultCam "", WText2D () (Just 8) defaultCam "with humans gone" ]
        , WVertical () [ WText2D () (Just 3) defaultCam "", WText2D () (Just 6) defaultCam "\nwill there be hope" ]
        , WVertical () [ WText2D () (Just 6) defaultCam "", WText2D () (Just 4) defaultCam "\n\nfor machines" ]
        ]
      , WHorizontal ()
        [ WVertical () [ WText2D () (Just 0) defaultCam "", WText2D () (Just 8) defaultCam "greetings:" ]
        , WVertical () [ WText2D () (Just 1) defaultCam ("\n  " ++ name) | name <- greetingNames ]
        ]
      , WHorizontal ()
        [ WVertical () [ WText2D () (Just 0) defaultCam "", WText2D () (Just 4) defaultCam "music:" ]
        , WVertical () [ WText2D () (Just 1) defaultCam "", WText2D () (Just 3) defaultCam "\n  ficture" ]
        ]
      , WHorizontal ()
        [ WVertical () [ WText2D () (Just 0) defaultCam "", WText2D () (Just 4) defaultCam "code:" ]
        , WVertical () [ WText2D () (Just 1) defaultCam "", WText2D () (Just 3) defaultCam "\n  divip\n  hcs\n  hranolky" ]
        ]
      , WHorizontal ()
        [ WVertical () [ WText2D () (Just 0) defaultCam "", WText2D () (Just 8) defaultCam "with machines gone" ]
        , WVertical () [ WText2D () (Just 3) defaultCam "", WText2D () (Just 6) defaultCam "\nwill there be hope" ]
        , WVertical () [ WText2D () (Just 6) defaultCam "", WText2D () (Just 4) defaultCam "\n\nfor humans" ]
        ]
      , WFadeOut () (Just 5)
      ]
    , wire2DNorm False 60 16 $ tubularPatch (mulSV3 2 . unKnot) (mulSV3 (0.1 * (sin (4 * time) + 5)) . unKnot)
    , (wire2DNormAlpha True 2000 3 (tubularNeighbourhood (helix 2 0) . translateZ (0.2 * sin (6 * time)) . twistZ 1 . magnifyZ 50 . magnifyX 0.2 . translateY 0.65 . translateX (-0.5) . planeZX) (Just $ const $ V3 0.5 0.5 0.5) Nothing) {wSimpleColor = True}
--    wire2DNorm False 200 20 $ magnifyZ 3 . cylinderZ 0.3
--    wire2DNorm False 200 20 $ twistZ 1 . translateX 0.5 . magnifyZ 3 . cylinderZ 0.1
--    wire1D 100 $ translateZ (-1.5) . helix 0.3 0.5 . (10 *)
--    wire1D 1000 $ translateZ (-1.5) . tubularNeighbourhood (helix 0.3 0.5) . helix 0.1 (0.5/3) . (50*)
--    wire2DNorm False 100 10 $ translateZ (-1.5) . tubularNeighbourhood (helix 0.3 0.5) . cylinderZ 0.08 . (10*)
--    wire1D 10000 $ env . helix (0.1/3) (0.5/9) . (200 *)
--    wire2DNorm False 1000 10 $ env . cylinderZ 0.015 . (50*)
{-
    , WVertical ()
        [
        ---------
          WCamera (Just  9) $ CamCurve $ mulSV3 2 . unKnot
        , WCamera (Just  9) $ CamCurve $ (V3 0 0 2 +) . mulSV3 2 . unKnot
        , WCamera (Just  8) $ CamCurve $ magnify 0.5 . lissajousKnot (V3 3 2 5) (V3 0.7 0.1 0)
        , WCamera (Just  5) $ CamMat $ fromProjective (lookat (Vec3 4 3 3) (Vec3 0 0 0) (Vec3 0 1 0))
        , WCamera (Just 10) $ CamCurve $ torusKnot 7 3
        , WCamera (Just 10) $ CamMat $ fromProjective (lookat (Vec3 0 3 1) (Vec3 0 1 0) (Vec3 0 1 0))
        , WCamera (Just 10) $ CamMat $ fromProjective (lookat (Vec3 0 0 2) (Vec3 7 0 9) (Vec3 0 1 0))
        ---------
        , WCamera Nothing $ CamMat $ fromProjective (lookat (Vec3 5 1 2) (Vec3 3 1 0) (Vec3 0 1 0))
        ]
-}
    , WVertical ()

        [ setDuration 30 $ WHorizontal ()
            [ wParticle 40 40 1 id Nothing -- (magnify 100 . hopf . magnify (2*pi)) (Just $ const $ V3 1 1 1)
            ]
{-
        [ setDuration 30 $ WHorizontal ()
            [ wire1D 10000 $ env3 . helix 0.1 0.2 . (200 *)
            , wire2DNormAlpha True 1000 10 (env3 . magnifyZ 60 . rotateXY time . twistZ 1 . translateY (-0.5) . planeZY)
                                (Just $ \(V2 x y) -> V3 x 1 y) (Just $ \(V2 x y) -> y)
            ]
        , setDuration 10 $ WHorizontal ()
            [ wire1D 10000 $ env2 . helix 0.1 0.2 . (200 *)
            , wire2DNorm False 1000 10 $ env2 . cylinderZ 0.08 . (70*)
            ]
        , setDuration 10 $ WHorizontal ()
            [ wire2DNorm False 1000 10 $ env3 . cylinderZ 0.08 . (60*)
            , wire2DNorm True 1000 10 $ env3 . translateY (-0.5) . magnifyZ 60 . planeZY
            ]
        , setDuration 10 $ WHorizontal ()
            [ wire1D 10000 $ env . helix (0.1/3) (0.5/9) . (200 *)
            , wire2DNorm False 1000 10 $ env . cylinderZ 0.015 . (50*)
            ]
        , setDuration 10 $ WHorizontal ()
            [ wire1D 10000 $ env . helix (0.1/3) (0.5/9) . (200 *)
            , wire2DNorm False 1000 10 $ env . cylinderZ 0.015 . (50*)
            ]
-}
{-
        , setDuration 10 $ WHorizontal ()
            [ wire2DNorm False 200 20 $ magnifyZ 3 . cylinderZ 0.3
            , wire2DNorm False 200 20 $ twistZ 1 . translateX 0.5 . magnifyZ 3 . cylinderZ 0.1
            ]
        , setDuration 10 $ WHorizontal ()
            [ wire1D 100 $ translateZ (-1.5) . helix 0.3 0.5 . (10 *)
            ]

        , setDuration 10 $ WHorizontal ()
            [ wire1D 1000 $ translateZ (-1.5) . tubularNeighbourhood (helix 0.3 0.5) . helix 0.1 (0.5/3) . (50*)
            , wire2DNorm False 100 10 $ translateZ (-1.5) . tubularNeighbourhood (helix 0.3 0.5) . cylinderZ 0.08 . (10*)
            ]
-}
        ]
--    wire2DNorm False 1000 10 $ env3 . cylinderZ 0.08 . (60*)
--    wire2DNorm True 1000 10 $ env3 . translateY (-0.5) . magnifyZ 60 . planeZY
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

wiresTest :: IO (Wire Int ExpV1)
wiresTest = flip evalStateT 0 $ transWire $ setDuration 100 $ WHorizontal ()
  [ wire1D 200 $ mulSV3 (sin (3* time) + 1.1) . unKnot
  , wire2DNorm False 60 16 $ tubularPatch (mulSV3 2 . unKnot) (mulSV3 (0.1 * (sin (4 * time) + 5)) . unKnot)
  , (wire2DNormAlpha True 1000 5 (tubularNeighbourhood (helix 2 0) . translateZ (0.2 * sin (6 * time)) . twistZ 1 . magnifyZ 50 . magnifyX 0.2 . translateY 0.65 . translateX (-0.5) . planeZX) (Just $ const $ V3 0.5 0.5 0.5) Nothing) {wSimpleColor = True}
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
        , wVertex    :: V3 e -> V3 e
        , wNormal    :: Maybe (V3 e -> V3 e)
        , wColor     :: Maybe (V3 e -> V3 e)
        , wAlpha     :: Maybe (V3 e -> e)
        }
    | WParticle
        { wInfo :: i
        , wDuration  :: Maybe Float
        , wSimpleColor  :: Bool
        , wXResolution :: Int
        , wYResolution :: Int
        , wZResolution :: Int
        , wVertex    :: V3 e -> V3 e
        , wNormal    :: Maybe (V3 e -> V3 e)
        , wColor     :: Maybe (V3 e -> V3 e)
        , wAlpha     :: Maybe (V3 e -> e)
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
    | WCamera
        { wDuration  :: Maybe Float
        , wCamera :: Camera
        }
    | WText2D
        { wInfo :: i
        , wDuration  :: Maybe Float
        , wTextPosition :: LC.M33F
        , wText :: String
        }
    -- sprite
    -- color
    -- normal

data Camera
    = CamCurve Knot.Curve
    | CamMat Mat4


wire1D = Wire1D () Nothing

wire2DNorm :: Bool -> Int -> Int -> Patch -> Wire () Exp
wire2DNorm t i j v = Wire2D () Nothing t False i j (to2 v) (Just $ to2 $ normalPatch v) Nothing Nothing

wParticle i j k v c = WParticle () Nothing False i j k v Nothing c Nothing

wire2DNormAlpha :: Bool -> Int -> Int -> Patch -> Maybe (V2 Exp -> V3 Exp) -> Maybe (V2 Exp -> Exp) -> Wire () Exp
wire2DNormAlpha t i j v c a = Wire2D () Nothing t False i j (to2 v) (Just $ to2 $ normalPatch v) (to2 <$> c) (to2 <$> a)

to2 f (V3 x y z) = f (V2 x y)

transWire :: Wire () Exp -> StateT Int IO (Wire Int ExpV1)
transWire (Wire1D info d i f) = newid >>= \id -> Wire1D <$> pure id <*> pure d <*> pure i <*> lift (transFun "t" f)
transWire (Wire2D info d b sc i j v n c a) = newid >>= \id -> Wire2D <$> pure id <*> pure d <*> pure b <*> pure sc <*> pure i <*> pure j <*> lift (transFun3 "t" "s" "k" v) <*> traverse (lift . transFun3 "t" "s" "k") n <*> traverse (lift . transFun3 "t" "s" "k") c <*> (traverse) (lift . transFun3_ "t" "s" "k") a
transWire (WParticle info d sc i j k v n c a) = newid >>= \id -> WParticle <$> pure id <*> pure d <*> pure sc <*> pure i <*> pure j <*> pure k <*> lift (transFun3 "t" "s" "k" v) <*> traverse (lift . transFun3 "t" "s" "k") n <*> traverse (lift . transFun3 "t" "s" "k") c <*> (traverse) (lift . transFun3_ "t" "s" "k") a
transWire (WHorizontal info ws) = newid >>= \id -> WHorizontal <$> pure id <*> traverse transWire ws
transWire (WVertical info ws) = newid >>= \id -> WVertical <$> pure id <*> traverse transWire ws
transWire (WFadeOut info ws) = newid >>= \id -> WFadeOut <$> pure id <*> pure ws
transWire (WCamera dur ws) = WCamera <$> pure dur <*> pure ws
transWire (WText2D info dur ws txt) = newid >>= \id -> WText2D <$> pure id <*> pure dur <*> pure ws <*> pure txt

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

transFun3_ :: String -> String -> String -> (V3 Exp -> Exp) -> IO (V3 ExpV1 -> ExpV1)
transFun3_ s1 s2 s3 = fmap (fmap runIdentity) . transFun3 s1 s2 s3 . fmap Identity

transFun3 :: Traversable f => String -> String -> String -> (V3 Exp -> f Exp) -> IO (V3 ExpV1 -> f ExpV1)
transFun3 s1 s2 s3 f = fmap (\e (V3 t1 t2 t3) -> fmap ($ M.fromList [(s1,t1), (s2,t2), (s3, t3)]) e) . traverse transExp $ f $ V3 (Var s1) (Var s2) (Var s3)

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
