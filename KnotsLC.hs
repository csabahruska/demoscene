{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module KnotsLC where

import Data.Maybe
import Data.Traversable
import Data.Foldable (foldMap)
import Data.Functor.Product
import Data.Functor.Constant
import Control.Applicative hiding (Const)
import Control.Monad.Writer hiding (Product)
import Control.Monad.State
--import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Cont
--import Control.Arrow
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.ByteString.Char8 as BS

import Knot
import AD
import Data.Reify.Graph
import Data.Vect
--import Utility

import LambdaCube.GL hiding (Exp, Var, Let, V3, V2)
import qualified LambdaCube.GL as LC

---------------------

program :: Wire_ (Maybe Float) () Exp -> IO (Wire Int ExpV1)
program = fmap timing . flip evalStateT 0 . transWire

delay t = WDelay (Just t)
wText2D = WText2D ()
{-
instance Timed Float where
  time = error "Can't get the time in Float land"
-}
curveToCameraPath :: (forall c . Curve c) -> forall s . Floating s => s -> (V3 s, V3 (V3 s))
curveToCameraPath curve t = (getNoTime <$> curve (NoTime t), fmap getNoTime <$> frenetFrame curve (NoTime t))

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

type ExpV1 = LC.Exp V Float
type ExpV3 = V3 ExpV1

type Wire = Wire_ Float

data Wire_ dur i e
    = Wire1D
        { wInfo :: i
        , wDuration  :: dur
        , wXResolution :: Int
        , wVertex    :: V3 e -> V3 e
        }
    | Wire2D
        { wInfo :: i
        , wDuration  :: dur
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
        , wDuration  :: dur
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
        { wWires :: [Wire_ dur i e]
        }
    | WVertical
        { wWires :: [Wire_ dur i e]
        }
    | WFadeOut
        { wDuration  :: dur
        }
    | WCamera
        { wDuration  :: dur
        , wCamera :: Camera
        }
    | WText2D
        { wInfo :: i
        , wDuration  :: dur
        , wTextPosition :: LC.M33F
        , wText :: String
        }
    | WDelay
        { wDuration  :: dur
        }
    | WHalt
        { completeHalt :: Bool      -- False: time goes on and reset when spaces is pressed
        , resetTime :: Bool
        }
    | WSound
        { wDuration  :: dur
        , wSample :: FilePath
        }
    -- sprite
    -- color
    -- normal

data Camera
    = CamCurve (forall s . Floating s => CurveS s)
    | CamMat Mat4

flattenWire (WHorizontal w) = concatMap flattenWire w
flattenWire (WVertical w) = concatMap flattenWire w
flattenWire w = [w]

hardHalt = WHalt True True


wire1D i f = Wire1D () Nothing i (to1 f)

wire2DNorm :: Bool -> Int -> Int -> Patch Identity -> Wire_ (Maybe t) () Exp
wire2DNorm t i j v = Wire2D () Nothing t False i j (to2 v) (Just $ to2 $ normalPatch v) Nothing Nothing

wParticle i j k v c = WParticle () Nothing False i j k v Nothing c Nothing

wire2DNormAlpha :: Bool -> Int -> Int -> Patch Identity -> Maybe (V2 Exp -> V3 Exp) -> Maybe (V2 Exp -> Exp) -> Wire_ (Maybe t) () Exp
wire2DNormAlpha t i j v c a = Wire2D () Nothing t False i j (to2 v) (Just $ to2 $ normalPatch v) (to2 <$> c) (to2 <$> a)

to1 f (V3 x _ _) = f x
to2 f (V3 x y _) = f (V2 x y)


timing :: RealFrac t => Wire_ (Maybe t) i e -> Wire_ t i e
timing (trav -> PC (t, _) g) = g t

pattern PC t f = Pair (Constant t) f

trav :: RealFrac t => Wire_ (Maybe t) i e -> Product (Constant (t, Bool)) ((->) t) (Wire_ t i e)
trav = \case
    WHorizontal (traverse ((\(PC (t, b) f) -> PC (Maximum t, All b) f) . trav) -> PC (Maximum t, All b) f)
        -> PC (t, b) $ WHorizontal . f
    WVertical (foldMap ((\(PC (t, b) f) -> (Sum t, [t], [b], Sum (fromEnum b), [f])) . trav) -> (Sum t, ts, bs, Sum b, fs))
        -> PC (t, or bs) $ WVertical . zipWith ($) fs . zipWith (+) ts . zipWith ff bs . repeat . (/ fromIntegral b) . subtract t
      where
        ff True = id
        ff False = const 0
    WHalt{..} -> PC (0, False) $ \_ -> WHalt{..}
    w@(wDuration -> Nothing) -> PC (0, True)  $ \t -> w { wDuration = t }
    w@(wDuration -> Just d)  -> PC (d, False) $ \_ -> w { wDuration = d }

transW :: (V3 e -> V3 e) -> Wire_ t i e -> Wire_ t i e
transW tr = \case
    WHorizontal x -> WHorizontal $ map (transW tr) x
    WVertical x -> WVertical $ map (transW tr) x
    w@Wire1D{} -> w { wVertex = tr . wVertex w }
    w@Wire2D{} -> w { wVertex = tr . wVertex w }
    w@WParticle{} -> w { wVertex = tr . wVertex w }
    w -> w


newtype Maximum d = Maximum {getMaximum :: d}
    deriving Functor
instance Real d => Monoid (Maximum d) where
    mempty = Maximum 0
    Maximum d `mappend` Maximum e = Maximum (d `max` e)

transWire :: Wire_ t () Exp -> StateT Int IO (Wire_ t Int ExpV1)
transWire = \case
    Wire1D _ d i f
        -> newid >>= \id -> Wire1D <$> pure id <*> pure d <*> pure i <*> (lift . transFun3 "t" "s" "k") f
    Wire2D _ d b sc i j v n c a
        -> newid >>= \id -> Wire2D <$> pure id <*> pure d <*> pure b <*> pure sc <*> pure i <*> pure j <*> lift (transFun3 "t" "s" "k" v) <*> traverse (lift . transFun3 "t" "s" "k") n <*> traverse (lift . transFun3 "t" "s" "k") c <*> (traverse) (lift . transFun3_ "t" "s" "k") a
    WParticle _ d sc i j k v n c a
        -> newid >>= \id -> WParticle <$> pure id <*> pure d <*> pure sc <*> pure i <*> pure j <*> pure k <*> lift (transFun3 "t" "s" "k" v) <*> traverse (lift . transFun3 "t" "s" "k") n <*> traverse (lift . transFun3 "t" "s" "k") c <*> (traverse) (lift . transFun3_ "t" "s" "k") a
    WText2D _ dur ws txt -> newid >>= \id -> WText2D <$> pure id <*> pure dur <*> pure ws <*> pure txt
    WHorizontal ws  -> WHorizontal <$> traverse transWire ws
    WVertical ws    -> WVertical <$> traverse transWire ws
    WFadeOut ws     -> pure $ WFadeOut ws
    WDelay t        -> pure $ WDelay t
    WCamera dur ws  -> pure $ WCamera dur ws
    WSound a b      -> pure $ WSound a b
    WHalt{..}       -> pure WHalt{..}

newid = do
    st <- get
    put $ st + 1
    return st

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
            Exp_ e      -> exp' e
            Log_ e      -> log' e


testComplexity = (normalPatch $ tubularPatch (torusKnot 1 5) (mulSV3 0.1 . unKnot)) $ V2 "x" "y"  :: V3 Exp
testComplexity' = (normalPatch $ tubularPatch (mulSV3 0.1 . unKnot) (mulSV3 0.1 . unKnot)) $ V2 "x" "y"  :: V3 Exp

