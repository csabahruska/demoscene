{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module AD where

import qualified Data.Map as Map
--import Data.Function
--import Data.List
import Data.String
--import Data.Monoid
import Data.Traversable
--import Data.Reflection (Reifies)
--import Control.Applicative
import qualified Data.Traversable as T
import qualified Data.Foldable as F

--import Numeric.AD.Internal.Reverse (Tape)
import Numeric.AD ()

import Data.Reify
--import Data.Reify.Graph

newtype Mu a = In (a (Mu a))

instance (T.Traversable a) => MuRef (Mu a) where
  type DeRef (Mu a) = a

  mapDeRef f (In a) = T.traverse f a

data Exp_ a
    = C_ Float
    | Var_ String
    | Add_ a a 
    | Mul_ a a
    | Neg_ a 
    | Recip_ a
    | Abs_ a
    | Signum_ a
    | Sin_ a
    | Cos_ a
    | ASin_ a
    | ACos_ a
    | ATan_ a
    | Sinh_ a
    | Cosh_ a
    | ASinh_ a
    | ACosh_ a
    | ATanh_ a
    | Exp_ a
    | Log_ a
        deriving (Show, Functor, F.Foldable, Traversable)

type Exp = Mu Exp_ 

pattern C x = In (C_ x)
pattern Var x = In (Var_ x)
pattern Add x y = In (Add_ x y)
pattern Mul x y = In (Mul_ x y)
pattern Neg x = In (Neg_ x)
pattern Recip x = In (Recip_ x)
pattern Abs x = In (Abs_ x)
pattern Signum x = In (Signum_ x)
pattern Sin x = In (Sin_ x)
pattern Cos x = In (Cos_ x)
pattern ASin x = In (ASin_ x)
pattern ACos x = In (ACos_ x)
pattern ATan x = In (ATan_ x)
pattern Sinh x = In (Sinh_ x)
pattern Cosh x = In (Cosh_ x)
pattern ASinh x = In (ASinh_ x)
pattern ACosh x = In (ACosh_ x)
pattern ATanh x = In (ATanh_ x)
pattern Exp x = In (Exp_ x)
pattern Log x = In (Log_ x)

reify :: Exp -> IO (Graph Exp_)
reify = reifyGraph

pattern Zero <- C 0
pattern One <- C 1

instance IsString Exp where
    fromString = Var

withC f _ (C x) = C $ f x
withC _ g x = g x

flipC _ g x y@(C _) = g y x
flipC f _ x y = f x y

add = flipC f f where
    f (C a) (C b) = C $ a + b
    f Zero x = x
    f (C a) (Add (C b) c) = Add (C $ a + b) c
    f (Add (C a) a') (Add (C b) c) = Add (C $ a + b) $ a' + c
    f (Neg x) (Neg y) = negate $ x + y
    f x y = Add x y

negate_ (Neg a) = a
negate_ a = Neg a

recip_ (Recip a) = a
recip_ (Neg a) = negate $ recip a
recip_ a = Recip a

mul = flipC f f where
    f (C a) (C b) = C $ a * b
    f Zero _ = 0
    f One x = x
    f (C a) (Mul (C b) c) = Mul (C $ a * b) c
    f (Mul (C a) a') (Mul (C b) c) = Mul (C $ a * b) $ a' * c
    f (Neg x) y = negate (x * y)
    f x (Neg y) = negate (x * y)
    f (Recip x) (Recip y) = recip (x * y)
    f x y = Mul x y

type Env a = Map.Map String a

instance Show Exp where
    showsPrec p e = case e of
        Add x y -> showParen (p > 2) $ showsPrec 2 x . (" + " ++) . showsPrec 2 y 
        Neg x -> showParen (p >= 2) $ (" -" ++) . showsPrec 2 x 
        Mul x y -> showParen (p > 3) $ showsPrec 3 x . (" * " ++) . showsPrec 3 y 
        Recip x -> showParen (p >= 3) $ (" 1/" ++) . showsPrec 3 x 
        Abs x -> showParen (p > 9) $ ("abs " ++) . showsPrec 10 x
        Signum x -> showParen (p > 9) $ ("signum " ++) . showsPrec 10 x
        Sin x -> showParen (p > 9) $ ("sin " ++) . showsPrec 10 x
        Cos x -> showParen (p > 9) $ ("cos " ++) . showsPrec 10 x
        ASin x -> showParen (p > 9) $ ("asin " ++) . showsPrec 10 x
        ACos x -> showParen (p > 9) $ ("acos " ++) . showsPrec 10 x
        Exp x -> showParen (p > 9) $ ("exp " ++) . showsPrec 10 x
        Log x -> showParen (p > 9) $ ("log " ++) . showsPrec 10 x
        Var s -> (s ++)
        C 0 -> ("0" ++)
        C 1 -> ("1" ++)
        C i -> shows i

instance Num Exp where
    (*) = mul
    (+) = add
    negate = withC negate negate_
    abs = withC abs Abs
    signum = withC signum Signum
    fromInteger = C . fromInteger

instance Fractional Exp where
    fromRational = C . fromRational
    recip = withC recip recip_

instance Floating Exp where
    exp = withC exp Exp
    log = withC log Log
    pi = 3.141592653589793
    sin = withC sin Sin
    cos = withC cos Cos
    asin = withC asin ASin
    acos = withC acos ACos
    atan = withC atan ATan
    sinh = withC sinh Sinh
    cosh = withC cosh Cosh
    asinh = withC asinh ASinh
    acosh = withC acosh ACosh
    atanh = withC atanh ATanh

(x:y:z:_) = map (Var . (:[])) ['x'..]
(a:b:c:d:_) = map (Var . (:[])) ['a'..]
vec = [x, y, z]


{-
adds (Add a b) = adds a ++ adds b
adds x = [x]

muls (Mul a b) = muls a ++ muls b
muls x = [x]

norm e
    | l@(_:_:_) <- adds e = foldl1 add $ sortBy (compare `on` ty) $ concatMap adds $ map norm l
    | l@(_:_:_) <- muls e = foldl1 mul $ sortBy (compare `on` ty) $ concatMap muls $ map norm l
    | otherwise = e
-}
{-
ty (C _) = 0
ty (Var _) = 1
ty _ = 2

instance Ord Exp where
    compare a b = compare (eval mempty a) (eval mempty b)
-}
{-
subs :: Env Exp -> Exp -> Exp
subs e x = case x of
    Add a b -> subs e a + subs e b
    Mul a b -> subs e a * subs e b
    Sub a b -> subs e a - subs e b
    Div a b -> subs e a / subs e b
    Abs a -> abs (subs e a)
    Signum a -> signum (subs e a)
    Sin a -> sin (subs e a)
    Cos a -> cos (subs e a)
    Exp a -> exp (subs e a)
    Log a -> log (subs e a)
    C a -> realToFrac a
    Var s -> case Map.lookup s e of
        Nothing -> Var s
        Just e  -> e

eval :: Floating a => Env a -> Exp -> a
eval e x = case x of
    Add a b -> eval e a + eval e b
    Mul a b -> eval e a * eval e b
    Sub a b -> eval e a - eval e b
    Div a b -> eval e a / eval e b
    Abs a -> abs (eval e a)
    Signum a -> signum (eval e a)
    Sin a -> sin (eval e a)
    Cos a -> cos (eval e a)
    ASin a -> asin (eval e a)
    ACos a -> acos (eval e a)
    Exp a -> exp (eval e a)
    Log a -> log (eval e a)
    C a -> realToFrac a
    Var s -> case Map.lookup s e of
        Nothing -> error $ "unknown value of: " ++ s
        Just e  -> e
--    Var s -> e Map.! s
-}

--deriving instance Functor (AD s)
--deriving instance Functor Forward

--der v@(Var name) e = diff (\x -> eval (Map.singleton name x) e) v
{-
grad_ :: (forall s. Reifies s Tape => [Reverse s Exp] -> Reverse s Exp) -> Vec
grad_ f = map norm $ grad f vec

grad__ :: Exp -> Vec
grad__ x = grad_ (flip evalMap x)

--grad' f = grad f vec

type Vec = [Exp]

testDiff :: Exp
testDiff = diff (\x -> 3 * x^2 + 2 * x - 4) 4

testGrad :: [Exp]
testGrad = grad (\[x, y, z] -> 3 * x^20 + 2 * x * y - 4) [x,3 * y,0]

testJacobian :: [[Exp]]
testJacobian = jacobian (\[x,y] -> [y,x,sin (x*y)]) [2 * x * y,1]

mkMap = Map.fromList . zip ["x","y","z"]
evalMap = eval . mkMap

hessian_ = map grad__ . grad__

testHessian :: [[Exp]]
testHessian = hessian_ $ x*y
-}
