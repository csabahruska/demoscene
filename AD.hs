{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module AD where

import qualified Data.Map as Map
import Data.Function
import Data.List
import Data.String
import Data.Monoid
import Data.Traversable
import Data.Reflection (Reifies)
import Numeric.AD.Internal.Reverse (Reverse, Tape)
import Numeric.AD

data Exp
    = C Float
    | Var String
    | Add Exp Exp 
    | Sub Exp Exp 
    | Mul Exp Exp
    | Div Exp Exp
    | Abs Exp
    | Signum Exp
    | Sin Exp
    | Cos Exp
    | ASin Exp
    | ACos Exp
    | Exp Exp
    | Log Exp
        deriving (Eq)

pattern Neg x <- Sub 0 x

instance IsString Exp where
    fromString = Var

ty (C _) = 0
ty (Var _) = 1
ty _ = 2


instance Ord Exp where
    compare a b = compare (eval mempty a) (eval mempty b)

--    | E Exp Integer

flipC f g x y@(C _) = g y x
flipC f g x y = f x y

add = flipC f f where
    f 0 x = x
    f (C a) (C b) = C $ a + b
    --add x y | x == y = norm $ 2 * x
    f (Neg x) (Neg y) = neg $ add x y
    f x (Neg y) = sub x y
    f (Neg x) y = sub y x
    f x y = Add x y

neg x = sub 0 x

--sub 0 x = x
sub (C a) (C b) = C $ a - b
sub 0 (Neg a) = a
sub x y = Sub x y

mul = flipC f f where
    f 0 x = 0
    f 1 x = x
    f (C a) (C b) = C $ a * b
    f (C a) (Mul (C b) c) = Mul (C $ a * b) c
    f (Mul (C a) a') (Mul (C b) c) = Mul (C $ a * b) $ mul a' c
    f (Neg x) (Neg y) = mul x y
    f (Neg x) y = neg (mul x y)
    f x (Neg y) = neg (mul x y)
    f x y = Mul x y

adds (Add a b) = adds a ++ adds b
adds x = [x]

muls (Mul a b) = muls a ++ muls b
muls x = [x]

norm e
    | l@(_:_:_) <- adds e = foldl1 add $ sortBy (compare `on` ty) $ concatMap adds $ map norm l
    | l@(_:_:_) <- muls e = foldl1 mul $ sortBy (compare `on` ty) $ concatMap muls $ map norm l
    | otherwise = e

type Env a = Map.Map String a

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

instance Show Exp where
    showsPrec p e = case e of
        Add x y -> showParen (p > 2) $ showsPrec 2 x . (" + " ++) . showsPrec 2 y 
        Sub x y -> showParen (p >= 2) $ showsPrec 2 x . (" - " ++) . showsPrec 2 y 
        Mul x y -> showParen (p > 3) $ showsPrec 3 x . (" * " ++) . showsPrec 3 y 
        Div x y -> showParen (p >= 3) $ showsPrec 3 x . (" / " ++) . showsPrec 3 y 
--        E x i -> showParen (p > 4) $ showsPrec 4 x . ("^" ++) . shows i 
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
    (*) = mul --flipC mul mul
    (+) = add --flipC add add
    (-) = sub
    abs = Abs
    signum = Signum
    fromInteger = C . fromInteger

instance Fractional Exp where
    fromRational = C . fromRational
    (/) = Div

instance Floating Exp where
    sin = Sin
    cos = Cos
    exp = Exp
    log = Log
    pi = 3.141592653589793
    asin = error "asin"
    atan = error "atan"
    acos = ACos
    sinh = ASin
    cosh = error "cosh"
    asinh = error "asinh"
    atanh = error "atanh"
    acosh = error "acosh"

(x:y:z:_) = map (Var . (:[])) ['x'..]
(a:b:c:d:_) = map (Var . (:[])) ['a'..]
vec = [x, y, z]

--deriving instance Functor (AD s)
--deriving instance Functor Forward

--der v@(Var name) e = diff (\x -> eval (Map.singleton name x) e) v

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

