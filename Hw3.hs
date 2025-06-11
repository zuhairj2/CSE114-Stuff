{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

{- | CSE114A: Programming Assignment 3

     See the README for instructions.
 -}

module Hw3 where

import Prelude hiding (lookup)

-- | The `Expr` data type represents simple arithmetic expressions.
data Expr = PlusE Expr Expr
          | MinusE Expr Expr
          | TimesE Expr Expr
          | NumE Int

-- | `simpleEval` takes an expr `e`, evaluates it, and returns its value as an `Int`.
--
-- >>> simpleEval (NumE 8)
-- 8
--
-- >>> simpleEval (PlusE (NumE 3) (MinusE (NumE 2) (NumE 1)))
-- 4
--
-- >>> simpleEval (TimesE (TimesE (NumE 4) (NumE 6)) (PlusE (NumE 1) (NumE 2)))
-- 72
--
-- >>> simpleEval (MinusE (NumE (-3)) (PlusE (NumE 1) (NumE 2)))
-- -6

simpleEval :: Expr -> Int
simpleEval (NumE n) = n
simpleEval (PlusE e1 e2) = simpleEval e1 + simpleEval e2
simpleEval (MinusE e1 e2) = simpleEval e1 - simpleEval e2
simpleEval (TimesE e1 e2) = simpleEval e1 * simpleEval e2

-- | `opMaybe` takes a binary operation `op` of type `Int -> Int -> Int`
--   and two expressions `m1` and `m2` of type `Maybe Int`.
--   If either (or both) of `m1` and `m2` is `Nothing`, then `opMaybe`
--   returns `Nothing`.  Otherwise, `m1` and `m2` are both `Int`s with
--   `Just` wrappers around them, and `opMaybe` returns the result of
--   combining those `Int` values using `op`, but wrapped in the `Just`
--   constructor.
--
-- >>> opMaybe (+) (Just 3) (Just 5)
-- Just 8
--
-- >>> opMaybe (-) (Just 3) (Just 5)
-- Just (-2)
--
-- >>> opMaybe (*) (Just 3) (Just 5)
-- Just 15
--
-- >>> opMaybe (+) (Just 3) Nothing
-- Nothing
--
-- >>> opMaybe (-) Nothing (Just 3)
-- Nothing
--
-- >>> opMaybe (*) Nothing Nothing
-- Nothing
--
-- >>> opMaybe (\x y -> x + y + 5) (Just 3) (Just 2)
-- Just 10
--
-- >>> opMaybe (\x y -> x + y + 5) Nothing (Just 2)
-- Nothing

opMaybe :: (Int -> Int -> Int) -> Maybe Int -> Maybe Int -> Maybe Int
opMaybe op (Just m1) (Just m2) = Just (op m1 m2)
opMaybe _ _ _ = Nothing






data VarExpr = PlusVE VarExpr VarExpr
             | MinusVE VarExpr VarExpr
             | TimesVE VarExpr VarExpr
             | NumVE Int
             | Var String

type ListEnv = [(String, Int)]

-- | `varExprListEval` takes an expression `e` of type `VarExpr`
--   and an environment `env` of type `ListEnv`
--   and evaluates `e` given `env`.
--   If `e` contains variables not bound in `env`,
--   `varExprListEval` returns `Nothing`.
--   Otherwise, `varExprListEval` returns the value of `e`, wrapped in `Just`.
--
-- >>> varExprListEval [("x", 3)] (Var "x")
-- Just 3
--
-- >>> varExprListEval [("x", 4), ("y", 7)] (PlusVE (Var "x") (MinusVE (NumVE 2) (Var "y")))
-- Just (-1)
--
-- >>> varExprListEval [("x", 4), ("y", 7)] (TimesVE (Var "z") (NumVE 3))
-- Nothing
--
-- >>> varExprListEval [] (TimesVE (Var "x") (NumVE 3))
-- Nothing
--
-- >>> varExprListEval [] (TimesVE (NumVE 7) (NumVE 3))
-- Just 21

--helper function
searchfor :: String -> ListEnv -> Maybe Int
searchfor _ [] = Nothing
searchfor var ((k, v):xs)
              | var == k = Just v
              | otherwise = searchfor var xs

varExprListEval :: ListEnv -> VarExpr -> Maybe Int
varExprListEval _ (NumVE n) = Just n
varExprListEval env (Var x) = searchfor x env
varExprListEval env (PlusVE e1 e2) = opMaybe (+) (varExprListEval env e1) (varExprListEval env e2) 
varExprListEval env (MinusVE e1 e2) = opMaybe (-) (varExprListEval env e1) (varExprListEval env e2) 
varExprListEval env (TimesVE e1 e2) = opMaybe (*) (varExprListEval env e1) (varExprListEval env e2) 



type FunEnv = String -> Maybe Int

-- | `varExprFunEval` takes an expression `e` of type `VarExpr`
--   and an environment `env` of type `FunEnv`
--   and evaluates `e` given `env`.
--   If `e` contains variables not bound in `env`,
--   `varExprFunEval` returns `Nothing`.
--   Otherwise, `varExprFunEval` returns the value of `e`, wrapped in `Just`.
--
-- >>> varExprFunEval (\v -> if v == "x" then Just 3 else Nothing) (Var "x")
-- Just 3
--
-- >>> varExprFunEval (\v -> if v == "x" then Just 4 else (if v == "y" then Just 7 else Nothing)) (PlusVE (Var "x") (MinusVE (NumVE 2) (Var "y")))
-- Just (-1)
--
-- >>> varExprFunEval (\v -> if v == "x" then Just 4 else (if v == "y" then Just 7 else Nothing)) (TimesVE (Var "z") (NumVE 3))
-- Nothing
--
-- >>> varExprFunEval (\v -> Nothing) (TimesVE (Var "x") (NumVE 3))
-- Nothing
--
-- >>> varExprFunEval (\v -> Nothing) (TimesVE (NumVE 7) (NumVE 3))
-- Just 21

varExprFunEval :: FunEnv -> VarExpr -> Maybe Int
varExprFunEval _ (NumVE n ) = Just n
varExprFunEval env (Var x) = env x
varExprFunEval env (PlusVE e1 e2) = opMaybe (+) (varExprFunEval env e1) (varExprFunEval env e2) 
varExprFunEval env (MinusVE e1 e2) = opMaybe (-) (varExprFunEval env e1) (varExprFunEval env e2) 
varExprFunEval env (TimesVE e1 e2) = opMaybe (*) (varExprFunEval env e1) (varExprFunEval env e2) 


-- | `show` takes a `VarExpr` and returns a printable string representation of it.
--
-- >>> show (NumVE 8)
-- "8"
-- 
-- >>> show (TimesVE (NumVE 7) (NumVE 3))
-- "(7 * 3)"
-- 
-- >>> show (PlusVE (NumVE 3) (MinusVE (NumVE 2) (NumVE 1)))
-- "(3 + (2 - 1))"
-- 
-- >>> show (TimesVE (TimesVE (NumE 4) (NumVE 6)) (PlusVE (NumVE 1) (NumVE 2)))
-- "((4 * 6) * (1 + 2))"
--
-- >>> show (Var "x")
-- "x"
--
-- >>> show (TimesVE (Var "x") (PlusVE (NumVE 1) (Var "y")))
-- "(x * (1 + y))"

instance Show VarExpr where
  show :: VarExpr -> String
  show (NumVE n) = show n
  show (Var x) = x
  show (PlusVE e1 e2) = "("++ show e1 ++" + " ++ show e2 ++")"
  show (MinusVE e1 e2) = "("++ show e1 ++" - " ++ show e2 ++")"
  show (TimesVE e1 e2) = "("++ show e1 ++" * " ++ show e2 ++")"


-- | `(==)` takes two `VarExpr`s and compares them for equality,
--   returning `True` if they are equal and `False` otherwise.
--   `VarExpr`s are considered equal if they evaluate to the same value.
--
-- >>> (TimesVE (NumVE 7) (NumVE 3)) == (TimesVE (NumVE 3) (NumVE 7))
-- True
-- >>> (TimesVE (NumVE 7) (NumVE 3)) == (TimesVE (NumVE 6) (NumVE 7))
-- False
-- >>> NumVE 2 == NumVE 3
-- False
-- >>> Var "x" == Var "x"
-- True
-- >>> Var "y" == Var "x"
-- True
-- >>> (TimesVE (NumVE 7) (NumVE 3)) == (TimesVE (NumVE 7) (Var "x")))
-- False

instance Eq VarExpr where
  (==) :: VarExpr -> VarExpr -> Bool
  (==) e1 e2 = varExprListEval [] e1 == varExprListEval [] e2


-- | A type class for environments
--
-- >>> lookupInEnv "x" (extendEnv "x" 3 (emptyEnv :: ListEnv))
-- Just 3
--
-- >>> lookupInEnv "y" (extendEnv "x" 3 (emptyEnv :: ListEnv))
-- Nothing
--
-- >>> lookupInEnv "y" (extendEnv "x" 3 (extendEnv "y" 4 (emptyEnv :: ListEnv)))
-- Just 4
--
-- >>> lookupInEnv "x" (extendEnv "x" 3 (emptyEnv :: FunEnv))
-- Just 3
--
-- >>> lookupInEnv "y" (extendEnv "x" 3 (emptyEnv :: FunEnv))
-- Nothing
--
-- >>> lookupInEnv "y" (extendEnv "x" 3 (extendEnv "y" 4 (emptyEnv :: FunEnv)))
-- Just 4

class Env a where
  emptyEnv :: a
  lookupInEnv :: String -> a -> Maybe Int
  extendEnv :: String -> Int -> a -> a

instance Env ListEnv where
  emptyEnv :: ListEnv
  emptyEnv = []


  lookupInEnv :: String -> ListEnv -> Maybe Int
  lookupInEnv _ [] = Nothing
  lookupInEnv s ((k,v) : xs)
    | s == k = Just v
    | otherwise = lookupInEnv s xs


  extendEnv :: String -> Int -> ListEnv -> ListEnv
  extendEnv s n env = (s, n) : env

instance Env FunEnv where
  emptyEnv :: FunEnv
  emptyEnv var = Nothing 


  lookupInEnv :: String -> FunEnv -> Maybe Int
  lookupInEnv s env = env s


  extendEnv :: String -> Int -> FunEnv -> FunEnv
  extendEnv s n env = newEnv
    where 
      newEnv :: FunEnv
      newEnv x
        | x == s = Just n
        | otherwise = env x



-- | `varExprEval` takes an expression `e` of type `VarExpr`
--   and an environment `env` that implements the `Env` type class,
--   and evaluates `e` given `env`.
--   If `e` contains variables not bound in `env`,
--   `varExprFunEval` returns `Nothing`.
--   Otherwise, `varExprFunEval` returns the value of `e`, wrapped in `Just`.
--
-- >>> varExprEval (extendEnv "x" 3 (emptyEnv :: ListEnv)) (Var "x")
-- Just 3
--
-- >>> varExprEval (extendEnv "x" 3 (extendEnv "y" 7 (emptyEnv :: ListEnv))) (PlusVE (Var "x") (MinusVE (NumVE 2) (Var "y")))
-- Just (-3)
--
-- >>> varExprEval (extendEnv "x" 3 (extendEnv "y" 7 (emptyEnv :: ListEnv))) (TimesVE (Var "z") (NumVE 3))
-- Nothing
--
-- >>> varExprEval (emptyEnv :: ListEnv) (TimesVE (Var "x") (NumVE 3))
-- Nothing
--
-- >>> varExprEval (emptyEnv :: ListEnv) (TimesVE (NumVE 7) (NumVE 3))
-- Just 21
--
-- >>> varExprEval (extendEnv "x" 3 (emptyEnv :: FunEnv)) (Var "x")
-- Just 3
--
-- >>> varExprEval (extendEnv "x" 3 (extendEnv "y" 7 (emptyEnv :: FunEnv))) (PlusVE (Var "x") (MinusVE (NumVE 2) (Var "y")))
-- Just (-3)
--
-- >>> varExprEval (extendEnv "x" 3 (extendEnv "y" 7 (emptyEnv :: FunEnv))) (TimesVE (Var "z") (NumVE 3))
-- Nothing
--
-- >>> varExprEval (emptyEnv :: FunEnv) (TimesVE (Var "x") (NumVE 3))
-- Nothing
--
-- >>> varExprEval (emptyEnv :: FunEnv) (TimesVE (NumVE 7) (NumVE 3))
-- Just 21

varExprEval :: Env a => a -> VarExpr -> Maybe Int
varExprEval _ (NumVE n) = Just n
varExprEval env (Var x) = lookupInEnv x env
varExprEval env (PlusVE  e1 e2) = opMaybe (+) (varExprEval env e1) (varExprEval env e2)
varExprEval env (MinusVE  e1 e2) = opMaybe (-) (varExprEval env e1) (varExprEval env e2)
varExprEval env (TimesVE  e1 e2) = opMaybe (*) (varExprEval env e1) (varExprEval env e2)





-- | `evalAll` takes a list of `VarExpr`s and an environment,
--   and evaluates each VarExpr using the provided environment,
--   resulting in a list of `Maybe Int` values.
--
-- >>> evalAll (emptyEnv :: ListEnv) [NumVE 30, NumVE 40, NumVE 20]
-- [Just 30, Just 40, Just 20]
--
-- >>> evalAll (emptyEnv :: ListEnv) [TimesVE (Var "z") (NumVE 3), TimesVE (NumVE 0) (NumVE 3)]
-- [Nothing, Just 0]
--
-- >>> evalAll (extendEnv "z" 3 (emptyEnv :: ListEnv)) [NumVE 0, MinusVE (NumVE 2) (NumVE 1), TimesVE (Var "z") (NumVE 3)]
-- [Just 0, Just 1, Just 9]
--
-- >>> evalAll (emptyEnv :: ListEnv) [TimesVE (NumVE 7) (NumVE 3), TimesVE (NumVE 3) (NumVE 7)]
-- [Just 21, Just 21]
--
-- >>> evalAll (extendEnv "z" 3 (emptyEnv :: ListEnv)) [Var "z", TimesVE (Var "z") (Var "z"), NumVE 1]
-- [Just 3, Just 9, Just 1]
--
-- >>> evalAll (extendEnv "z" 3 (emptyEnv :: ListEnv)) [NumVE 1, Var "x", Var "z"]
-- [Just 1, Nothing, Just 3]]
--
-- >>> evalAll (emptyEnv :: FunEnv) [NumVE 30, NumVE 40, NumVE 20]
-- [Just 30, Just 40, Just 20]
--
-- >>> evalAll (emptyEnv :: FunEnv) [TimesVE (Var "z") (NumVE 3), TimesVE (NumVE 0) (NumVE 3)]
-- [Nothing, Just 0]
--
-- >>> evalAll (extendEnv "z" 3 (emptyEnv :: FunEnv)) [NumVE 0, MinusVE (NumVE 2) (NumVE 1), TimesVE (Var "z") (NumVE 3)]
-- [Just 0, Just 1, Just 9]
--
-- >>> evalAll (emptyEnv :: FunEnv) [TimesVE (NumVE 7) (NumVE 3), TimesVE (NumVE 3) (NumVE 7)]
-- [Just 21, Just 21]
--
-- >>> evalAll (extendEnv "z" 3 (emptyEnv :: FunEnv)) [Var "z", TimesVE (Var "z") (Var "z"), NumVE 1]
-- [Just 3, Just 9, Just 1]
--
-- >>> evalAll (extendEnv "z" 3 (emptyEnv :: FunEnv)) [NumVE 1, Var "x", Var "z"]
-- [Just 1, Nothing, Just 3]]

evalAll :: Env a => a -> [VarExpr] -> [Maybe Int]
evalAll env exprs = map f exprs
  where f :: VarExpr -> Maybe Int
        f expr = varExprEval env expr



-- | `sumEval` takes a list of `VarExpr`s and an environment,
--   evaluates each `VarExpr` using the provided environment,
--   and sums their values, resulting in a single `Maybe Int` value.
--   If *any* of the provided `VarExpr`s evaluate to `Nothing`,
--   then sumEval should return `Nothing`;
--   otherwise, it should return the sum of the `Int`s they evaluate to,
--   wrapped in the `Just` constructor.
--
-- >>> sumEval (emptyEnv :: ListEnv) [NumVE 30, NumVE 40, NumVE 20]
-- Just 90
--
-- >>> sumEval (emptyEnv :: ListEnv) [TimesVE (Var "z") (NumVE 3), TimesVE (NumVE 0) (NumVE 3)]
-- Nothing
--
-- >>> sumEval (extendEnv "z" 3 (emptyEnv :: ListEnv)) [NumVE 0, MinusVE (NumVE 2) (NumVE 1), TimesVE (Var "z") (NumVE 3)]
-- Just 10
--
-- >>> sumEval (emptyEnv :: ListEnv) [TimesVE (NumVE 7) (NumVE 3), TimesVE (NumVE 3) (NumVE 7)]
-- Just 42
--
-- >>> sumEval (extendEnv "z" 3 (emptyEnv :: ListEnv)) [Var "z", TimesVE (Var "z") (Var "z"), NumVE 1]
-- Just 13
--
-- >>> sumEval (extendEnv "z" 3 (emptyEnv :: ListEnv)) [NumVE 1, Var "x", Var "z"]
-- Nothing
--
-- >>> sumEval (emptyEnv :: FunEnv) [NumVE 30, NumVE 40, NumVE 20]
-- Just 90
--
-- >>> sumEval (emptyEnv :: FunEnv) [TimesVE (Var "z") (NumVE 3), TimesVE (NumVE 0) (NumVE 3)]
-- Nothing
--
-- >>> sumEval (extendEnv "z" 3 (emptyEnv :: FunEnv)) [NumVE 0, MinusVE (NumVE 2) (NumVE 1), TimesVE (Var "z") (NumVE 3)]
-- Just 10
--
-- >>> sumEval (emptyEnv :: FunEnv) [TimesVE (NumVE 7) (NumVE 3), TimesVE (NumVE 3) (NumVE 7)]
-- Just 42
--
-- >>> sumEval (extendEnv "z" 3 (emptyEnv :: FunEnv)) [Var "z", TimesVE (Var "z") (Var "z"), NumVE 1]
-- Just 13
--
-- >>> sumEval (extendEnv "z" 3 (emptyEnv :: FunEnv)) [NumVE 1, Var "x", Var "z"]
-- Nothing

sumEval :: Env a => a -> [VarExpr] -> Maybe Int
sumEval env exprs = foldr f (Just 0) exprs
  where f :: VarExpr -> Maybe Int -> Maybe Int
        f expr m = opMaybe (+) (varExprEval env expr) m

