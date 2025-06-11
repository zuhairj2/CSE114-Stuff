import Text.Printf (printf)
import Data.Set

message :: String
message = printf "Welcome to lecture %s!" "9"

{-

- What's coming up in 114A
- More about tail calls
- More fun with ASTs
  - Break/quiz
  - An interpreter for our ArithExpr language
  - Adding features to our ArithExpr language
  - An AST type for lambda calculus
-}

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

-- Accumulator-passing style: pass around an extra number
-- that accumulates the result you want to return.
factAcc :: Int -> Int -> Int
factAcc 0 acc = acc
factAcc n acc = factAcc (n - 1) (n * acc)

factAcc' :: Int -> Int
factAcc' n = factAcc n 1

-- In continuation-passing style (CPS): you pass around an extra *function*
-- that accumulates the *computation you want to do*.
-- This computation is known as a *continuation*.

factCPS :: Int -> (Int -> Int) -> Int
factCPS 0 k = k 1
factCPS n k = factCPS (n-1) (\v -> k (n * v))

{-

factCPS 3 (\v0 -> v0)
factCPS 2 (\v1 -> (\v0 -> v0) (3 * v1))
factCPS 1 (\v2 -> (\v1 -> (\v0 -> v0) (3 * v1)) (2 * v2))
factCPS 0 (\v3 -> (\v2 -> (\v1 -> (\v0 -> v0) (3 * v1)) (2 * v2)) (1 * v3))
(\v3 -> (\v2 -> (\v1 -> (\v0 -> v0) (3 * v1)) (2 * v2)) (1 * v3)) 1

(\v2 -> (\v1 -> (\v0 -> v0) (3 * v1)) (2 * v2)) (1 * 1)
(\v2 -> (\v1 -> (\v0 -> v0) (3 * v1)) (2 * v2)) 1
(\v1 -> (\v0 -> v0) (3 * v1)) (2 * 1)
(\v1 -> (\v0 -> v0) (3 * v1)) 2
(\v0 -> v0) (3 * 2)
(\v0 -> v0) 6
6
-}



fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- A way to implement *anything* tail-recursively:
-- Continuation-passing style (CPS)

fibCPS :: Int -> (Int -> Int) -> Int
fibCPS 0 k = k 0
fibCPS 1 k = k 1
fibCPS n k = fibCPS (n-1) (\v1 -> fibCPS (n-2) (\v2 -> k (v1 + v2)))

-- An interpreter for our little ArithExpr language

data ArithExpr = Plus ArithExpr ArithExpr
               | Minus ArithExpr ArithExpr
               | Times ArithExpr ArithExpr
               | IfZero ArithExpr ArithExpr ArithExpr
               | Leaf Int


interp :: ArithExpr -> Int
interp (Leaf n) = n
interp (Plus e1 e2) = interp e1 + interp e2
interp (Minus e1 e2) = interp e1 - interp e2
interp (Times e1 e2) = interp e1 * interp e2
interp (IfZero e1 e2 e3) = if interp e1 == 0 then interp e2 else interp e3

-- (3 + 4) - 7
ourExpr :: ArithExpr
ourExpr = Minus (Plus (Leaf 3) (Leaf 4)) (Leaf 7)

-- Let's write a pretty-printer for ArithExprs.

prettyPrint :: ArithExpr -> String
prettyPrint (Leaf n) = printf "%d" n
prettyPrint (Plus e1 e2) = printf "(%s+%s)" (prettyPrint e1) (prettyPrint e2)
prettyPrint (Minus e1 e2) = printf "(%s-%s)" (prettyPrint e1) (prettyPrint e2)
prettyPrint (Times e1 e2) = printf "(%s*%s)" (prettyPrint e1) (prettyPrint e2)
prettyPrint (IfZero e1 e2 e3) = printf "if %s == 0 then %s else %s" (prettyPrint e1) (prettyPrint e2) (prettyPrint e3)

-- An AST type for lambda calculus

-- variables -- x, y, z, ...
-- lambda abstractions -- \x -> e
-- applications -- e1 e2

data LCExpr = LCVar String 
            | LCLam String LCExpr 
            | LCApp LCExpr LCExpr

freeVars :: LCExpr -> Set String
freeVars (LCVar s) = singleton s
freeVars (LCLam s e) = freeVars e `difference` singleton s
freeVars (LCApp e1 e2) = freeVars e1 `union` freeVars e2

-- \x -> x
identity :: LCExpr
identity = LCLam "x" (LCVar "x")

-- \x -> \y -> z
example2 :: LCExpr
example2 = LCLam "x" (LCLam "y" (LCVar "z"))

-- (\x -> \y -> z) (\q -> q)
example3 :: LCExpr
example3 = LCApp example2 (LCLam "q" (LCVar "q")) 

-- (\x -> \y -> x) (\q -> q) (\x -> \y -> y)
-- Remember: f g h is sugar for (f g) h
example4 :: LCExpr
example4 = LCApp (LCApp (LCLam "x" (LCLam "y" (LCVar "x"))) (LCLam "q" (LCVar "q"))) (LCLam "x" (LCLam "y" (LCVar "y")))