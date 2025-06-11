module Lecture13 where

message :: String
message = (\x -> "Welcome to lecture " ++ x ++ "!") "13"

{-
- Expanding the repertoire of our interpreter: `let`-expressions
- Break/quiz
- Comments on hw2
-}

-- AST Type
data Expr = Arith ArithOp Expr Expr
          | IfZero Expr Expr Expr
          | Leaf Int
          | Var String
          | Let String Expr Expr -- let <var> = <expr> in <body>
  deriving (Show)

data ArithOp = Add | Sub | Mul
  deriving Show


-- What an env might look like: [("x", 3), ("y", 4)]
type Env = [(String, Int)]

-- Our interpreter
interp :: Env -> Expr -> Maybe Int
interp _   (Leaf n) = Just n
interp env (Var s) = lookup s env
interp env (Arith op e1 e2) = case (interp env e1, interp env e2) of
    (Just v1, Just v2) -> Just (applyOp op v1 v2)
    _                  -> Nothing 
  where applyOp :: ArithOp -> Int -> Int -> Int
        applyOp Add n1 n2 = n1 + n2
        applyOp Sub n1 n2 = n1 - n2
        applyOp Mul n1 n2 = n1 * n2
interp env (IfZero e1 e2 e3) = case interp env e1 of
    Just 0 -> interp env e2
    Just n -> interp env e3
    _      -> Nothing
-- let <s> = <expr> in <body>    
interp env (Let s expr body) = case interp env expr of
    Just n  -> interp ((s,n):env) body
    Nothing -> Nothing

{-
- whatever the value of <body> is...
- in an environment that has been *extended* with a binding
  from <var> to the value of <expr>
-}

data Test = Test Expr Env (Maybe Int)

tests :: [Test]
tests = [Test (Arith Add (Leaf 3) (Var "x")) [] Nothing, -- 3 + x
         -- 3 + x in an env where x=4
         Test (Arith Add (Leaf 3) (Var "x")) [("x", 4)] (Just 7),
         -- let x = 5 in x + 3
         Test (Let "x" (Leaf 5) (Arith Add (Var "x") (Leaf 3))) [] (Just 8)]

runTests :: [Bool]
runTests = map (\(Test expr env result) -> interp env expr == result) tests

{-
`let`-expressions

The anatomy of a `let`-expression is

let <var> = <expr> in <body>

The <body> itself is an expression that may contain occurrences of <var>.

-}

exampleLet :: Int
exampleLet = let x = 5
                 y = 4
              in x + y

exampleLet' :: Int
exampleLet' = let x = 5
                  y = x + 2
                in x + y

exampleLet'' :: Int
exampleLet'' = let x = 5
                 in let y = x + 2
                      in x + y

exampleLet''' :: Int
exampleLet''' = let x = 5 in
                  (let x = 6 in
                    x + 2) + (x + 3)

-- quiz = let x = 5 in
--         (let y = x + z in
--           (let z = 10 in y))

quizAST :: Expr
quizAST = 
  Let "x" (Leaf 5) 
    (Let "y" (Arith Add (Var "x") (Var "z")) 
      (Let "z" (Leaf 10) (Var "y")))

{-

The *value* of 

let <var> = <expr> in <body>

should be

- whatever the value of <body> is...
- in an environment that has been *extended* with a binding
  from <var> to the value of <expr>
-}