message :: String
message = "Welcome to lecture 14!"
quiz = let x = 3 in
  let f = \y -> x + y in
    let x = 5 in
      f x

-- AST Type
data Expr = Arith ArithOp Expr Expr
          | IfZero Expr Expr Expr
          | Leaf Int
          | Var String
          | Let String Expr Expr -- let <var> = <expr> in <body>
          | Lam String Expr -- function definitions
          | App Expr Expr -- function calls
  deriving (Show, Eq)

data ArithOp = Add | Sub | Mul
  deriving (Show, Eq)

data Value = VInt Int
           | VClos String Expr Env
  deriving (Show, Eq)


-- What an env might look like: [("x", VInt 3), ("y", VInt 4), ("f", VClos ...)]
type Env = [(String, Value)]

-- Our interpreter
interp :: Env -> Expr -> Maybe Value
interp _   (Leaf n) = Just (VInt n)
interp env (Var s) = lookup s env
interp env (Arith op e1 e2) = case (interp env e1, interp env e2) of
    (Just v1, Just v2) -> Just (applyOp op v1 v2)
    _                  -> Nothing 
  where applyOp :: ArithOp -> Value -> Value -> Value
        applyOp Add (VInt n1) (VInt n2) = VInt (n1 + n2)
        applyOp Sub (VInt n1) (VInt n2) = VInt (n1 - n2)
        applyOp Mul (VInt n1) (VInt n2) = VInt (n1 * n2)
        applyOp _   _         _         = error "type error!"
interp env (IfZero e1 e2 e3) = case interp env e1 of
    Just (VInt 0) -> interp env e2
    Just (VInt n) -> interp env e3
    _             -> Nothing
-- let <s> = <expr> in <body>    
interp env (Let s expr body) = case interp env expr of
    Just n  -> interp ((s,n):env) body
    Nothing -> Nothing
-- function definitions
interp env (Lam s body) = Just (VClos s body env)
-- function calls
interp env (App e1 e2)  = case (interp env e1, interp env e2) of
    (Just (VClos s body closureEnv), Just v) -> interp ((s,v):closureEnv) body
    (Just (VInt _), _)                       -> error "type error!"
    (_, _)                                   -> Nothing


data Test = Test Expr Env (Maybe Value)

tests :: [Test]
tests = [Test (Arith Add (Leaf 3) (Var "x")) [] Nothing, -- 3 + x
         -- 3 + x in an env where x=4
         Test (Arith Add (Leaf 3) (Var "x")) [("x", VInt 4)] (Just (VInt 7)),
         -- let x = 5 in x + 3
         Test (Let "x" (Leaf 5) (Arith Add (Var "x") (Leaf 3))) [] (Just (VInt 8)),
         -- let's do one that involves a function call!
         -- let x = 5 in let f = \y -> x + y in f 2
         Test (Let "x" (Leaf 5) (Let "f" (Lam "y" (Arith Add (Var "x") (Var "y"))) (App (Var "f") (Leaf 2)))) [] (Just (VInt 7))]

runTests :: [Bool]
runTests = map (\(Test expr env result) -> interp env expr == result) tests