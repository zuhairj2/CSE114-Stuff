message :: String
message = "Welcome to lecture 15!"

-- Representing expressions
data Expr = EInt Int
          | EBool Bool
          | EVar String
          | EAdd Expr Expr
          | ELet String Expr Expr

-- Representing types
-- Here, `:=>` is just an infix data constructor
-- that takes two arguments
data Type = TInt | TBool | Type :=> Type
  deriving Show

-- A type environment maps variable names to types
type TypeEnv = [(String, Type)]

lookupVarType :: String -> TypeEnv -> Type
lookupVarType x [] = error ("unbound variable: " ++ x)
lookupVarType x ((y,t):rest) = if x == y then t else lookupVarType x rest

extendTypeEnv :: String -> Type -> TypeEnv -> TypeEnv
extendTypeEnv x t gamma = (x,t):gamma

-- A terrible type inferencer
infer :: TypeEnv -> Expr -> Type
infer _ (EInt _) = TInt
infer _ (EBool _) = TBool
infer gamma (EVar x) = lookupVarType x gamma
infer gamma (EAdd e1 e2) = case (infer gamma e1, infer gamma e2) of
    (TInt, TInt) -> TInt
    (_, _)       -> error "ill-typed expression"
-- let x = e1 in e2
infer gamma (ELet x e1 e2) = infer extGamma e2
  where extGamma = extendTypeEnv x (infer gamma e1) gamma


{-
let x = 3 in
  let y  = 4 in
    x + y  
-}
exampleWellTyped :: Expr
exampleWellTyped = ELet "x" (EInt 3) 
  (ELet "y" (EInt 4) (EAdd (EVar "x") (EVar "y")))

{-
let x = True in
  let y  = 4 in
    x + y  
-}
exampleIllTyped :: Expr
exampleIllTyped = ELet "x" (EBool True) 
  (ELet "y" (EInt 4) (EAdd (EVar "x") (EVar "y")))