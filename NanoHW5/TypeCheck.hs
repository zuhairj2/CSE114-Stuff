{-# LANGUAGE FlexibleInstances, OverloadedStrings, BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}

module Language.Nano.TypeCheck where

import Language.Nano.Types
import Language.Nano.Parser

import qualified Data.List as L
import           Text.Printf (printf)  
import           Control.Exception (throw)

--------------------------------------------------------------------------------
typeOfFile :: FilePath -> IO Type
typeOfFile f = parseFile f >>= typeOfExpr

typeOfString :: String -> IO Type
typeOfString s = typeOfExpr (parseString s)

typeOfExpr :: Expr -> IO Type
typeOfExpr e = do
  let (!st, t) = infer initInferState preludeTypes e
  if (length (stSub st)) < 0 then throw (Error ("count Negative: " ++ show (stCnt st)))
  else return t

--------------------------------------------------------------------------------
-- Problem 1: Warm-up
--------------------------------------------------------------------------------

-- | Things that have free type variables
class HasTVars a where
  freeTVars :: a -> [TId]

-- | Type variables of a type
instance HasTVars Type where
  freeTVars :: Type -> [TId]
  freeTVars TInt = []
  freeTVars TBool = []
  freeTVars (TVar a) = [a]
  freeTVars (t1 :=> t2) = L.union (freeTVars t1) (freeTVars t2)
  freeTVars (TList t) = freeTVars t


-- | Free type variables of a poly-type (remove forall-bound vars)
instance HasTVars Poly where
  freeTVars :: Poly -> [TId]
  freeTVars (Mono t) = freeTVars t
  freeTVars (Forall a p) = L.delete a (freeTVars p)

-- | Free type variables of a type environment
instance HasTVars TypeEnv where
  freeTVars :: TypeEnv -> [TId]
  freeTVars gamma   = concat [freeTVars s | (x, s) <- gamma]  
  
-- | Look up a variable in a type environment
lookupVarType :: Id -> TypeEnv -> Poly
lookupVarType x ((y, s) : gamma)
  | x == y    = s
  | otherwise = lookupVarType x gamma
lookupVarType x [] = throw (Error ("unbound variable: " ++ x))

-- | Extend a type environment with a new binding
extendTypeEnv :: Id -> Poly -> TypeEnv -> TypeEnv
extendTypeEnv x s gamma = (x,s) : gamma  

-- | Look up a type variable in a substitution;
--   if not present, return the variable unchanged
lookupTVar :: TId -> Subst -> Type
lookupTVar a [] = TVar a
lookupTVar a ((b,t) : sub)
  | a == b  = t
  | otherwise = lookupTVar a sub

-- | Remove a type variable from a substitution
removeTVar :: TId -> Subst -> Subst
removeTVar a [] = []
removeTVar a ((b,t) : sub)
  | a == b = sub
  | otherwise = (b,t) : removeTVar a sub
     
-- | Things to which type substitutions can be applied
class Substitutable a where
  apply :: Subst -> a -> a
  
-- | Apply substitution to type
instance Substitutable Type where  
  apply :: Subst -> Type -> Type
  apply _ TInt = TInt
  apply _ TBool = TBool 
  apply sub (TVar a) = lookupTVar a sub
  apply sub (t1 :=> t2) = (apply sub t1) :=> (apply sub t2)
  apply sub (TList t) = TList (apply sub t)

-- | Apply substitution to poly-type
instance Substitutable Poly where    
  apply :: Subst -> Poly -> Poly
  apply sub (Mono t) = Mono (apply sub t)
  apply sub (Forall a p) = 
    let sub' = removeTVar a sub
    in Forall a (apply sub' p)

-- | Apply substitution to (all poly-types in) another substitution
instance Substitutable Subst where  
  apply :: Subst -> Subst -> Subst
  apply sub to = zip keys (map (apply sub) vals)
    where
      (keys, vals) = unzip to
      
-- | Apply substitution to a type environment
instance Substitutable TypeEnv where  
  apply :: Subst -> TypeEnv -> TypeEnv
  apply sub gamma = zip keys (map (apply sub) vals)
    where
      (keys, vals) = unzip gamma
      
-- | Extend substitution with a new type assignment
extendSubst :: Subst -> TId -> Type -> Subst
extendSubst sub a t =
  let subNoA      = removeTVar a sub
      t'          = apply subNoA t
      subExtended = apply [(a, t')] subNoA
  in
      (a, t') : subExtended   
--------------------------------------------------------------------------------
-- Problem 2: Unification
--------------------------------------------------------------------------------
      
-- | State of the type inference algorithm      
data InferState = InferState { 
    stSub :: Subst -- ^ current substitution
  , stCnt :: Int   -- ^ number of fresh type variables generated so far
} deriving (Eq,Show)

-- | Initial state: empty substitution; 0 type variables
initInferState = InferState [] 0

-- | Fresh type variable number n
freshTV n = TVar ("a" ++ show n)
    
-- | Extend the current substitution of a state with a new type assignment   
extendState :: InferState -> TId -> Type -> InferState
extendState (InferState sub n) a t = InferState (extendSubst sub a t) n
        
-- | Unify a type variable with a type; 
--   if successful return an updated state, otherwise throw an error
unifyTVar :: InferState -> TId -> Type -> InferState
unifyTVar st a t = 
  case t of
    TVar b | b == a -> st
    _ | a `elem` freeTVars t -> throw (Error ("type error: cannot unify " ++ a ++ " and " ++ show t ++ " (occurs check)"))
    _ -> extendState st a t
    
-- | Unify two types;
--   if successful return an updated state, otherwise throw an error
unify :: InferState -> Type -> Type -> InferState
unify st t1 t2 
  | t1 == t2 = st
unify st TInt TBool = throw (Error ("type error: cannot unify Int and Bool"))
unify st TBool TInt = throw (Error ("type error: cannot unify Bool and Int"))

unify st (TVar a) t2 = unifyTVar st a t2
unify st t1 (TVar b) = unifyTVar st b t1

unify st (arg1 :=> res1) (arg2 :=> res2) = 
  let 
    st1 = unify st arg1 arg2
    sub1  = stSub st1
    res1' = apply sub1 res1
    res2' = apply sub1 res2
    st2 = unify st1 res1' res2'
  in st2

unify st (TList a) (TList b) = unify st a b
unify _ t1 t2 = throw (Error ("type error: cannot unify " ++ show t1 ++ " and " ++ show t2))

--------------------------------------------------------------------------------
--New Helper Func definitions
--------------------------------------------------------------------------------
-- | Produce a fresh type variable and updated InferState
freshTVar :: InferState -> (InferState, Type)
freshTVar (InferState sub n) =
  let newTV  = freshTV n
      st'    = InferState sub (n + 1)
  in (st', newTV)

-- | Instantiate a Poly into a mono-type, updating the stCnt
instantiatePoly :: InferState -> Poly -> (InferState, Type)
instantiatePoly (InferState sub n) poly =
  let (n', t) = instantiate n poly
      st'     = InferState sub n'
  in (st', t)

--------------------------------------------------------------------------------
-- Problem 3: Type Inference
--------------------------------------------------------------------------------    
  
infer :: InferState -> TypeEnv -> Expr -> (InferState, Type)
infer st _   (EInt _)          = (st, TInt)
infer st _   (EBool _)         = (st, TBool)
infer st gamma (EVar x)        = 
  let poly = lookupVarType x gamma
      (st' , t') = instantiatePoly st poly
  in (st' , t')
infer st gamma (ELam x body)   = 
  let (st1, argTy) = freshTVar st
      gamma' = extendTypeEnv x (Mono argTy) gamma
      (st2, bodyTy) = infer st1 gamma' body
      sub2 = stSub st2
      argTy' = apply sub2 argTy
      bodyTy' = apply sub2 bodyTy
  in  (st2, argTy' :=> bodyTy')

infer st gamma (EApp e1 e2)    = 
  let (st1, t1) = infer st gamma e1
      (st2, t2) = infer st1 gamma e2
      (st3, rTy) = freshTVar st2
      st4 = unify st3 t1 (t2:=> rTy)
      sub4 = stSub st4
      rTy' = apply sub4 rTy
  in (st4, rTy')

infer st gamma (ELet x e1 e2)  = 
  let (st1, a) = freshTVar st
      gamma1 = extendTypeEnv x (Mono a) gamma
      (st2, t1) = infer st1 gamma1 e1
      st3 = unify st2 t1 a
      sub3 = stSub st3
      a' = apply sub3 a
      gamma' = apply sub3 gamma
      polyX = generalize gamma' a'
      gamma2 = extendTypeEnv x polyX gamma'
      (st4 ,t2) = infer st3 gamma2 e2
  in 
    (st4, t2)
infer st gamma (EBin op e1 e2) = infer st gamma (EApp (EApp (EVar (show op)) e1) e2)
  where
    asApp = EApp (EApp opVar e1) e2
    opVar = EVar (show op)
infer st gamma (EIf c e1 e2) = infer st gamma (EApp (EApp (EApp (EVar "if") c) e1) e2)
  where
    asApp = EApp (EApp (EApp ifVar c) e1) e2
    ifVar = EVar "if"    
infer st gamma ENil = infer st gamma (EVar "[]")

-- | Generalize type variables inside a type
generalize :: TypeEnv -> Type -> Poly
generalize gamma t = 
  let freeInGamma = freeTVars gamma
      freeInT = freeTVars t
      newBinders = freeInT L.\\ freeInGamma
  in foldr Forall (Mono t) newBinders
    
-- | Instantiate a polymorphic type into a mono-type with fresh type variables
instantiate :: Int -> Poly -> (Int, Type)
instantiate n s = helper n [] s
  where
    helper :: Int -> Subst -> Poly -> (Int, Type)
    helper n sub (Mono t)     = (n, apply sub t)
    helper n sub (Forall a s) = helper (n + 1) ((a, freshTV n):sub) s
      
-- | Types of built-in operators and functions      
preludeTypes :: TypeEnv
preludeTypes =
  [ ("+",    Mono (TInt :=> TInt :=> TInt))
  , ("-",    Mono (TInt :=> TInt :=> TInt))
  , ("*",    Mono (TInt :=> TInt :=> TInt))
  , ("/",    Mono (TInt :=> TInt :=> TInt))
  , ("==",   Forall "a" (Mono (TVar "a" :=> TVar "a" :=> TBool)))
  , ("!=",   Forall "a" (Mono (TVar "a" :=> TVar "a" :=> TBool)))
  , ("<",    Mono (TInt :=> TInt :=> TBool))
  , ("<=",   Mono (TInt :=> TInt :=> TBool))
  , ("&&",   Mono (TBool :=> TBool :=> TBool))
  , ("||",   Mono (TBool :=> TBool :=> TBool))
  , ("if",   Forall "a" (Mono (TBool :=> TVar "a" :=> TVar "a" :=> TVar "a")))
  -- lists: 
  , ("[]",   Forall "a" (Mono (TList (TVar "a"))))
  , (":",    Forall "a" (Mono (TVar "a" :=> TList (TVar "a") :=> TList (TVar "a"))))
  , ("head", Forall "a" (Mono (TList (TVar "a") :=> TVar "a")))
  , ("tail", Forall "a" (Mono (TList (TVar "a") :=> TList (TVar "a"))))
  ]
