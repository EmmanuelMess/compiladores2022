{-|
Module      : CEK
Description : Implementacion de la maquina CEK
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental
Implementacion de la maquina CEK
-}

module CEK where

import Lang
import MonadFD4 ( MonadFD4, lookupDecl, printFD4, failPosFD4 )
import Eval ( semOp )
import Common


data Val =
    Num Const
  | Closure CloseCEK
  deriving (Show)

data CloseCEK =
    ClosureLam Env TTerm TTerm
  | ClosureFix Env TTerm TTerm
  deriving (Show)

type Env = [Val] -- TODO optimizar?

data Frame =
    FPrint String
  | FAppL Env TTerm
  | FAppR CloseCEK
  | FBinaryOpL Env BinaryOp TTerm
  | FBinaryOpR BinaryOp Val
  | FIfZ Env TTerm TTerm
  | FLet Env TTerm
  deriving (Show)

type Kont = [Frame]

search :: MonadFD4 m => TTerm -> Env -> Kont -> m Val
search (Print _ str t) env k =
  do
    let k' = (FPrint str):k
    r <- search t env k'
    return r
search (BinaryOp _ op t1 t2) env k =
  do
    let k' = (FBinaryOpL env op t2):k
    r <- search t1 env k'
    return r
search (IfZ _ c t1 t2) env k =
  do
    let k' = (FIfZ env t1 t2):k
    r <- search c env k'
    return r
search (App _ t1 t2) env k =
  do
    let k' = (FAppL  env t2):k
    r <- search t1 env k'
    return r
search (V i (Free x)) _ _ = undefined --Imposible este pattern
search (V _ (Bound i)) env k = destroy (env !! i) k
search (V i (Global n)) env k =
  do
     val <- lookupDecl n
     case val of
          Just x -> search x env k
          Nothing -> failPosFD4 (fst i) "Frame V error, V undefined"
search (Const _ c) env k = destroy (Num c) k
search l@(Lam _ _ _ (Sc1 t)) env k = destroy (Closure (ClosureLam env t l)) k
search l@(Fix _ _ _ _ _ (Sc2 t)) env k = destroy (Closure (ClosureFix env t l)) k
search l@(Let _ _ _ t (Sc1 t')) env k = search t env ((FLet env t') : k)

destroy :: MonadFD4 m => Val -> Kont -> m Val
destroy v@(Num (CNat n)) ((FPrint str):k) =
  do
    printFD4 (str++show n)
    r <- destroy v k
    return r
destroy n@(Num v) ((FBinaryOpL  env op t):k) =
  do
    r <- search t env ((FBinaryOpR  op n):k)
    return r
destroy (Num (CNat v1)) ((FBinaryOpR op (Num (CNat v2))):k) =
  destroy (Num $ CNat $ semOp op v1 v2) k
destroy (Num (CNat n)) ((FIfZ env t1 t2):k) =
  do
    let t = if n == 0 then t1 else t2
    r <- search t env k
    return r
destroy (Closure closure) ((FAppL env2 t2):k) =
  do
    let k' = (FAppR closure):k
    r <- search t2 env2 k'
    return r
destroy v ((FAppR closure) : k) =
  case closure of
    ClosureLam e tmL _ -> search tmL (v : e) k
    a@(ClosureFix  e tmF _) -> search tmF (v : (Closure a) : e) k
destroy v (FLet e tL : xs) = search tL (v : e) xs
destroy v [] = return v

evalCEK ::  MonadFD4 m => TTerm -> m TTerm
evalCEK t =
  do
    s <- search t [] []
    return $ abrir s

abrir :: Val -> TTerm
abrir (Num c) = Const (NoPos, NatTy) c
abrir (Closure (ClosureLam  _ _ f)) = f
abrir (Closure (ClosureFix _ _ f)) = f
