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
import MonadFD4 ( MonadFD4, lookupDecl, printFD4 )
import Eval ( semOp )

data Frame =
    FPrint String
  | FBinaryOpA Env BinaryOp TTerm
  | FBinaryOpB BinaryOp Const
  | FIfZ Env TTerm TTerm
  | FKArg Env TTerm
  | FClosureLam Env Var TTerm
  | FClosureFix Env Var Var TTerm


type Kont = [Frame] -- TODO optimizar?

type Env = [Const] -- TODO optimizar?

data Val =
    VConst Const
  | VClosureLam Env Var TTerm
  | VClosureFix Env Var Var TTerm

search :: MonadFD4 m => TTerm -> Env -> Kont -> m Val
search (Print _ str t) env k =
  do
    let k' = (FPrint str):k
    r <- search t env k'
    return r
search (BinaryOp _ op t1 t2) env k =
  do
    let k' = (FBinaryOpA env op t2):k
    r <- search t1 env k'
    return r
search (IfZ _ c t1 t2) env k =
  do
    let k' = (FIfZ env t1 t2):k
    r <- search c env k'
    return r
search (App _ t1 t2) env k =
  do
    let k' = (FKArg env t2):k
    r <- search t1 env k'
    return r
search (V _ (Bound i)) env k =
  do
    let v = VConst (env!!i)
    r <- destroy v k
    return r
search (Const _ c) env k =
  do
    let v = VConst c
    r <- destroy v k
    return r
search (Lam _ var ty (Sc1 t)) env k =
  do
    let v = VClosureLam env 0 t -- TODO correjir de bruijn
    r <- destroy v k
    return r
search (Fix _ nameF _ nameP _ (Sc2 t)) env k =
  do
    let v = VClosureFix env 0 1 t -- TODO correjir de bruijn
    r <- destroy v k
    return r
--TODO let

destroy :: MonadFD4 m => Val -> Kont -> m Val
destroy v@(VConst (CNat n)) ((FPrint str):k) =
  do
    printFD4 (str++show n)
    r <- destroy v k
    return r
destroy (VConst v) ((FBinaryOpA env op t):k) =
  do
    r <- search t env ((FBinaryOpB op v):k)
    return r
destroy (VConst (CNat v1)) ((FBinaryOpB op (CNat v2)):k) =
  do
    return (VConst (CNat (semOp op v1 v2)))
destroy (VConst (CNat n)) ((FIfZ env t1 t2):k) =
  do
    let t = if n == 0 then t1 else t2
    r <- search t env k
    return r
destroy (VClosureLam env1 v t1) ((FKArg env2 t2):k) =
  do
    let k' = (FClosureLam env1 v t1):k
    r <- search t2 env2 k'
    return r
destroy (VClosureFix env1 v1 v2 t1) ((FKArg env2 t2):k) =
  do
    let k' = (FClosureFix env1 v1 v2 t1):k
    r <- search t2 env2 k'
    return r
destroy (VConst n) ((FClosureLam env v t):k) =
  do
    let env' = n:env
    r <- search t env' k
    return r
destroy (VConst n) ((FClosureFix env v1 v2 t):k) =
  do
    let env' = (FClosureFix env v1 v2 t):n:env
    r <- search t env' k
    return r