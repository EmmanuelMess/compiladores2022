{-|
Module      : Optimize
Description : Optimiza declaraciones
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}

module Optimize where

import Data.Maybe

import Eval ( semOp )
import Lang
import MonadFD4

optimize :: MonadFD4 m => Decl TTerm -> m (Decl TTerm)
optimize (Decl p n ty t) =
  do
    t' <- constantFoldingAndPropagation t
    return (Decl p n ty t')
optimize d = return d


constantFoldingAndPropagation :: MonadFD4 m => TTerm -> m TTerm
constantFoldingAndPropagation t@(V _ (Bound _)) =
  do
    return t
constantFoldingAndPropagation t@(V _ (Free _)) = undefined
constantFoldingAndPropagation (V _ (Global n)) =
  do
    t <- lookupDecl n
    let t' = fromJust t
    return t' -- asumo que esta optimizado (del pasado)
constantFoldingAndPropagation t@(Const _ _) =
  do
    return t
constantFoldingAndPropagation (Print p str t) =
  do
    t' <- constantFoldingAndPropagation t
    return (Print p str t')
constantFoldingAndPropagation (IfZ p c t1 t2) =
  do
    c' <- constantFoldingAndPropagation c
    t1' <- constantFoldingAndPropagation t1
    t2' <- constantFoldingAndPropagation t2
    return (IfZ p c' t1' t2') --TODO remove dead code
constantFoldingAndPropagation (Lam p v ty (Sc1 t)) =
  do
    t' <- constantFoldingAndPropagation t
    return (Lam p v ty (Sc1 t'))
constantFoldingAndPropagation (App p l r) =
  do
    l' <- constantFoldingAndPropagation l
    r' <- constantFoldingAndPropagation r
    return (App p l' r') --TODO fix constant functions
constantFoldingAndPropagation (Fix p f fty x xty (Sc2 t)) =
  do
    t' <- constantFoldingAndPropagation t
    return (Fix p f fty x xty (Sc2 t'))
constantFoldingAndPropagation (Let p v ty def (Sc1 t)) =
  do
    def' <- constantFoldingAndPropagation def
    t' <- constantFoldingAndPropagation t
    return (Let p v ty def' (Sc1 t'))
constantFoldingAndPropagation (BinaryOp p op l r) =
  do
    l' <- constantFoldingAndPropagation l
    r' <- constantFoldingAndPropagation r
    let t' = optSum (BinaryOp p op l' r')
    return t'

optSum :: TTerm -> TTerm
optSum (BinaryOp p op (Const _ (CNat l)) (Const _ (CNat r))) =
  let result = semOp op l r
  in Const p (CNat result)
optSum (BinaryOp p Add (BinaryOp p1 Add lr ll) r@(Const _ _)) =
  let
    lr' = optSum lr
    ll' = optSum ll
    l' = optSum (BinaryOp p1 Add r lr')
  in (BinaryOp p Add l' ll')
optSum (BinaryOp p Add l r@(Const _ _)) = BinaryOp p Add r l
optSum (BinaryOp p op l (Const _ (CNat 0))) = l
optSum (BinaryOp p Add (Const _ (CNat 0)) r) = r
optSum t = t