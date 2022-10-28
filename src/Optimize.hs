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

letWithPrint :: String
letWithPrint = ""

optimize :: MonadFD4 m => Decl TTerm -> m (Decl TTerm)
optimize (Decl p n ty t) =
  do
    t' <- deadCodeElimination t
    t'' <- constantFoldingAndPropagation t'
    return (Decl p n ty t'')
optimize d = return d


deadCodeElimination :: MonadFD4 m => TTerm -> m TTerm
deadCodeElimination t = return (removeRedundantLets t) -- TODO preguntar si eliminamos codigo del in cuando no hay print

removeRedundantLets :: TTerm -> TTerm
removeRedundantLets t@(V _ _) = t
removeRedundantLets (Lam a b c (Sc1 t)) = Lam a b c (Sc1 (removeRedundantLets t))
removeRedundantLets (App a l r) = App a (removeRedundantLets l) (removeRedundantLets r)
removeRedundantLets (Print a b t) = Print a b (removeRedundantLets t)
removeRedundantLets (BinaryOp a b t u) = BinaryOp a b (removeRedundantLets t) (removeRedundantLets u)
removeRedundantLets (Fix a b c d e (Sc2 t)) = Fix a b c d e  (Sc2 (removeRedundantLets t))
removeRedundantLets (IfZ n c t e) = IfZ n (removeRedundantLets c) (removeRedundantLets t) (removeRedundantLets e)
removeRedundantLets t@(Const _ _) = t
removeRedundantLets (Let p n ty e (Sc1 t)) =
  if findInLet 0 t
  then Let p n ty e (Sc1 (removeRedundantLets t))
  else if findPrint e
       then Let p letWithPrint ty e (Sc1 (removeRedundantLets (removeOneFromBound 0 t)))
       else removeRedundantLets (removeOneFromBound 0 t)

removeOneFromBound :: Int ->  TTerm -> TTerm
removeOneFromBound n (V p (Bound j)) = if j > n then (V p (Bound (j-1))) else (V p (Bound j))
removeOneFromBound n t@(V _ _) = t
removeOneFromBound n (Lam a b c (Sc1 t)) = Lam a b c (Sc1 (removeOneFromBound n t))
removeOneFromBound n (App a l r) = App a (removeOneFromBound n l) (removeOneFromBound n r)
removeOneFromBound n (Print a b t) = Print a b (removeOneFromBound n t)
removeOneFromBound n (BinaryOp a b t l) = BinaryOp a b (removeOneFromBound n t) (removeOneFromBound n l)
removeOneFromBound n (Fix a b c d e (Sc2 t)) = Fix a b c d e  (Sc2 (removeOneFromBound n t))
removeOneFromBound n (IfZ p c t e) = IfZ p (removeOneFromBound n c) (removeOneFromBound n t) (removeOneFromBound n e)
removeOneFromBound n t@(Const _ _) = t
removeOneFromBound n (Let p na ty e (Sc1 t)) = Let p na ty e (Sc1 (removeOneFromBound (n+1) t))

findInLet :: Int -> TTerm -> Bool
findInLet n (V _ (Bound i)) = n == i
findInLet n (V _ _) = False
findInLet n (Lam _ _ _ (Sc1 t)) = findInLet n t
findInLet n (App _ l r) = (findInLet n l) || (findInLet n r)
findInLet n (Print _ _ t) = (findInLet n t)
findInLet n (BinaryOp _ _ t u) = (findInLet n t) || (findInLet n u)
findInLet n (Fix _ _ _ _ _ (Sc2 t)) = (findInLet n t)
findInLet n (IfZ _ c t e) = (findInLet n c) || (findInLet n t) || (findInLet n e)
findInLet n (Const _ _) = False
findInLet n (Let _ _ _ e (Sc1 t)) = (findInLet n e) || (findInLet (n+1) t)

findPrint :: TTerm -> Bool
findPrint (V _ _) = False
findPrint (Const _ _) = False
findPrint (Lam _ _ _ (Sc1 t)) = findPrint t
findPrint (App _ l r) = findPrint l || findPrint r
findPrint (Print _ _ _) = True
findPrint (BinaryOp _ _ t u) = findPrint t || findPrint u
findPrint (IfZ _ c t e) = findPrint c || findPrint t || findPrint e
findPrint (Let _ _ _ t1 (Sc1 t2)) = findPrint t1 || findPrint t2


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
    case c' of
      (Const p (CNat 0)) -> return t1'
      (Const p (CNat _)) -> return t2'
      _                  -> return (IfZ p c' t1' t2')
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