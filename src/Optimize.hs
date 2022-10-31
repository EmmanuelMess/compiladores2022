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
import Subst ( subst, varChanger )
import Lang
import MonadFD4

letWithPrint :: String
letWithPrint = ""

optimize :: MonadFD4 m => Decl TTerm -> m (Decl TTerm)
optimize (Decl p n ty t) =
  do
    let t1 = deadCodeElimination t
    t2 <- constantFoldingAndPropagation t1
    let t3 = inlineExpansion t2
    t4 <- constantFoldingAndPropagation t3
    return (Decl p n ty t4)
optimize d = return d


inlineExpansion :: TTerm -> TTerm
inlineExpansion t = (expand . inline) t

inline :: TTerm -> TTerm
inline t@(Let p v ty def@(Lam _ _ _ _) (Sc1 t1)) =
  let
    t1' = inline t1
    def' = inline def
  in if doInlineExpansion t1'
     then subst def' (Sc1 t1')
     else Let p v ty def' (Sc1 t1')
inline (Let p v ty def (Sc1 t)) = Let p v ty (inline def) (Sc1 (inline t))
inline (Fix p x xty y yty (Sc2 t)) = Fix p x xty y yty (Sc2 (inline t)) -- TODO Fix
inline t@(V _ _) = t
inline t@(Const _ _) = t
inline (Lam p n ty (Sc1 t)) = Lam p n ty (Sc1 (inline t))
inline (App p l r) = App p (inline l) (inline r)
inline (Print p str t) = Print p str (inline t)
inline (BinaryOp p op t u) = BinaryOp p op (inline t) (inline u)
inline (IfZ p c t e) = IfZ p (inline c) (inline t) (inline e)

expand :: TTerm -> TTerm
expand (App _ l@(Lam _ _ _ (Sc1 t)) r) = subst (expand r) (Sc1 (expand t))
expand (App p l r) = App p (expand l) (expand r)
expand t@(V _ _) = t
expand t@(Const _ _) = t
expand (Lam p n ty (Sc1 t)) = Lam p n ty (Sc1 (expand t))
expand (Print p str t) = Print p str (expand t)
expand (BinaryOp p op t u) = BinaryOp p op (expand t) (expand u)
expand (Fix p x xty y yty (Sc2 t)) = Fix p x xty y yty (Sc2 (expand t))
expand (IfZ p c t e) = IfZ p (expand c) (expand t) (expand e)
expand (Let p n nty y (Sc1 t)) = Let p n nty (expand y) (Sc1 (expand t))

doInlineExpansion :: TTerm -> Bool
doInlineExpansion t = (numberOfCalls 0 t) * (costScore t) <= 60

numberOfCalls :: Int -> TTerm -> Int
numberOfCalls n (V _ (Bound i)) = if n == i then 1 else 0
numberOfCalls n (V _ _) = 0
numberOfCalls n (Lam _ _ _ (Sc1 t)) = numberOfCalls (n+1) t
numberOfCalls n (App _ l r) = (numberOfCalls n l) + (numberOfCalls n r)
numberOfCalls n (Print _ _ t) = (numberOfCalls n t)
numberOfCalls n (BinaryOp _ _ t u) = (numberOfCalls n t) + (numberOfCalls n u)
numberOfCalls n (Fix _ _ _ _ _ (Sc2 t)) = (numberOfCalls (n+2) t)
numberOfCalls n (IfZ _ c t e) = (numberOfCalls n c) + (numberOfCalls n t) + (numberOfCalls n e)
numberOfCalls n (Const _ _) = 0
numberOfCalls n (Let _ _ _ e (Sc1 t)) = (numberOfCalls n e) + (numberOfCalls (n+1) t)

costScore :: TTerm -> Int
costScore (V _ (Bound i)) = 2
costScore (V _ _) = 0
costScore (Lam _ _ _ (Sc1 t)) = 2 + (costScore t) + 1
costScore (App _ l r) = (costScore l) + (costScore r) + 1
costScore (Print _ str t) = (length str) + 1 + 1 + (costScore t) + 1
costScore (BinaryOp _ _ t u) = (costScore t) + (costScore u) + 1
costScore (Fix _ _ _ _ _ (Sc2 t)) = 2 + (costScore t) + 2
costScore (IfZ _ c t e) = (costScore c) + 1 + (costScore t) + 2 + (costScore e)
costScore (Const _ _) = 2
costScore (Let _ _ _ e (Sc1 t)) = (costScore e) + 1 + (costScore t) + 1


deadCodeElimination :: TTerm -> TTerm
deadCodeElimination = removeRedundantLets -- TODO preguntar si eliminamos codigo del in cuando no hay print

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
  if findInLet t
  then Let p n ty e (Sc1 (removeRedundantLets t))
  else if findPrint e
       then Let p letWithPrint ty e (Sc1 (removeRedundantLets (removeOneFromBound t)))
       else removeRedundantLets (removeOneFromBound t)

removeOneFromBound :: TTerm -> TTerm
removeOneFromBound = removeOneFromBound' 0

removeOneFromBound' :: Int ->  TTerm -> TTerm
removeOneFromBound' n (V p (Bound j)) = if j > n then (V p (Bound (j-1))) else (V p (Bound j))
removeOneFromBound' n t@(V _ _) = t
removeOneFromBound' n (Lam a b c (Sc1 t)) = Lam a b c (Sc1 (removeOneFromBound' (n+1) t))
removeOneFromBound' n (App a l r) = App a (removeOneFromBound' n l) (removeOneFromBound' n r)
removeOneFromBound' n (Print a b t) = Print a b (removeOneFromBound' n t)
removeOneFromBound' n (BinaryOp a b t l) = BinaryOp a b (removeOneFromBound' n t) (removeOneFromBound' n l)
removeOneFromBound' n (Fix a b c d e (Sc2 t)) = Fix a b c d e  (Sc2 (removeOneFromBound' (n+2) t))
removeOneFromBound' n (IfZ p c t e) = IfZ p (removeOneFromBound' n c) (removeOneFromBound' n t) (removeOneFromBound' n e)
removeOneFromBound' n t@(Const _ _) = t
removeOneFromBound' n (Let p na ty e (Sc1 t)) = Let p na ty e (Sc1 (removeOneFromBound' (n+1) t))

findInLet :: TTerm -> Bool
findInLet = findInLet' 0

findInLet' :: Int -> TTerm -> Bool
findInLet' n (V _ (Bound i)) = n == i
findInLet' n (V _ _) = False
findInLet' n (Lam _ _ _ (Sc1 t)) = findInLet' n t
findInLet' n (App _ l r) = (findInLet' n l) || (findInLet' (n+1) r)
findInLet' n (Print _ _ t) = (findInLet' n t)
findInLet' n (BinaryOp _ _ t u) = (findInLet' n t) || (findInLet' n u)
findInLet' n (Fix _ _ _ _ _ (Sc2 t)) = (findInLet' (n+2) t)
findInLet' n (IfZ _ c t e) = (findInLet' n c) || (findInLet' n t) || (findInLet' n e)
findInLet' n (Const _ _) = False
findInLet' n (Let _ _ _ e (Sc1 t)) = (findInLet' n e) || (findInLet' (n+1) t)

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
    return (App p l' r')
constantFoldingAndPropagation (Fix p f fty x xty (Sc2 t)) =
  do
    t' <- constantFoldingAndPropagation t
    return (Fix p f fty x xty (Sc2 t'))
constantFoldingAndPropagation (Let p v ty def (Sc1 t)) =
  do
    def' <- constantFoldingAndPropagation def
    t' <- constantFoldingAndPropagation t
    return (optLet (Let p v ty def' (Sc1 t')))
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

optLet :: TTerm -> TTerm
optLet (Let _ _ _ def@(Const _ _) s) = subst def s
optLet (Let _ _ _ def@(V _ _) s) = subst def s
optLet t = t