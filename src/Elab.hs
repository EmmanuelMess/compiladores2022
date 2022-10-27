{-|
Module      : Elab
Description : Elabora un término fully named a uno locally closed.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

Este módulo permite elaborar términos y declaraciones para convertirlas desde
fully named (@STerm) a locally closed (@Term@)
-}

module Elab ( elab, elabDecl ) where

import Lang
import Subst

-- | 'elab' transforma variables ligadas en índices de de Bruijn
-- en un término dado. 
elab :: STerm -> Term
elab = elab' []

elab' :: [Name] -> STerm -> Term
elab' env (SV p v) =
  -- Tenemos que ver si la variable es Global o es un nombre local
  -- En env llevamos la lista de nombres locales.
  if v `elem` env 
    then  V p (Free v)
    else V p (Global v)

elab' _ (SConst p c) = Const p c
elab' env (SLam p (v,ty) t) = Lam p v ty (close v (elab' (v:env) t))
elab' env (SFix p (f,fty) (x,xty) t) = Fix p f fty x xty (close2 f x (elab' (x:f:env) t))
elab' env (SIfZ p c t e)         = IfZ p (elab' env c) (elab' env t) (elab' env e)
-- Operadores binarios
elab' env (SBinaryOp i o t u) = BinaryOp i o (elab' env t) (elab' env u)
-- Operador Print
elab' env (SPrint i str t) = Print i str (elab' env t)
-- Aplicaciones generales
elab' env (SApp p h a) = App p (elab' env h) (elab' env a)
elab' env (SLet p (v,vty) def body) =  
  Let p v vty (elab' env def) (close v (elab' (v:env) body))
elab' env (SSugar (TSugarLam p xs t)) =
  let
    a = map (\(v,ty) -> SLam p (v,ty)) xs
    t' = foldr (\t1 t2 -> t1 t2) t a
  in elab' env t'
elab' env (SSugar (TSugarFix p (f,fty) xs t)) =
  let
    t' = SFix p (f,fty) (head xs) (SSugar (TSugarLam p (tail xs) t))
  in elab' env t'
elab' env (SSugar (TSugarLetFun p (functionName,params,returnType) sugarDef body)) =
  let
    def = SSugar (TSugarLam p params sugarDef)
    vty = foldr FunTy returnType (map snd params)
    v = functionName
  in elab' env (SLet p (v, vty) def body)
elab' env (SSugar (TSugarLetFunRec p (functionName,(v,ty):lamParams,returnType) sugarDef body)) =
  let
    vty = foldr FunTy returnType (map snd lamParams)
    fixty = FunTy ty vty
    def = SFix p (functionName,fixty) (v,ty) (SSugar (TSugarLam p lamParams sugarDef))
  in elab' env (SLet p (functionName,fixty) def body)
elab' env (SSugar (TSugarPrint p str)) =
  let
    v = "$"
  in elab' env (SLam p (v,NatTy) (SPrint p str (SV p v)))


elabDecl :: SDecl STerm -> Decl STerm
elabDecl (SDecl x) = x
elabDecl (SDSugar (DSugarLetFun p (v,xs,ty) t)) =
  let
    tys = map snd xs
    ty' = foldr (FunTy) ty tys
  in Decl p v ty' (SSugar (TSugarLam p xs t))
elabDecl (SDSugar (DSugarLetFunRec p (v,xs,ty) t)) =
  let
    tys = map snd xs
    ty' = foldr (FunTy) ty tys
  in Decl p v ty' (SSugar (TSugarFix p (v,ty') xs t))
