{-|
Module      : UnnameTypes
Description : Reemplaza nombres de tipos por los tipos
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

Reemplaza nombres de tipos por los tipos
-}

module UnnameTypes ( toPureDecl, toPureDecls ) where

import Data.Maybe

import Lang
import Subst
import MonadFD4

unnameSTerm :: MonadFD4 m => STerm -> m STerm
unnameSTerm x@(SV _ _) = return x
unnameSTerm x@(SConst _ _) = return x
unnameSTerm (SLam p (n, ty) t) =
  do
    ty' <- unnameTy p ty
    t' <- unnameSTerm t
    return (SLam p (n, ty') t')
unnameSTerm (SApp p t1 t2) =
  do
    t1' <- unnameSTerm t1
    t2' <- unnameSTerm t2
    return (SApp p  t1' t2')
unnameSTerm (SPrint p str t) =
  do
    t' <- unnameSTerm t
    return (SPrint p str t')
unnameSTerm (SBinaryOp p op t1 t2) =
 do
   t1' <- unnameSTerm t1
   t2' <- unnameSTerm t2
   return (SBinaryOp p op t1' t2')
unnameSTerm (SFix p (n1, ty1) (n2, ty2) t) =
  do
    ty1' <- unnameTy p ty1
    ty2' <- unnameTy p ty2
    t' <- unnameSTerm t
    return (SFix p (n1, ty1') (n2, ty2') t')
unnameSTerm (SIfZ p c t1 t2) =
  do
    c' <- unnameSTerm c
    t1' <- unnameSTerm t1
    t2' <- unnameSTerm t2
    return (SIfZ p c' t1' t2')
unnameSTerm (SLet p (n, ty) t1 t2) =
  do
    ty' <- unnameTy p ty
    t1' <- unnameSTerm t1
    t2' <- unnameSTerm t2
    return (SLet p (n, ty') t1' t2')
unnameSTerm (SSugar (TSugarLam p vs t)) =
  do
    vs' <- mapM (\(v, ty) -> do { ty' <- unnameTy p ty ; return (v, ty') }) vs
    t' <- unnameSTerm t
    return (SSugar (TSugarLam p vs' t'))
unnameSTerm (SSugar (TSugarFix p (v1, ty1) vs t)) =
  do
    ty1' <- unnameTy p ty1
    vs' <- mapM (\(v2, ty2) -> do { ty2' <- unnameTy p ty2 ; return (v2, ty2') }) vs
    t' <- unnameSTerm t
    return (SSugar (TSugarFix p (v1, ty1') vs' t'))
unnameSTerm (SSugar (TSugarLetFun p (v1, vs, ty1) t1 t2)) =
  do
    vs' <- mapM (\(v2, ty2) -> do { ty2' <- unnameTy p ty2 ; return (v2, ty2') }) vs
    ty1' <- unnameTy p ty1
    t1' <- unnameSTerm t1
    t2' <- unnameSTerm t2
    return (SSugar (TSugarLetFun p (v1, vs', ty1') t1' t2'))
unnameSTerm (SSugar (TSugarLetFunRec p (v1, vs, ty1) t1 t2)) =
  do
    vs' <- mapM (\(v2, ty2) -> do { ty2' <- unnameTy p ty2 ; return (v2, ty2') }) vs
    ty1' <- unnameTy p ty1
    t1' <- unnameSTerm t1
    t2' <- unnameSTerm t2
    return (SSugar (TSugarLetFunRec p (v1, vs', ty1') t1' t2'))
unnameSTerm t@(SSugar (TSugarPrint _ _)) = return t

toPureDecl :: MonadFD4 m => Decl STerm -> m (Decl STerm)
toPureDecl (Decl p n ty t) =
  do
    ty' <- unnameTy p ty
    t' <- (unnameSTerm t)
    return (Decl p n ty' t')
toPureDecl (DeclType p n ty) =
  do
    ty' <- unnameTy p ty
    return (DeclType p n ty')

toPureDecls :: MonadFD4 m => [Decl STerm] -> m [Decl STerm]
toPureDecls [] = return []
toPureDecls ((Decl p n ty t):ms) =
  do
    ty' <- unnameTy p ty
    t' <- (unnameSTerm t)
    ms' <- toPureDecls ms
    return ((Decl p n ty' t'):ms')
toPureDecls ((DeclType p n ty):ms) =
  do
    ty' <- unnameTy p ty
    addDecl (DeclType p n ty')
    toPureDecls ms