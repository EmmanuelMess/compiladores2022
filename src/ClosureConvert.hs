{-|
Module      : ClosureConvert
Description : Conversion declausuras y hoisiting
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental
Implementacion de la maquina CEK
-}

module ClosureConvert where

import Control.Monad.State
import Control.Monad.Writer

import IR
import Lang


closureConvert :: Term -> StateT Int (Writer [IrDecl]) Ir
closureConvert (V _ (Bound i)) = undefined -- TODO
closureConvert (V _ (Free _)) = undefined
closureConvert (V _ (Global _)) = undefined
closureConvert (Const _ c) = return (IrConst c)
closureConvert (Lam _ n ty (Sc1 t)) = undefined -- TODO
closureConvert (App _ t1 t2) = undefined -- TODO
closureConvert (Print _ str t) =
  do
    t' <- closureConvert t
    return (IrPrint str t')
closureConvert (BinaryOp _ op t1 t2) =
  do
    t1' <- closureConvert t1
    t2' <- closureConvert t2
    return (IrBinaryOp op t1' t2')
closureConvert (Fix _ x xty n nty (Sc2 t)) = undefined -- TODO
closureConvert (IfZ _ c t1 t2) =
  do
    c' <- closureConvert c
    t1' <- closureConvert t1
    t2' <- closureConvert t2
    return (IrIfZ c' t1' t2')
closureConvert (Let _ n ty t1 (Sc1 t2)) =
  do
    let ty' = typeConvert ty
    t1' <- closureConvert t1
    t2' <- closureConvert t2
    return (IrLet n ty' t1' t2')


typeConvert :: Ty -> IrTy
typeConvert (NamedTy _) = undefined
typeConvert NatTy = IrInt
typeConvert (FunTy _ _) = IrFunTy