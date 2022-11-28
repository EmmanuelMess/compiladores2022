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

freshName :: () -> StateT Int (Writer [IrDecl]) Name
freshName () = do
    counter <- get
    (modify (+1))
    let fresh_name = "__" ++ "f" ++ show (counter)
    return fresh_name

closeIr :: [Name] -> Name -> Ir -> Ir --fijar acceso a var libres
closeIr freevars closname term = foldr (\(arg, i) ir -> IrLet arg (IrAccess (IrVar closname) i) ir) term (zip freevars [1..])

--Falta Hacer el LAM y FIX
closureConvert :: Term -> StateT Int (Writer [IrDecl]) Ir
closureConvert (V _ (Bound i)) = error "Bound en cc" -- Listo
closureConvert (V _ (Free _)) = return $ IrVar name --Listo
closureConvert (V _ (Global _)) = return $ IrGlobal name --Listo
closureConvert (Const _ c) = return $ IrConst c  --Listo
closureConvert (Lam _ n ty (Sc1 t)) = --Listo
  do
     newName <- freshName () ++ "__"++n
     let freevars = freeVars t
            closureName = newName ++ "__closure"
            varName = newName ++ "__argument"
     term' <- closeIr freevars closureName <$> closureConvert (open varName t)
     tell [IrFun freshname [closureName, varName] term']
     return $ MkClosure freshname (fmap IrVar freevars)
closureConvert (App _ t1 t2) = --Listo
  do
    term1' <- closureConvert term1
    term2' <- closureConvert term2
    name <- freshName ()
    return $ IrLet name term1' (IrCall (IrAccess (IrVar name) 0) [IrVar name, term2'])
closureConvert (Print _ str t) = --Listo
  do
    t' <- closureConvert t
    return (IrPrint str t')
closureConvert (BinaryOp _ op t1 t2) = --Listo
  do
    t1' <- closureConvert t1
    t2' <- closureConvert t2
    return (IrBinaryOp op t1' t2')
closureConvert (Fix _ x xty n nty (Sc2 t)) = undefined -- TODO
closureConvert (IfZ _ c t1 t2) = --Listo
  do
    c' <- closureConvert c
    t1' <- closureConvert t1
    t2' <- closureConvert t2
    return (IrIfZ c' t1' t2')
closureConvert (Let _ n ty t1 (Sc1 t2)) = --Listo
  do
    let ty' = typeConvert ty
    t1' <- closureConvert t1
    t2' <- closureConvert t2
    return (IrLet n ty' t1' t2')


--typeConvert :: Ty -> IrTy
--typeConvert (NamedTy _) = undefined
--typeConvert NatTy = IrInt
--typeConvert (FunTy _ _) = IrFunTy

runCC :: [Decl Term Ty] -> [IrDecl]
runCC decls = snd $ runWriter $ runStateT (go decls) 0
              where go [] = return []
                    go ((Decl _ name _ body):decls') = do body' <- closureConvert body
                                                          let irdecl = IrVal name body'
                                                          tell [irdecl]
                                                          go decls'

compileC :: [Decl Term] -> String
compileC xs = ir2C $ IrDecls $ runCC xs
