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
import Subst

import C ( ir2C )

freshName :: StateT Int (Writer [IrDecl]) Name
freshName = do -- TODO make fresh variables relate to actual fd4 code, it is unreadable
    counter <- get
    (modify (+1))
    let freshName = "__" ++ "f" ++ show (counter)
    return freshName

closeIr :: [Name] -> Name -> Ir -> Ir
closeIr freevars closname term =
  let f (name, i) ir = IrLet name IrInt (IrAccess (IrVar closname) IrInt i) ir -- TODO types are wrong in this line
  in foldr f term (zip freevars [1..])

closureConvert :: TTerm -> StateT Int (Writer [IrDecl]) Ir
closureConvert (V _ (Bound i)) =
 do
   name <- freshName
   return $ IrAccess (IrVar name) IrInt i -- TODO types are wrong in this line
closureConvert (V _ (Free n)) = return $ IrVar n
closureConvert (V _ (Global n)) = return $ IrGlobal n
closureConvert (Const _ c) = return $ IrConst c
closureConvert (Lam _ n ty s@(Sc1 t)) =
  do
     newName <- freshName
     let completeName = newName ++ "__" ++ n
     let freevars = freeVars t
     let closureName = completeName ++ "__closure"
     let varName = completeName ++ "__argument"
     t' <- closeIr freevars closureName <$> closureConvert (open varName s)

     newName1 <- freshName
     tell [IrFun newName1 IrInt [(closureName, IrInt), (varName, IrInt)] t'] -- TODO types are wrong in this line

     newName2 <- freshName
     return $ MkClosure newName2 (fmap IrVar freevars)
closureConvert (App _ t1 t2) =
  do
    t1' <- closureConvert t1
    t2' <- closureConvert t2
    name <- freshName
    return $ IrLet name IrInt t1' $ IrCall (IrAccess (IrVar name) IrInt 0) [IrVar name, t2'] IrInt -- TODO types are wrong in this line
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

runCC :: [Decl TTerm] -> [IrDecl]
runCC decls = snd $ runWriter $ runStateT (go decls) 0
              where go [] = return []
                    go ((Decl _ name _ body):decls') =
                      do
                        body' <- closureConvert body
                        let irdecl = IrVal name IrInt body' -- TODO types are wrong in this line
                        tell [irdecl]
                        go decls'

compileC :: [Decl TTerm] -> String
compileC xs = ir2C $ IrDecls $ runCC xs
