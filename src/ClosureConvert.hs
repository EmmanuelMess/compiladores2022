{-|
Module      : ClosureConvert
Description : Conversion declausuras y hoisiting
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental
Conversion declausuras y hoisiting
-}

module ClosureConvert where

import Control.Monad.State
import Control.Monad.Writer

import Data.Maybe

import IR
import Lang
import Subst

import C ( ir2C )

freshName :: String -> StateT ([(String, Int)]) (Writer [IrDecl]) Name
freshName str = do -- TODO make fresh variables relate to actual fd4 code, it is unreadable
    counters <- get
    let update c = case (lookup str c) of
                      Nothing -> (str, 0):c
                      Just x -> (str, x+1):(filter ((/=str) . fst) c)
    modify update
    let counter = fromMaybe 0 (lookup str counters)
    return $ str++(show counter)

closeIr :: [Name] -> Name -> Ir -> Ir
closeIr freevars closname term =
  let f (name, i) ir = IrLet name IrInt (IrAccess (IrVar closname) IrInt i) ir -- TODO types are wrong in this line
  in foldr f term (zip freevars [1..])

closureConvert :: TTerm -> StateT ([(String, Int)]) (Writer [IrDecl]) Ir
closureConvert (V (_, ty) (Bound i)) =
 do
   name <- freshName "bound"
   return $ IrAccess (IrVar name) (typeConvert ty) i
closureConvert (V _ (Free n)) = return $ IrVar n
closureConvert (V _ (Global n)) = return $ IrGlobal n
closureConvert (Const _ c) = return $ IrConst c
closureConvert (Lam _ n ty s@(Sc1 t)) = -- TODO fix?
  do
     let freevars = freeVars t
     varName <- freshName "lam"
     let closureName = "clos"++varName
     t' <- closeIr freevars closureName <$> closureConvert (open varName s)

     newName1 <- freshName "fun"
     tell [IrFun newName1 IrInt [(closureName, IrInt), (varName, IrInt)] t'] -- TODO types are wrong in this line

     newName2 <- freshName "clos"
     return $ MkClosure newName2 (fmap IrVar freevars)
closureConvert (App (_, ty) t1 t2) =
  do
    let t1ty = (typeConvert . getTy) t1

    t1' <- closureConvert t1
    t2' <- closureConvert t2

    name <- freshName "app"
    let appResult = IrCall (IrAccess (IrVar name) t1ty 0) [IrVar name, t2'] (typeConvert ty)
    return $ IrLet name t1ty t1' $ appResult
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
runCC decls = snd $ runWriter $ runStateT (go decls) []
              where go [] = return []
                    go ((Decl _ name _ body):decls') =
                      do
                        body' <- closureConvert body
                        let irdecl = IrVal name IrInt body' -- TODO types are wrong in this line
                        tell [irdecl]
                        go decls'

compileC :: [Decl TTerm] -> String
compileC xs = ir2C $ IrDecls $ runCC xs
