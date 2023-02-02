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
closureConvert (V (_, ty) (Bound i)) = undefined -- Si llego aca la compilacion esta rota
closureConvert (V _ (Free n)) = return $ IrVar n
closureConvert (V _ (Global n)) = undefined -- Si llego aca la pre compilacion esta rota
closureConvert (Const _ c) = return $ IrConst c
closureConvert (Lam _ n ty s@(Sc1 t)) =
  do
     let freevars = freeVars t

     let envName = "env"++n
     let varName = "var"
     let funName = "fun"++n

     s' <- closureConvert $ open varName s
     let t' = closeIr freevars envName s'
     let tty = termType t

     tell [IrFun funName IrInt [(envName, IrClo), (varName, tty)] t']

     return $ MkClosure funName (fmap IrVar freevars)
closureConvert t@(App _ t1@(Lam _ _ _ _) _) = convertNamedApp t
closureConvert t@(App _ t1@(V _ _) _) = convertNamedApp t
closureConvert (App i t1 t2) =
  do
    funName <- freshName "lam"
    let new = Let i funName (getTy t1) t1 (Sc1 $ App i (V i $ Bound 0) t2)
    closureConvert new
closureConvert (Print _ str t) =
  do
    t' <- closureConvert t
    return (IrPrint str t')
closureConvert (BinaryOp _ op t1 t2) =
  do
    t1' <- closureConvert t1
    t2' <- closureConvert t2
    return (IrBinaryOp op t1' t2')
closureConvert (Fix _ f fty x xty s@(Sc2 t)) =
  do
    let freevars = freeVars t

    let envName = "env"++f
    let varName = "var"
    let funName = "fun"++f

    s' <- closureConvert $ open2 f varName s
    let t' = closeIr freevars envName s'
    let tty = termType t

    tell [IrFun funName IrInt [(envName, IrClo), (varName, tty)] t']

    return $ MkClosure funName (fmap IrVar freevars)
closureConvert (IfZ _ c t1 t2) =
  do
    c' <- closureConvert c
    t1' <- closureConvert t1
    t2' <- closureConvert t2
    return (IrIfZ c' t1' t2')
closureConvert (Let _ n ty@(FunTy _ _) t1 s) =
  do
    let envName = "env"++n

    let ty' = typeConvert ty
    t1' <- closureConvert t1
    t2' <- closureConvert (open n s) -- Se usa la funcion

    return (IrLet envName ty' t1' t2') -- Pero el valor guardado es la clausura
closureConvert (Let _ n ty@(NatTy) t1 s) =
  do
    t1' <- closureConvert t1
    t2' <- closureConvert (open n s)
    return (IrLet n IrInt t1' t2')
closureConvert (Let _ _ (NamedTy _) _ _) = undefined -- Si llego aca el unname types esta roto

convertNamedApp :: TTerm -> StateT ([(String, Int)]) (Writer [IrDecl]) Ir
convertNamedApp (App (_, ty) t1 t2) =
  do
    let n = case t1 of
              (Lam _ name _ _) -> name
              (V _ (Free name)) -> name
              otherwise -> undefined -- No nombrada

    let t1ty = termType t1

    t1' <- closureConvert t1
    t2' <- closureConvert t2

    let funName = "fun"++n
    let envName = "env"++n

    return $ IrCall (IrAccess (IrVar envName) IrFunTy 0) [IrVar envName, t2'] (typeConvert ty)

typeConvert :: Ty -> IrTy
typeConvert (NamedTy _) = undefined
typeConvert NatTy = IrInt
typeConvert (FunTy _ _) = IrClo

termType :: TTerm -> IrTy
termType = typeConvert . getTy

runCC :: Decl TTerm -> [IrDecl]
runCC (Decl _ name _ body) =
 let
   initial =
     do
       body' <- closureConvert body
       let ty = termType body
       let irdecl = IrVal name ty body'
       tell [irdecl]
       return ()
 in snd $ runWriter $ runStateT initial []


compileC :: Decl TTerm -> String
compileC xs = ir2C $ IrDecls $ runCC xs
