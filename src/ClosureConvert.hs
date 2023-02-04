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
freshName str = do
    counter <- gets (\c -> fromMaybe 0 $ lookup str c)
    let update c = case (lookup str c) of
                      Nothing -> (str, 1):c
                      Just x -> (str, x+1):(filter ((/=str) . fst) c)
    modify update
    return $ str++" "++(show counter)

env :: Name -> Name
env = ("env "++)

var :: Name -> Name
var = ("var "++)

fun :: Name -> Name
fun = ("fun "++)

lam :: Name
lam = "lam"

closeIr :: [Name] -> [IrTy] -> Name -> Ir -> Ir
closeIr freeVars freeVarsIrty closureName t =
  let
    f (name, IrClo, i) ir = IrLet (env name) IrClo (IrAccess (IrVar closureName) IrClo i) ir
    f (name, irty, i) ir = IrLet name irty (IrAccess (IrVar closureName) irty i) ir
  in foldr f t (zip3 freeVars freeVarsIrty [1..])

closureConvert :: TTerm -> StateT ([(String, Int)]) (Writer [IrDecl]) Ir
closureConvert (V (_, ty) (Bound i)) = undefined -- Si llego aca la compilacion esta rota
closureConvert (V _ (Free n)) = return $ IrVar n
closureConvert (V _ (Global n)) = undefined -- Si llego aca la pre compilacion esta rota
closureConvert (Const _ c) = return $ IrConst c
closureConvert (Lam _ n ty s@(Sc1 t)) =
  do
    let varsWithType = freeVarsWithType t
    let freeVars = map fst varsWithType
    let freeVarsIrty = map (typeConvert . snd) varsWithType

    let envName = env n -- TODO use actual function name
    let varName = var n
    funName <- freshName $ fun n -- TODO use actual function name

    s' <- closureConvert $ open varName s
    let t' = closeIr freeVars freeVarsIrty envName s'
    let tty = termType t

    tell [IrFun funName (tty) [(envName, IrClo), (varName, typeConvert ty)] t']

    return $ MkClosure funName (fmap IrVar freeVars)
closureConvert t@(App _ t1@(Lam _ _ _ _) _) = convertNamedApp t
closureConvert t@(App _ t1@(V _ _) _) = convertNamedApp t
closureConvert (App (p, ty) t1 t2) =
  do
    let tyt1 = getTy t1
    let tyt2 = getTy t2
    funName <- freshName lam -- Anonima a nombrada
    let new = Let (p, tyt2) funName tyt1 t1 (Sc1 $ App (p, tyt2) (V (p, tyt1) $ Bound 0) t2)
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
    let varsWithType = freeVarsWithType t
    let freeVars = map fst varsWithType
    let freeVarsIrty = map (typeConvert . snd) varsWithType

    let envName = env f  -- TODO use actual function name
    let varName = var x
    funName <- freshName $ fun f  -- TODO use actual function name

    s' <- closureConvert $ open2 envName varName s
    let t' = closeIr freeVars freeVarsIrty envName s'
    let tty = termType t

    tell [IrFun funName IrInt [(envName, IrClo), (varName, tty)] t']

    return $ MkClosure funName (fmap IrVar freeVars)
closureConvert (IfZ _ c t1 t2) =
  do
    c' <- closureConvert c
    t1' <- closureConvert t1
    t2' <- closureConvert t2
    return (IrIfZ c' t1' t2')
closureConvert (Let _ n ty@(FunTy _ _) t1 s) =
  do
    let envName = env n

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

    let envName = env n

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
