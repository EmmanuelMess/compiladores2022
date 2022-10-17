{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Bytecompile
Description : Compila a bytecode. Ejecuta bytecode.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

Este módulo permite compilar módulos a la Macchina. También provee
una implementación de la Macchina para ejecutar el bytecode.
-}
module Bytecompile
  (Bytecode, runBC, bcWrite, bcRead, bytecompileModule, showBC)
 where

import Lang
import Eval ( semOp )
import Subst
import MonadFD4

import Control.Arrow

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as B
import Data.Binary ( Word8, Binary(put, get), decode, encode )
import Data.Binary.Put ( putWord8 )
import Data.Binary.Get ( getWord8, isEmpty )

import Data.List (intercalate)
import Data.Text.Encoding
import Data.Char
import qualified Data.Text as T

type Opcode = Word8
type Bytecode = [Word8]

newtype Bytecode8 = BC { un8 :: [Word8] }

{- Esta instancia explica como codificar y decodificar Bytecode de 8 bits -}
instance Binary Bytecode8 where
  put (BC bs) = mapM_ putWord8 bs
  get = go
    where go =
           do
            empty <- isEmpty
            if empty
              then return $ BC []
              else do x <- getWord8
                      BC xs <- go
                      return $ BC (x:xs)

{- Estos sinónimos de patrón nos permiten escribir y hacer
pattern-matching sobre el nombre de la operación en lugar del código
entero, por ejemplo:

   f (CALL : cs) = ...

 Notar que si hubieramos escrito algo como
   call = 5
 no podríamos hacer pattern-matching con `call`.

 En lo posible, usar estos códigos exactos para poder ejectutar un
 mismo bytecode compilado en distintas implementaciones de la máquina.
-}
pattern NULL     = 0
pattern RETURN   = 1
pattern CONST    = 2
pattern ACCESS   = 3
pattern FUNCTION = 4
pattern CALL     = 5
pattern ADD      = 6
pattern SUB      = 7
pattern FIX      = 9
pattern STOP     = 10
pattern SHIFT    = 11
pattern DROP     = 12
pattern PRINT    = 13
pattern PRINTN   = 14
pattern JUMP     = 15
pattern TAILCALL = 16
pattern IFZ      = 17

--función util para debugging: muestra el Bytecode de forma más legible.
showOps :: Bytecode -> [String]
showOps [] = []
showOps (NULL:xs)        = "NULL" : showOps xs
showOps (RETURN:xs)      = "RETURN" : showOps xs
showOps (CONST:i:xs)     = ("CONST " ++  show i) : showOps xs
showOps (ACCESS:i:xs)    = ("ACCESS " ++ show i) : showOps xs
showOps (FUNCTION:i:xs)  = ("FUNCTION len=" ++ show i) : showOps xs
showOps (CALL:xs)        = "CALL" : showOps xs
showOps (ADD:xs)         = "ADD" : showOps xs
showOps (SUB:xs)         = "SUB" : showOps xs
showOps (FIX:xs)         = "FIX" : showOps xs
showOps (STOP:xs)        = "STOP" : showOps xs
showOps (JUMP:i:xs)      = ("JUMP off=" ++ show i) : showOps xs
showOps (SHIFT:xs)       = "SHIFT" : showOps xs
showOps (DROP:xs)        = "DROP" : showOps xs
showOps (PRINT:xs)       = let (msg,_:rest) = span (/=NULL) xs
                           in ("PRINT " ++ show (bc2string msg)) : showOps rest
showOps (PRINTN:xs)      = "PRINTN" : showOps xs
showOps (ADD:xs)         = "ADD" : showOps xs
showOps (IFZ:i:j:xs)     = ("IFZ endif="++(show i)++" endelse="++(show j)) : showOps xs
showOps (x:xs)           = show x : showOps xs

showBC :: Bytecode -> String
showBC = intercalate "; " . showOps

bcc :: MonadFD4 m => TTerm -> m Bytecode
bcc (V _ (Bound i)) = return [ACCESS, fromIntegral i] -- TODO fix truncation
bcc (V _ (Free _)) = undefined
bcc (V _ (Global _)) = undefined
bcc (Const _ (CNat v)) = return [CONST, fromIntegral v] -- TODO fix truncation
bcc (Lam _ f _ (Sc1 t)) =
  do
    t' <- bcc t
    let len = (length t') + 1
    if len > 255
    then failFD4 ("Funcion muy larga: " ++ f ++ "!") -- TODO fix truncation
    else return ([FUNCTION, fromIntegral len]++t'++[RETURN])
bcc (App _ t1 t2) =
  do
    t1' <- bcc t1
    t2' <- bcc t2
    return (t1'++t2'++[CALL])
bcc (Print _ str t) =
  do
    t' <- bcc t
    let serialStr = string2bc str
    return ([PRINT]++serialStr++[NULL]++t'++[PRINTN])
bcc (BinaryOp _ op t1 t2) =
  do
    t1' <- bcc t1
    t2' <- bcc t2

    let opcode = case op of
                   Add -> ADD
                   Sub -> SUB
    return (t1' ++ t2' ++ [opcode])
bcc (Fix _ name _ f _ (Sc2 t)) =
  do
    t' <- bcc t
    let len = (length t') + 1
    if len > 255
    then failFD4 ("Funcion muy larga: " ++ f ++ "!") -- TODO fix truncation
    else return ([FUNCTION, fromIntegral len]++t'++[RETURN, FIX])
bcc (IfZ _ c t1 t2) =
  do
    c' <- bcc c
    t1' <- bcc t1
    t2' <- bcc t2
    let lenIf = length t1'
    let lenElse = length t2'
    if lenIf > 255
    then failFD4 ("Rama if muy larga!") -- TODO fix truncation
    else if lenElse > 255
      then failFD4 ("Rama else muy larga!") -- TODO fix truncation
      else return (c' ++ [IFZ, fromIntegral lenIf, fromIntegral lenElse] ++ t1' ++ t2')
bcc (Let _ n _ t1 (Sc1 t2)) =
  do
    t1' <- bcc t1
    t2' <- bcc t2

    return (t1' ++ [SHIFT] ++ t2' ++ [DROP])

string2bc :: String -> Bytecode
string2bc = B.unpack . encodeUtf8 . T.pack

bc2string :: Bytecode -> String
bc2string = T.unpack . decodeUtf8 . B.pack

bytecompileModule :: MonadFD4 m => Module -> m Bytecode
bytecompileModule [] = undefined
bytecompileModule (x:(_:_)) = undefined
bytecompileModule [(Decl _ _ _ t)] =
  do
    t' <- bcc t
    return (t'++[STOP])

-- | Toma un bytecode, lo codifica y lo escribe un archivo
bcWrite :: Bytecode -> FilePath -> IO ()
bcWrite bs filename = BS.writeFile filename (encode $ BC $ fromIntegral <$> bs)

---------------------------
-- * Ejecución de bytecode
---------------------------

type Env = [Val]

data Val =
    I Const
  | Fun Env Bytecode
  | RA Env Bytecode
  deriving (Eq)

instance Show Val where
  show (I c) = "I ("++show c++")"
  show (Fun e bc) = "Fun ("++showBC bc++")"
  show (RA e bc) = "RA ("++showBC bc++")"

-- | Lee de un archivo y lo decodifica a bytecode
bcRead :: FilePath -> IO Bytecode
bcRead filename = (map fromIntegral <$> un8) . decode <$> BS.readFile filename

runBC :: MonadFD4 m => Bytecode -> m ()
runBC bc = runBC' bc [] []

printState :: MonadFD4 m => String -> Bytecode -> Env -> [Val] -> m ()
printState str bc e s =
  do
    printFD4 $ str
    printFD4 $ "code "++showBC bc
    printFD4 $ "env "++show e
    printFD4 $ "stack "++show s

runBC' :: MonadFD4 m => Bytecode -> Env -> [Val] -> m ()
-- runBC' (NULL:bc) _ _ = undefined
runBC' (RETURN:_) _ (v:(RA e bc):s) = runBC' bc e (v:s)
runBC' (CONST:x:bc) e s = runBC' bc e ((I $ CNat $ fromIntegral x):s)
runBC' (ACCESS:i:bc) e s = runBC' bc e ((e !! (fromIntegral i)):s)
runBC' (FUNCTION:n:bc) e s =
  let
    cf = take (fromIntegral n) bc
    c = drop (fromIntegral n) bc
  in case c of
       (FIX:c') -> let s' = (Fun s' cf):s
                   in runBC' c' e s'
       otherwise -> runBC' c e ((Fun e cf):s)
runBC' (CALL:bc) e (v:(Fun ef cf):s) = runBC' cf (v:ef) ((RA e bc):s)
runBC' (ADD:bc) e ((I (CNat n)):(I (CNat m)):s) = runBC' bc e ((I $ CNat $ (semOp Add m n)):s)
runBC' (SUB:bc) e ((I (CNat n)):(I (CNat m)):s) = runBC' bc e ((I $ CNat $ (semOp Sub m n)):s)
runBC' (FIX:_) _ _ = undefined
runBC' (STOP:_) _ _ = return ()
runBC' (SHIFT:bc) e (v:s) = runBC' bc (v:e) s
runBC' (DROP:bc) (v:e) s = runBC' bc e s
runBC' (PRINT:bc) e s =
  do
    let (str, bc') = splitOn NULL bc
    printFD4 $ bc2string str
    runBC' bc' e s
runBC' (PRINTN:bc) e ((I (CNat n)):s) =
  do
    printFD4 $ show n
    runBC' bc e s
-- runBC' (JUMP:bc) _ _ = undefined
runBC' (IFZ:lenIf:lenElse:bc) e (c:s) =
  let
    lenIf' = fromIntegral lenIf
    lenElse' = fromIntegral lenElse
  in if c == (I $ CNat $ 0)
     then runBC' ((take lenIf' bc)++(drop (lenIf'+lenElse') bc)) e s
     else runBC' (drop lenIf' bc) e s
runBC' bc e s = printState "Failure in VM" bc e s

splitOn :: Eq a => a -> [a] -> ([a], [a])
splitOn b xs = g xs
  where
  g []                 = ([], [])
  g (x:xs) | x==b      = f xs
           | otherwise = first (x:) $ g xs
  f []                 = ([], [])
  f (x:xs) | x==b      = first (x:) $ f xs
           | otherwise = ([], x:xs)
