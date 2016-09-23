module LogicInterp where

import LogicExpr.Abs
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe

type VarState a = State (Map.Map Ident Bool)  a

p = EVar (Ident "p")
q = EVar (Ident "q")

exExpr = (EImp (EAnd (ENot p) q) (EOr p (ENot q)))

showExpr :: Expr -> String
showExpr ELitTrue   = "True"
showExpr ELitFalse  = "False"
showExpr (EVar (Ident s))  = s
showExpr (EAnd  a b) = showExpr a ++ " & "  ++ showExpr b
showExpr (EOr   a b) = showExpr a ++ " | "  ++ showExpr b
showExpr (EImp  a b) = showExpr a ++ " -> " ++ showExpr b
showExpr (ENot  a)   = case a of
  ELitTrue       -> "!" ++ showExpr a
  ELitFalse      -> "!" ++ showExpr a
  EVar (Ident s) -> "!" ++  s
  _              -> "!(" ++ showExpr a ++ ")"


sEval :: Map.Map Ident Bool -> Expr -> IO Bool
sEval m ELitTrue            = return True
sEval m ELitFalse           = return False
sEval m (EVar (Ident s))    = case Map.lookup (Ident s) m of
  Just a -> return a
  Nothing -> error $ "value doesn't exist for " ++ s
sEval m (EAnd a b)  = do
  x <- sEval m a
  y <- sEval m b
  return $ and [x,y]
sEval m (EOr  a b)  = do
  x <- sEval m a
  y <- sEval m b
  return $ or [x,y]
sEval m (EImp a b)  = do
  x <- sEval m a
  case x of
    True  -> do
      y <- sEval m b
      case y of 
        False -> return False
        _     -> return True
    False -> return True
sEval m (ENot a)    = do
  x <- sEval m a
  return $ not x

evalStmts :: Map.Map Ident Bool -> [Stmt] -> IO (Map.Map Ident Bool)
evalStmts m []     = return m
evalStmts m (x:xs) = do
  a <- evalStmt m x
  evalStmts a xs

evalStmt :: Map.Map Ident Bool -> Stmt -> IO (Map.Map Ident Bool)
evalStmt m (SAssign (Ident s) e) = case Map.lookup (Ident s) m of
  Just _ -> error $ "Value exists for " ++ s
  Nothing -> do
             x <- sEval m e
             return $ Map.insert (Ident s) x m

-- heh
evalStmt m (SExpr e) = do
  x <- sEval m e
  putStrLn $ concat ["User input: ", showExpr e]
  putStrLn $ show x
  return m

evalProg :: Program -> IO ()
evalProg (Program prog) = do
  evalStmts Map.empty prog
  return ()
