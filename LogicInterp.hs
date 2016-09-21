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


sEval :: Map.Map Ident Bool -> Expr -> Bool
sEval m ELitTrue            = True
sEval m ELitFalse           = False
sEval m (EVar (Ident s))    = case Map.lookup (Ident s) m of
  Just a -> a
  Nothing -> error $ "value doesn't exist for " ++ s
sEval m (EAnd a b)  = and [sEval m a, sEval m b]
sEval m (EOr  a b)  = or  [sEval m a, sEval m b]
sEval m (EImp a b)  = case sEval m a of
  True  -> case sEval m b of
    False -> False
    _     -> True
  False -> True
sEval m (ENot a)    = not (sEval m a)

-- Assign Value
asValue :: Map.Map Ident Bool -> Expr -> Bool -> Map.Map Ident Bool
asValue m (EVar i) b  = Map.insert i b m


-- Not sure we need this anymore

{-
evalStmt :: Map.Map Ident Bool -> Stmt -> Map.Map Ident Bool
evalStmt m (SAssign i e) = case Map.lookup i m of
  Just _ -> error "Value exists for " ++ getter i
  Nothing -> Map.insert i (eval e) m

getter :: Ident -> String
getter (Ident s) = s

eval :: Map.Map Ident Bool -> Expr -> Bool
eval _ ELitTrue = True
eval _ ELitFalse = False
eval m (EVar (Ident s)) = undefined
-}
