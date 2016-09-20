module LogicInterp where

import LogicExpr.Abs
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe

type VarState a = State (Map.Map Ident Bool)  a

p = EVar (Ident "p")
q = EVar (Ident "q")

showExpr :: Expr -> String
showExpr ELitTrue   = "True"
showExpr ELitFalse  = "False"
showExpr (EVar (Ident s))  = s
showExpr (EAnd  p q) = showExpr p ++ " & "  ++ showExpr q
showExpr (EOr   p q) = showExpr p ++ " | "  ++ showExpr q
showExpr (EImp  p q) = showExpr p ++ " -> " ++ showExpr q
showExpr (ENot  p)   = case p of
  ELitTrue -> "!" ++ showExpr p
  ELitFalse -> "!" ++ showExpr p
  EVar (Ident s) -> "!" ++  s
  expr -> "!(" ++ showExpr p ++ ")"

eval :: Expr -> VarState Bool
eval ELitTrue  = return True
eval ELitFalse = return False
eval (EVar v) = do
  mp <- get
  return $ fromJust $ Map.lookup v mp
