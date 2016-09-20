module LogicExpr where

import qualified Data.Map as M
import Data.Maybe

empty :: M.Map String Bool
empty = M.empty


data Expr
  = Lit Bool
  | And  Expr Expr
  | Nand Expr Expr
  | Or   Expr Expr
  | Nor  Expr Expr
  | Imp  Expr Expr
  | Not  Expr
  | Var  Name
  deriving (Eq, Show)

type Name = String

p = Var "p"
q = Var "q"
r = Var "r"

------
showExpr :: Expr -> String
showExpr (Lit  b)   = show b
showExpr (Var  v)   = v
showExpr (And  p q) = showExpr p ++ " & "  ++ showExpr q
showExpr (Or   p q) = showExpr p ++ " | "  ++ showExpr q
showExpr (Imp  p q) = showExpr p ++ " -> " ++ showExpr q
showExpr (Not  p)   = "!(" ++ showExpr p ++ ")"
showExpr (Nand p q) = showExpr (Not (And p q))
showExpr (Nor  p q) = showExpr (Not (Or  p q))


