module LogicExpr.Abs where

-- Haskell module generated by the BNF converter


newtype Ident = Ident String deriving (Eq,Ord,Show)

data Expr =
   EImp Expr Expr
 | EAnd Expr Expr
 | EOr Expr Expr
 | ENot Expr
 | ELitTrue
 | ELitFalse
 | EVar Ident
  deriving (Eq,Ord,Show)