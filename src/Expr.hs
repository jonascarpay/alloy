module Expr where

import Data.Map (Map)

newtype Fix f = Fix {unFix :: f (Fix f)}

type Name = String

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr
  | Lit Int
  | Arith ArithOp Expr Expr
  | Attr (Map Name Expr)
  | Acc Name Expr
  | BlockLit BlockExpr

data ArithOp = Add | Sub | Mul

newtype BlockExpr = BlockExpr {unBlockExpr :: [StmtExpr]}

data StmtExpr
  = Break RTExpr
  | Decl Name RTExpr
  | Assign Name RTExpr

-- TODO Maybe unify app var (num lit)
-- eventually also struct literals in rt
-- maybe just unify with Expr entirely?

data RTExpr
  = RTVar Name
  | RTLit Int
  | RTApp RTExpr RTExpr
  | RTArith ArithOp RTExpr RTExpr
  | RTBlock BlockExpr
