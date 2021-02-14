module Expr where

import Data.Map (Map)
import Data.Sequence (Seq)

newtype Fix f = Fix {unFix :: f (Fix f)}

type Name = String

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr
  | Lit Int
  | List (Seq Expr)
  | Arith ArithOp Expr Expr
  | Attr (Map Name Expr)
  | Acc Name Expr
  | BlockExpr (Block Expr)
  deriving (Eq, Show)

data ArithOp = Add | Sub | Mul
  deriving (Eq, Show)

newtype Block expr = Block {unBlock :: [Stmt expr]}
  deriving (Eq, Show)

data Stmt expr
  = Break expr
  | Decl Name expr
  | Assign Name expr
  | ExprStmt expr
  deriving (Eq, Show)
