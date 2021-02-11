module Expr where

import Data.Map (Map)

newtype Fix f = Fix {unFix :: f (Fix f)}

type Name = String

data ExprF f
  = Var Name
  | App f f
  | Lam Name f
  | Lit Int
  | Arith ArithOp f f
  | Attr (Map Name f)
  | Acc Name f
  | ASTLit (BlockF f)

data ArithOp = Add | Sub | Mul

type Expr = Fix ExprF

newtype ASTF f = Return f

newtype BlockF f = BlockF {unBlockF :: [ASTF f]}

type AST = Fix ASTF
