module Expr where

newtype Fix f = Fix {unFix :: f (Fix f)}

type Name = String

data ExprF f
  = Var Name
  | App f f
  | Lam Name f
  | Lit Int
  | ASTLit (BlockF f)
  | Add f f

type Expr = Fix ExprF

data ASTF f = Return f

newtype BlockF f = BlockF {unBlockF :: [ASTF f]}

type AST = Fix ASTF
