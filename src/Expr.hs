module Expr where

import Data.Map (Map)
import Data.Sequence (Seq)

newtype Fix f = Fix {unFix :: f (Fix f)}

type Name = String

data Prim
  = PInt Int
  | PDouble Double
  | PBool Bool
  deriving (Eq, Show)

data Type
  = TInt
  | TDouble
  | TBool
  | TVoid
  | TStruct (Map Name Type)
  deriving (Eq, Show)

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr
  | Let [(Name, Expr)] Expr
  | Prim Prim
  | Func [(Name, Expr)] Expr (Block Expr Expr)
  | List (Seq Expr)
  | Arith ArithOp Expr Expr
  | Attr (Map Name Expr)
  | Acc Name Expr
  | BlockExpr (Block Expr Expr)
  deriving (Eq, Show)

data ArithOp = Add | Sub | Mul
  deriving (Eq, Show)

newtype Block typ expr = Block {unBlock :: [Stmt typ expr]}
  deriving (Eq, Show)

data Stmt typ expr
  = Return expr
  | Decl Name typ expr -- TODO encode that this can only be a type for RTExpr
  | Assign Name expr
  | ExprStmt expr
  deriving (Eq, Show)
