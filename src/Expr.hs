{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Expr where

import Data.Hashable
import Data.Map (Map)
import Data.Map qualified as M
import Data.Sequence (Seq)
import GHC.Generics (Generic)

newtype Fix f = Fix {unFix :: f (Fix f)}

type Name = String

data Prim
  = PInt Int
  | PDouble Double
  | PBool Bool
  deriving (Eq, Show, Generic)

data Type
  = TInt
  | TDouble
  | TBool
  | TVoid
  | TStruct (Map Name Type)
  deriving (Eq, Show, Ord, Generic)

instance (Hashable a, Hashable b) => Hashable (Map a b) where
  hashWithSalt salt m = hashWithSalt salt (M.toList m)

instance Hashable Type

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
  deriving (Eq, Show, Generic)

instance Hashable ArithOp

newtype Block typ expr = Block
  {blkStatements :: [Stmt typ expr]}
  deriving (Eq, Show, Hashable)

data Stmt typ expr
  = Return expr
  | Decl Name typ expr -- TODO encode that this can only be a type for RTExpr
  | Assign Name expr
  | ExprStmt expr
  deriving (Eq, Show, Generic)

instance (Hashable expr, Hashable typ) => Hashable (Stmt typ expr)
