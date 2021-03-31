{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Expr where

import Data.Hashable
import Data.Map (Map)
import Data.Map qualified as M
import Data.Sequence (Seq)
import GHC.Generics (Generic)
import Lens.Micro.Platform

newtype Fix f = Fix {unFix :: f (Fix f)}

type Name = String

data Prim
  = PInt Int
  | PDouble Double
  | PBool Bool
  | PString String
  deriving (Eq, Show, Generic)

data Type
  = TInt
  | TDouble
  | TBool
  | TVoid
  | TStruct (Map Name Type)
  deriving (Eq, Show, Ord, Generic)

-- TODO move to orphan module
instance (Hashable a, Hashable b) => Hashable (Map a b) where
  hashWithSalt salt m = hashWithSalt salt (M.toList m)

instance Hashable Type

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr
  | Let [(Name, Expr)] Expr
  | Prim Prim
  | Func [(Name, Expr)] Expr Expr
  | List (Seq Expr)
  | BinExpr BinOp Expr Expr
  | Attr (Map Name Expr)
  | Acc Name Expr
  | With Expr Expr
  | BlockExpr (Block (Maybe Expr) Expr)
  | Cond Expr Expr Expr
  deriving (Eq, Show)

data BinOp = ArithOp ArithOp | CompOp CompOp
  deriving (Eq, Show, Generic)

data ArithOp = Add | Sub | Mul
  deriving (Eq, Show, Generic)

data CompOp = Eq | Neq | Lt | Gt | Geq | Leq
  deriving (Eq, Show, Generic)

instance Hashable BinOp

instance Hashable ArithOp

instance Hashable CompOp

data Block typ expr = Block
  { _blkLabel :: Maybe Name,
    _blkStmts :: [Stmt typ expr],
    _blkType :: typ
  }
  deriving (Eq, Show, Generic)

instance (Hashable typ, Hashable expr) => Hashable (Block typ expr)

data Stmt typ expr
  = Return expr
  | Decl Name typ expr -- TODO encode that this can only be a type for RTExpr
  | Assign Name expr
  | ExprStmt expr
  | Continue (Maybe Name)
  | Break (Maybe Name) (Maybe expr)
  deriving (Eq, Show, Generic)

instance (Hashable expr, Hashable typ) => Hashable (Stmt typ expr)

makeLenses ''Block
