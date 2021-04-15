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

-- TODO Strings aren't prim
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
  | BlockExpr (Block Name Name (Maybe Expr) Expr)
  | Cond Expr Expr Expr
  deriving (Eq, Show)

data BinOp = ArithOp ArithOp | CompOp CompOp | Concat
  deriving (Eq, Show, Generic)

data ArithOp = Add | Sub | Mul
  deriving (Eq, Show, Generic)

data CompOp = Eq | Neq | Lt | Gt | Geq | Leq
  deriving (Eq, Show, Generic)

instance Hashable BinOp

instance Hashable ArithOp

instance Hashable CompOp

data Block var lbl typ expr = Block
  { _blkLabel :: Maybe lbl,
    _blkStmts :: [Stmt var lbl typ expr],
    _blkType :: typ
  }
  deriving (Eq, Show, Generic)

instance
  (Hashable var, Hashable lbl, Hashable typ, Hashable expr) =>
  Hashable (Block var lbl typ expr)

data Stmt var lbl typ expr
  = Return expr
  | Decl var typ expr
  | Assign var expr
  | ExprStmt expr
  | Continue (Maybe lbl) -- TODO Maybe lbl -> lbl
  | Break (Maybe lbl) (Maybe expr) -- TODO Maybe lbl -> lbl
  deriving (Eq, Show, Generic)

instance
  (Hashable var, Hashable lbl, Hashable expr, Hashable typ) =>
  Hashable (Stmt var lbl typ expr)

makeLenses ''Block
