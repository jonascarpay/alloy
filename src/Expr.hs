{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Expr where

import Control.Monad.Except
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.Hashable
import Data.Map (Map)
import Data.Map qualified as M
import Data.Sequence (Seq)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Lens.Micro.Platform
import Lib.Bound

data Expr a
  = Var a
  | App (Expr a) (Expr a)
  | Lam (Scope () Expr a)
  | Let [Binding] (Expr a)
  | Prim Prim
  | Func [(Name, (Expr a))] (Expr a) (Expr a)
  | List (Seq (Expr a))
  | BinExpr BinOp (Expr a) (Expr a)
  | Attr [Binding]
  | Acc Name (Expr a)
  | With (Expr a) (Expr a)
  | BlockExpr (Block Name Name (Maybe (Expr a)) (Expr a))
  | Cond (Expr a) (Expr a) (Expr a)
  deriving (Eq, Show)

newtype Fix f = Fix {unFix :: f (Fix f)}

type Name = ByteString

-- TODO Strings aren't prim
data Prim
  = PInt Int
  | PDouble Double
  | PBool Bool
  | PString ByteString
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

data Type
  = TInt
  | TDouble
  | TBool
  | TVoid
  | TStruct (Map Name Type)
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Hashable)

-- TODO move to orphan module
instance (Hashable a, Hashable b) => Hashable (Map a b) where
  hashWithSalt salt m = hashWithSalt salt (M.toList m)

data Binding
  = Binding Name [Name] Expr
  | Inherit [Name]
  | InheritFrom Expr [Name]
  deriving (Eq, Show)

data BinOp = ArithOp ArithOp | CompOp CompOp | Concat
  deriving (Eq, Show, Generic)
  deriving anyclass (Hashable)

data ArithOp = Add | Sub | Mul | Div
  deriving (Eq, Show, Generic)
  deriving anyclass (Hashable)

data CompOp = Eq | Neq | Lt | Gt | Geq | Leq
  deriving (Eq, Show, Generic)
  deriving anyclass (Hashable)

data Block var lbl typ expr = Block
  { _blkLabel :: Maybe lbl,
    _blkStmts :: [Stmt var lbl typ expr],
    _blkType :: typ
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

data Stmt var lbl typ expr
  = Return expr
  | Decl var typ expr
  | Assign var expr
  | ExprStmt expr
  | Continue (Maybe lbl) -- TODO Maybe lbl -> lbl
  | Break (Maybe lbl) (Maybe expr) -- TODO Maybe lbl -> lbl
  deriving (Eq, Show, Generic)
  deriving anyclass (Hashable)

data DesugaredBindings = DesugaredBindings
  { _bindSimple :: [(Name, Expr)],
    _bindInherit :: [Name],
    _bindInheritFrom :: [(Expr, [Name])]
  }

makeLenses ''Block
makeLenses ''DesugaredBindings
