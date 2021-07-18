{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Expr where

import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import Data.Hashable
import Data.Map (Map)
import Data.Map qualified as M
import GHC.Generics

type Name = ShortByteString

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr
  | Prim Prim
  | Func [(Name, Expr)] Expr Expr
  | BinExpr BinOp Expr Expr
  | Run (Maybe Name) ProgE
  | Let [Binding] Expr
  | Attr [Binding]
  | String ByteString
  | Acc Expr Name
  | With Expr Expr
  | Cond Expr Expr Expr
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

data ProgE
  = DeclE Name Expr Expr ProgE
  | AssignE Expr Expr ProgE
  | BreakE (Maybe Expr) Expr
  | ExprE Expr ProgE
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

data Binding
  = Simple Name [Name] Expr
  | Inherit [Name]
  | InheritFrom Expr [Name]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

data Prim
  = PInt Int
  | PDouble Double
  | PBool Bool
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

data BinOp = ArithOp ArithOp | CompOp CompOp
  deriving (Eq, Show, Generic)
  deriving anyclass (Hashable)

data ArithOp = Add | Sub | Mul | Div
  deriving (Eq, Show, Generic)
  deriving anyclass (Hashable)

data CompOp = Eq | Neq | Lt | Gt | Geq | Leq
  deriving (Eq, Show, Generic)
  deriving anyclass (Hashable)
