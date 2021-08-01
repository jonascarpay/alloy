{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Expr where

import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import Data.Hashable
import Data.Sequence (Seq)
import GHC.Generics

type Name = ShortByteString

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr
  | Prim Prim
  | Func (Maybe Name) [(Name, Expr)] Expr Expr
  | BinExpr BinOp Expr Expr
  | Run (Maybe Name) ProgE
  | Let [Binding] Expr
  | Attr [Binding]
  | String ByteString
  | Acc Expr Name -- TODO rename sel
  | With Expr Expr
  | List (Seq Expr)
  | Cond Expr Expr Expr
  deriving stock (Eq, Show, Generic)

data ProgE
  = DeclE Name Expr Expr ProgE
  | AssignE Expr Expr ProgE
  | BreakE Expr (Maybe Expr)
  | ContinueE Expr
  | ExprE Expr (Maybe ProgE)
  deriving stock (Eq, Show, Generic)

data Binding
  = Simple Name [Name] Expr
  | Inherit [Name]
  | InheritFrom Expr [Name]
  deriving stock (Eq, Show, Generic)

data Prim
  = PInt Int
  | PDouble Double
  | PBool Bool
  | PVoid -- TODO PUnit
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

data BinOp = ArithOp ArithOp | CompOp CompOp
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

data ArithOp = Add | Sub | Mul | Div
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

data CompOp = Eq | Neq | Lt | Gt | Geq | Leq
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)
