{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Expr where

import Data.Hashable
import Data.Sequence (Seq)
import Data.Text (Text)
import GHC.Generics

type Symbol = Text

data Expr
  = Var Symbol
  | App Expr Expr
  | Lam Symbol Expr
  | Prim Prim
  | Func (Maybe Symbol) [(Symbol, Expr)] Expr Expr
  | BinExpr BinOp Expr Expr
  | Run (Maybe Symbol) ProgE
  | Let [Binding] Expr
  | Attr [Binding]
  | String Text -- TODO better representation for strings
  | Sel Expr Expr
  | With Expr Expr
  | List (Seq Expr)
  | Cond Expr Expr Expr
  | Ref Expr
  | Deref Expr
  deriving stock (Eq, Show, Generic)

data ProgE
  = DeclE Symbol (Maybe Expr) Expr ProgE
  | AssignE Expr Expr ProgE
  | BreakE Expr (Maybe Expr)
  | ContinueE Expr
  | ExprE Expr (Maybe ProgE)
  deriving stock (Eq, Show, Generic)

data Binding
  = Simple Symbol [Symbol] Expr
  | Inherit [Symbol]
  | InheritFrom Expr [Symbol]
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
