{-# LANGUAGE DeriveTraversable #-}

module Parser.Tree where

import Data.ByteString (ByteString)
import Data.Sequence (Seq)

type Name = ByteString

data Atom
  = AInt Int
  | ADouble Double
  | ABool Bool
  | AString ByteString
  deriving (Eq, Show)

data PTree
  = Let [Binding] PTree
  | Var Name
  | App PTree PTree
  | Lam Name PTree
  | Atom Atom
  | List (Seq PTree)
  | Cond PTree PTree PTree
  | Attr [Binding]
  | With PTree PTree
  | BinExpr BinOp PTree PTree
  | Acc Name PTree
  | Func [(Name, PTree)] PTree PTree
  | BlockExpr (Block Name Name (Maybe Expr) Expr)
  deriving (Eq, Show)

data BinOp = ArithOp ArithOp | CompOp CompOp | Concat
  deriving (Eq, Show)

data ArithOp = Add | Sub | Mul | Div
  deriving (Eq, Show)

data CompOp = Eq | Neq | Lt | Gt | Geq | Leq
  deriving (Eq, Show)
