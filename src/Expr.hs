{-# LANGUAGE DeriveTraversable #-}

module Expr where

import Data.Map (Map)

type Name = String

data ExprF f
  = Var Name
  | App f f
  | Lam Name f
  | Lit Int
  | Add f f
  deriving (Eq, Show)

data ValueF val
  = VInt Int
  | VClosure Name Expr (Map Name val)
  deriving (Functor, Foldable, Traversable)

--TODO Closure Args (p -> m r)

newtype Fix f = Fix {unFix :: f (Fix f)}

instance Show (ValueF val) where
  show (VInt n) = show n
  show VClosure {} = "<<closure>>"

type Expr = Fix ExprF

type Value = Fix ValueF
