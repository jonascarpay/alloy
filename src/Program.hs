{-# LANGUAGE DeriveTraversable #-}

module Program where

import Data.Map (Map)
import Expr

-- essentially just a writer monad, maybe somehow unify
-- TODO More appropriate name since it clases with VClosure
-- maybe this is a module?
-- Only if a is block
data Closure a = Closure
  { functions :: Map Name Function,
    closed :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Semigroup a => Semigroup (Closure a) where
  Closure fns a <> Closure fns' a' = Closure (fns <> fns') (a <> a')

instance Monoid a => Monoid (Closure a) where
  mempty = Closure mempty mempty

instance Applicative Closure where
  pure a = Closure mempty a
  Closure ff f <*> Closure fa a = Closure (ff <> fa) (f a)

instance Monad Closure where
  Closure fa a >>= f = let Closure fb b = f a in Closure (fa <> fb) b

data Module

type Runtime = Block RTExpr

data Function = Function
  deriving (Eq, Show)

data RTExpr
  = RTVar Name
  | RTLit Int
  | RTArith ArithOp RTExpr RTExpr
  | RTBlock (Block RTExpr)
  | RTCall Name [RTExpr]
  deriving (Eq, Show)
