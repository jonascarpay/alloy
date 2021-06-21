{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Expr where

import Bound.Scope.Simple
import Control.Monad.Except
import Data.ByteString.Short (ShortByteString)
import Data.Hashable
import Data.Hashable.Lifted
import Data.Map (Map)
import Data.Map qualified as M
import GHC.Generics

type Name = ShortByteString

data Expr a
  = Var a
  | App (Expr a) (Expr a)
  | Lam (Scope () Expr a)
  | Func [Expr a] (Expr a) (Scope (Maybe Int) Expr a) -- Nothing is rec call, Just n is arg
  | Type Type
  | -- | Prim Prim
    -- | BinExpr BinOp (Expr a) (Expr a)
    Run (RVal Expr Expr Expr Expr Expr a)
  deriving (Functor, Foldable, Traversable)

data Place val a
  = RVar a
  | Deref (val a)
  deriving stock (Functor, Foldable, Traversable, Generic, Generic1)
  deriving anyclass (Hashable1)

data RVal typ plc val lbl fun a
  = RBin BinOp (val a) (val a)
  | Block (Prog (Scope () typ) (Scope () plc) (Scope () val) (Scope () lbl) (Scope () fun) a)
  | Call (fun a) [val a]
  | Place (plc a)
  deriving stock (Functor, Foldable, Traversable, Generic, Generic1)
  deriving anyclass (Hashable1)

data Prog typ plc val lbl fun a
  = Decl
      (typ a)
      (val a)
      (Prog (Scope () typ) (Scope () plc) (Scope () val) (Scope () lbl) (Scope () fun) a)
  | Assign
      (plc a)
      (val a)
      (Prog typ plc val lbl fun a)
  | Break
      (lbl a)
      (val a)
  | Expr
      (val a)
      (Prog typ plc val lbl fun a)
  deriving stock (Functor, Foldable, Traversable, Generic, Generic1)
  deriving anyclass (Hashable1)

instance Applicative Expr where
  pure = Var
  (<*>) = ap

instance Monad Expr where
  Var a >>= f = f a
  App l r >>= f = App (l >>= f) (r >>= f)
  Lam body >>= f = Lam (body >>= lift . f)
  Run run >>= f = Run $ bindVal f run
  Func argTypes retType body >>= f = Func ((>>= f) <$> argTypes) (retType >>= f) (body >>= lift . f)
  Type typ >>= _ = Type typ

bindVal ::
  Monad f =>
  (a -> f b) ->
  RVal f f f f f a ->
  RVal f f f f f b
bindVal f (RBin op l r) = RBin op (l >>= f) (r >>= f)
bindVal f (Block p) = Block (bindProg (lift . f) p)
bindVal f (Call fn vs) = Call (fn >>= f) ((>>= f) <$> vs)
bindVal f (Place p) = Place (p >>= f)

bindProg ::
  Monad f =>
  (a -> f b) ->
  Prog f f f f f a ->
  Prog f f f f f b
bindProg f (Decl t v k) = Decl (t >>= f) (v >>= f) (bindProg (lift . f) k)
bindProg f (Assign p v k) = Assign (p >>= f) (v >>= f) (bindProg f k)
bindProg f (Break l v) = Break (l >>= f) (v >>= f)
bindProg f (Expr v k) = Expr (v >>= f) (bindProg f k)

hoistProg ::
  (forall a. m a -> n a) ->
  Prog m m m m m b ->
  Prog n n n n n b
hoistProg f (Decl t v k) = Decl (f t) (f v) (hoistProg (hoistScope f) k)
hoistProg f (Assign p v k) = Assign (f p) (f v) (hoistProg f k)
hoistProg f (Break l v) = Break (f l) (f v)
hoistProg f (Expr v k) = Expr (f v) (hoistProg f k)

-- newtype Fix f = Fix {unFix :: f (Fix f)}

-- TODO Strings aren't prim
data Prim
  = PInt Int
  | PDouble Double
  | PBool Bool
  | PString ShortByteString
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

-- deriving anyclass (Hashable)

-- -- TODO move to orphan module
instance (Hashable a, Hashable b) => Hashable (Map a b) where
  hashWithSalt salt m = hashWithSalt salt (M.toList m)

data BinOp = ArithOp ArithOp | CompOp CompOp | Concat
  deriving (Eq, Show, Generic)
  deriving anyclass (Hashable)

data ArithOp = Add | Sub | Mul | Div
  deriving (Eq, Show, Generic)
  deriving anyclass (Hashable)

data CompOp = Eq | Neq | Lt | Gt | Geq | Leq
  deriving (Eq, Show, Generic)
  deriving anyclass (Hashable)
