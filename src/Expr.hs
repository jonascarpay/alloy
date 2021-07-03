{-# LANGUAGE DefaultSignatures #-}
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
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Expr where

import Bound.Scope.Simple
import Bound.Var
import Control.Monad.Except
import Data.ByteString.Short (ShortByteString)
import Data.Hashable
import Data.Hashable.Lifted
import Data.Map (Map)
import Data.Map qualified as M
import GHC.Generics

type Name = ShortByteString

data Expr a
  = Pure a
  | App (Expr a) (Expr a)
  | Lam (Scope () Expr a)
  | Func [Expr a] (Expr a) (Scope (Maybe Int) Expr a) -- Nothing is rec call, Just n is arg
  | Type Type
  | -- | Prim Prim
    -- | BinExpr BinOp (Expr a) (Expr a)
    Run (Prog Expr Expr Expr Expr a)
  deriving (Functor, Foldable, Traversable)

data Prog typ plc val lbl a
  = Decl
      (typ a)
      (val a)
      (Scope RVar (Prog typ plc val lbl) a)
  | Assign
      (plc a)
      (val a)
      (Prog typ plc val lbl a)
  | Break
      (lbl a)
      (val a)
  | Expr
      (val a)
      (Prog typ plc val lbl a)
  deriving stock (Functor, Foldable, Traversable, Generic, Generic1)
  deriving anyclass (Hashable1)

data Place val a
  = Place a
  | Deref (val a)
  deriving stock (Functor, Foldable, Traversable, Generic, Generic1)
  deriving anyclass (Hashable1)

data RVal plc fun prg a
  = RBin BinOp (RVal plc fun prg a) (RVal plc fun prg a)
  | Block (Scope Label prg a)
  | Call (fun a) [RVal plc fun prg a]
  | PlaceVal (plc a)
  deriving stock (Functor, Foldable, Traversable, Generic, Generic1)
  deriving anyclass (Hashable1)

data Label = Label
  deriving stock (Generic)
  deriving anyclass (Hashable)

data RVar = RVar
  deriving stock (Generic)
  deriving anyclass (Hashable)

instance Applicative Expr where
  pure = Pure
  (<*>) = ap

instance Monad Expr where
  Pure a >>= f = f a
  App l r >>= f = App (l >>= f) (r >>= f)
  Lam body >>= f = Lam (body >>= lift . f)
  Run prg >>= f = Run $ bindProg f prg
  Func argTypes retType body >>= f = Func ((>>= f) <$> argTypes) (retType >>= f) (body >>= lift . f)
  Type typ >>= _ = Type typ

bindProg :: Monad f => (a -> f b) -> Prog f f f f a -> Prog f f f f b
bindProg f (Decl t v (Scope k)) = Decl (t >>= f) (v >>= f) (Scope $ bindProg (traverse f) k)
bindProg f (Assign p v k) = Assign (p >>= f) (v >>= f) (bindProg f k)
bindProg f (Break l v) = Break (l >>= f) (v >>= f)
bindProg f (Expr v k) = Expr (v >>= f) (bindProg f k)

swapScope :: Functor f => Scope p (Scope q f) a -> Scope q (Scope p f) a
swapScope = hoistScope' . hoistScope' $ fmap sequence

hoistScope' ::
  (f (Var b a) -> f' (Var b' a')) ->
  Scope b f a ->
  Scope b' f' a'
hoistScope' fn (Scope f) = Scope (fn f)

hoistScopeM ::
  Functor m =>
  (f (Var b a) -> m (f' (Var b' a'))) ->
  Scope b f a ->
  m (Scope b' f' a')
hoistScopeM mf (Scope f) = Scope <$> mf f

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

-- TODO move to orphan module
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
