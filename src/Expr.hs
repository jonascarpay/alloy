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
    Run (RVal Expr Expr Expr Expr Expr a)
  deriving (Functor, Foldable, Traversable)

data Place val a
  = Place a
  | Deref (val a)
  deriving stock (Functor, Foldable, Traversable, Generic, Generic1)
  deriving anyclass (Hashable1)

data RVal typ plc val lbl fun a
  = RBin BinOp (val a) (val a)
  | Block (Scope Label (Prog typ plc val lbl fun) a)
  | Call (fun a) [val a]
  | PlaceVal (plc a)
  deriving stock (Functor, Foldable, Traversable, Generic, Generic1)
  deriving anyclass (Hashable1)

data Label = Label
  deriving stock (Generic)
  deriving anyclass (Hashable)

data RVar = RVar
  deriving stock (Generic)
  deriving anyclass (Hashable)

data Prog typ plc val lbl fun a
  = Decl
      (typ a)
      (val a)
      (Scope RVar (Prog typ plc val lbl fun) a)
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
  pure = Pure
  (<*>) = ap

instance Monad Expr where
  Pure a >>= f = f a
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
bindVal f (Block (Scope p)) = Block (Scope $ bindProg (traverse f) p)
bindVal f (Call fn vs) = Call (fn >>= f) ((>>= f) <$> vs)
bindVal f (PlaceVal p) = PlaceVal (p >>= f)

bindProg ::
  Monad f =>
  (a -> f b) ->
  Prog f f f f f a ->
  Prog f f f f f b
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

-- Mainly acts as proof that scoping an entire prog is equivalent to scoping all subexpressions
-- TODO potentially simplify, combine/rewrite hoistScope'
unscopeProg ::
  (Functor t, Functor p, Functor v, Functor l, Functor f) =>
  Scope b (Prog t p v l f) a ->
  Prog (Scope b t) (Scope b p) (Scope b v) (Scope b l) (Scope b f) a
unscopeProg (Scope (Decl t v k)) = Decl (Scope t) (Scope v) (hoistScope unscopeProg $ swapScope $ Scope k)
unscopeProg (Scope (Assign p v k)) = Assign (Scope p) (Scope v) (unscopeProg $ Scope k)
unscopeProg (Scope (Break l v)) = Break (Scope l) (Scope v)
unscopeProg (Scope (Expr v k)) = Expr (Scope v) (unscopeProg $ Scope k)

scopeProg ::
  (Functor t, Functor p, Functor v, Functor l, Functor f) =>
  Prog (Scope b t) (Scope b p) (Scope b v) (Scope b l) (Scope b f) a ->
  Scope b (Prog t p v l f) a
scopeProg (Decl (Scope t) (Scope v) k) = Scope $ Decl t v (unscope $ swapScope $ hoistScope scopeProg k)
scopeProg (Assign (Scope p) (Scope v) k) = Scope $ Assign p v (unscope $ scopeProg k)
scopeProg (Break (Scope l) (Scope v)) = Scope $ Break l v
scopeProg (Expr (Scope v) k) = Scope $ Expr v (unscope $ scopeProg k)

unscopeVal ::
  (Functor t, Functor p, Functor v, Functor l, Functor f) =>
  Scope b (RVal t p v l f) a ->
  RVal (Scope b t) (Scope b p) (Scope b v) (Scope b l) (Scope b f) a
unscopeVal (Scope (RBin op l r)) = RBin op (Scope l) (Scope r)
unscopeVal (Scope (Block blk)) = Block (hoistScope unscopeProg $ swapScope $ Scope blk)
unscopeVal (Scope (Call fn args)) = Call (Scope fn) (Scope <$> args)
unscopeVal (Scope (PlaceVal plc)) = PlaceVal (Scope plc)

scopeVal ::
  (Functor t, Functor p, Functor v, Functor l, Functor f) =>
  RVal (Scope b t) (Scope b p) (Scope b v) (Scope b l) (Scope b f) a ->
  Scope b (RVal t p v l f) a
scopeVal (RBin op (Scope l) (Scope r)) = Scope $ RBin op l r
scopeVal (Block blk) = Scope $ Block $ unscope $ swapScope $ hoistScope scopeProg blk
scopeVal (Call (Scope fn) args) = Scope $ Call fn (unscope <$> args)
scopeVal (PlaceVal (Scope plc)) = Scope $ PlaceVal plc

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
