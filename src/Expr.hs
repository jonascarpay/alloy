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
    Run (RVal Expr Expr Expr Expr a)
  deriving (Functor, Foldable, Traversable)

data Place typ var lbl fun a
  = Place (var a)
  | Deref (RVal typ var lbl fun a)
  deriving stock (Functor, Foldable, Traversable, Generic, Generic1)
  deriving anyclass (Hashable1)

data RVal typ var lbl fun a
  = RBin BinOp (RVal typ var lbl fun a) (RVal typ var lbl fun a)
  | Block (Scope Label (Prog typ var lbl fun) a)
  | Call (fun a) [RVal typ var lbl fun a]
  | PlaceVal (Place typ var lbl fun a)
  deriving stock (Functor, Foldable, Traversable, Generic, Generic1)
  deriving anyclass (Hashable1)

data Label = Label
  deriving stock (Generic)
  deriving anyclass (Hashable)

data RVar = RVar
  deriving stock (Generic)
  deriving anyclass (Hashable)

data Prog typ plc lbl fun a
  = Decl
      (typ a)
      (RVal typ plc lbl fun a)
      (Scope RVar (Prog typ plc lbl fun) a)
  | Assign
      (Place typ plc lbl fun a)
      (RVal typ plc lbl fun a)
      (Prog typ plc lbl fun a)
  | Break
      (lbl a)
      (RVal typ plc lbl fun a)
  | Expr
      (RVal typ plc lbl fun a)
      (Prog typ plc lbl fun a)
  deriving stock (Functor, Foldable, Traversable, Generic, Generic1)
  deriving anyclass (Hashable1)

instance Applicative Expr where
  pure = Pure
  (<*>) = ap

instance Monad Expr where
  Pure a >>= f = f a
  App l r >>= f = App (l >>= f) (r >>= f)
  Lam body >>= f = Lam (body >>= lift . f)
  Run run >>= f = Run $ bindA f run
  Func argTypes retType body >>= f = Func ((>>= f) <$> argTypes) (retType >>= f) (body >>= lift . f)
  Type typ >>= _ = Type typ

class ASTT ast where
  bindA :: Monad f => (a -> f b) -> ast f f f f a -> ast f f f f b

  -- Mainly acts as proof that scoping an entire prog is equivalent to scoping all subexpressions
  -- TODO potentially simplify, combine/rewrite hoistScope'
  -- TODO these might be generalizable. MonadTransControl doesn't work directly, since it places Monad constraints on the inner thing
  unscopeA ::
    (Functor t, Functor p, Functor l, Functor f) =>
    Scope b (ast t p l f) a ->
    ast (Scope b t) (Scope b p) (Scope b l) (Scope b f) a
  scopeA ::
    (Functor t, Functor p, Functor l, Functor f) =>
    ast (Scope b t) (Scope b p) (Scope b l) (Scope b f) a ->
    Scope b (ast t p l f) a

instance ASTT Place where
  bindA f (Place p) = Place (p >>= f)
  bindA f (Deref v) = Deref (bindA f v)

  unscopeA (Scope (Place a)) = Place (Scope a)
  unscopeA (Scope (Deref v)) = Deref (unscopeA $ Scope v)

  scopeA (Place (Scope a)) = Scope (Place a)
  scopeA (Deref v) = Scope (Deref (fromScope $ scopeA v))

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> (s -> f t)

instance ASTT RVal where
  bindA f (RBin op l r) = RBin op (bindA f l) (bindA f r)
  bindA f (Block (Scope p)) = Block (Scope $ bindA (traverse f) p)
  bindA f (Call fn vs) = Call (fn >>= f) (bindA f <$> vs)
  bindA f (PlaceVal p) = PlaceVal (bindA f p)

  unscopeA (Scope (RBin op l r)) = RBin op (unscopeA $ Scope l) (unscopeA $ Scope r)
  unscopeA (Scope (Block blk)) = Block (hoistScope unscopeA $ swapScope $ Scope blk)
  unscopeA (Scope (Call fn args)) = Call (Scope fn) (unscopeA . Scope <$> args)
  unscopeA (Scope (PlaceVal plc)) = PlaceVal (unscopeA . Scope $ plc)

  scopeA (RBin op l r) = Scope $ RBin op (fromScope . scopeA $ l) (fromScope . scopeA $ r)
  scopeA (Block blk) = Scope $ Block $ unscope $ swapScope $ hoistScope scopeA blk
  scopeA (Call (Scope fn) args) = Scope $ Call fn (fromScope . scopeA <$> args)
  scopeA (PlaceVal p) = Scope $ PlaceVal (fromScope $ scopeA p)

instance ASTT Prog where
  bindA f (Decl t v (Scope k)) = Decl (t >>= f) (bindA f v) (Scope $ bindA (traverse f) k)
  bindA f (Assign p v k) = Assign (bindA f p) (bindA f v) (bindA f k)
  bindA f (Break l v) = Break (l >>= f) (bindA f v)
  bindA f (Expr v k) = Expr (bindA f v) (bindA f k)

  unscopeA (Scope (Decl t v k)) = Decl (Scope t) (unscopeA $ Scope v) (hoistScope unscopeA $ swapScope $ Scope k)
  unscopeA (Scope (Assign p v k)) = Assign (unscopeA $ Scope p) (unscopeA $ Scope v) (unscopeA $ Scope k)
  unscopeA (Scope (Break l v)) = Break (Scope l) (unscopeA $ Scope v)
  unscopeA (Scope (Expr v k)) = Expr (unscopeA $ Scope v) (unscopeA $ Scope k)

  scopeA (Decl (Scope t) v k) = Scope $ Decl t (fromScope $ scopeA v) (unscope $ swapScope $ hoistScope scopeA k)
  scopeA (Assign p v k) = Scope $ Assign (fromScope $ scopeA p) (fromScope $ scopeA v) (unscope $ scopeA k)
  scopeA (Break (Scope l) v) = Scope $ Break l (fromScope $ scopeA v)
  scopeA (Expr v k) = Scope $ Expr (fromScope $ scopeA v) (unscope $ scopeA k)

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
