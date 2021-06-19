{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Expr where

import Control.Monad.Except
import Control.Monad.State
import Data.ByteString.Short (ShortByteString)
import Data.Hashable
import Data.IntMap (IntMap)
import Data.Kind
import Data.Map (Map)
import Data.Map qualified as M
import Data.Sequence (Seq)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Lens.Micro.Platform
import Lib.Bound

type Name = ShortByteString

data Expr a
  = Var a
  | App (Expr a) (Expr a)
  | Lam (Scope () Expr a)
  | -- | Prim Prim
    -- | Func [(Name, Expr a)] (Expr a) (Scope Int Expr a)
    -- | BinExpr BinOp (Expr a) (Expr a)
    Run (RVal Expr Expr Expr Expr Expr a)
  deriving (Functor, Foldable, Traversable)

data Place typ plc val lbl fun a
  = RVar a
  | Deref (val a)
  deriving (Functor, Foldable, Traversable)

data RVal typ plc val lbl fun a
  = RBin BinOp (val a) (val a)
  | Block (Prog typ plc val (Scope () lbl) fun a)
  | Call (fun a) [val a]
  | Place (plc a)
  deriving (Functor, Foldable, Traversable)

data Prog typ plc val lbl fun a
  = Decl
      (typ a)
      (val a)
      (Prog typ (Scope () plc) val lbl fun a)
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
  deriving (Functor, Foldable, Traversable)

instance Applicative Expr where
  pure = Var
  (<*>) = ap

instance Monad Expr where
  Var a >>= f = f a
  App l r >>= f = App (l >>= f) (r >>= f)
  Lam body >>= f = Lam (body >>= lift . f)
  Run run >>= f = Run $ bindVal f f f f f run

bindVal ::
  (Monad typ, Monad plc, Monad val, Monad lbl, Monad fun) =>
  (a -> typ b) ->
  (a -> plc b) ->
  (a -> val b) ->
  (a -> lbl b) ->
  (a -> fun b) ->
  RVal typ plc val lbl fun a ->
  RVal typ plc val lbl fun b
bindVal ft fp fv fl ff = go
  where
    go (RBin op l r) = RBin op (l >>= fv) (r >>= fv)
    go (Block p) = Block (bProg ft fp fv (lift . fl) p)
    go (Call f x) = Call (f >>= ff) ((>>= fv) <$> x)
    go (Place p) = Place (p >>= fp)

bProg ::
  (Monad typ, Monad plc, Monad val, Monad lbl) =>
  (a -> typ b) ->
  (a -> plc b) ->
  (a -> val b) ->
  (a -> lbl b) ->
  Prog typ plc val lbl fun a ->
  Prog typ plc val lbl fun b
bProg ft fp fv fl (Decl t v k) = Decl (t >>= ft) (v >>= fv) (bProg ft (lift . fp) fv fl k)
bProg ft fp fv fl (Assign p v k) = Assign (p >>= fp) (v >>= fv) (bProg ft fp fv fl k)
bProg ft fp fv fl (Expr v k) = Expr (v >>= fv) (bProg ft fp fv fl k)
bProg _ _ fv fl (Break l v) = Break (l >>= fl) (v >>= fv)

-- newtype Fix f = Fix {unFix :: f (Fix f)}

-- TODO Strings aren't prim
data Prim
  = PInt Int
  | PDouble Double
  | PBool Bool
  | PString ShortByteString
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

-- data Type
--   = TInt
--   | TDouble
--   | TBool
--   | TVoid
--   | TStruct (Map Name Type)
--   deriving stock (Eq, Show, Ord, Generic)
--   deriving anyclass (Hashable)

-- -- TODO move to orphan module
-- instance (Hashable a, Hashable b) => Hashable (Map a b) where
--   hashWithSalt salt m = hashWithSalt salt (M.toList m)

data BinOp = ArithOp ArithOp | CompOp CompOp | Concat
  deriving (Eq, Show, Generic)
  deriving anyclass (Hashable)

data ArithOp = Add | Sub | Mul | Div
  deriving (Eq, Show, Generic)
  deriving anyclass (Hashable)

data CompOp = Eq | Neq | Lt | Gt | Geq | Leq
  deriving (Eq, Show, Generic)
  deriving anyclass (Hashable)

-- data Stmt var lbl typ expr
--   = Return expr
--   | Decl var typ expr
--   | Assign var expr
--   | ExprStmt expr
--   | Continue (Maybe lbl) -- TODO Maybe lbl -> lbl
--   | Break (Maybe lbl) (Maybe expr) -- TODO Maybe lbl -> lbl
--   deriving (Eq, Show, Generic)
--   deriving anyclass (Hashable)

-- data DesugaredBindings = DesugaredBindings
--   { _bindSimple :: [(Name, Expr)],
--     _bindInherit :: [Name],
--     _bindInheritFrom :: [(Expr, [Name])]
--   }

-- makeLenses ''Block
-- makeLenses ''DesugaredBindings
