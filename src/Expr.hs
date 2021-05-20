{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Expr where

import Control.Monad.Except
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.Hashable
import Data.Map (Map)
import Data.Map qualified as M
import Data.Sequence (Seq)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Lens.Micro.Platform

newtype Fix f = Fix {unFix :: f (Fix f)}

type Name = ByteString

-- TODO Strings aren't prim
data Prim
  = PInt Int
  | PDouble Double
  | PBool Bool
  | PString ByteString
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

data Binding
  = Binding Name [Name] Expr
  | Inherit [Name]
  | InheritFrom Expr [Name]
  deriving (Eq, Show)

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr
  | Let [Binding] Expr
  | Prim Prim
  | Func [(Name, Expr)] Expr Expr
  | List (Seq Expr)
  | BinExpr BinOp Expr Expr
  | Attr [Binding]
  | Acc Name Expr
  | With Expr Expr
  | BlockExpr (Block Name Name (Maybe Expr) Expr)
  | Cond Expr Expr Expr
  deriving (Eq, Show)

data BinOp = ArithOp ArithOp | CompOp CompOp | Concat
  deriving (Eq, Show, Generic)
  deriving anyclass (Hashable)

data ArithOp = Add | Sub | Mul | Div
  deriving (Eq, Show, Generic)
  deriving anyclass (Hashable)

data CompOp = Eq | Neq | Lt | Gt | Geq | Leq
  deriving (Eq, Show, Generic)
  deriving anyclass (Hashable)

data Block var lbl typ expr = Block
  { _blkLabel :: Maybe lbl,
    _blkStmts :: [Stmt var lbl typ expr],
    _blkType :: typ
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

data Stmt var lbl typ expr
  = Return expr
  | Decl var typ expr
  | Assign var expr
  | ExprStmt expr
  | Continue (Maybe lbl) -- TODO Maybe lbl -> lbl
  | Break (Maybe lbl) (Maybe expr) -- TODO Maybe lbl -> lbl
  deriving (Eq, Show, Generic)
  deriving anyclass (Hashable)

data DesugaredBindings = DesugaredBindings
  { _bindSimple :: [(Name, Expr)],
    _bindInherit :: [Name],
    _bindInheritFrom :: [(Expr, [Name])]
  }

makeLenses ''Block
makeLenses ''DesugaredBindings

desugarBinds ::
  [Binding] ->
  Either Name DesugaredBindings
desugarBinds binds = snd <$> execStateT (mapM_ go binds) (mempty, DesugaredBindings [] [] [])
  where
    check name =
      gets (Set.member name . fst) >>= \case
        True -> throwError name
        False -> _1 %= Set.insert name
    go (Binding name args body) = do
      check name
      let expr = foldr Lam body args
      _2 . bindSimple %= ((name, expr) :)
    go (Inherit names) = forM_ names $ \name -> do
      check name
      _2 . bindInherit %= (name :)
    go (InheritFrom expr names) = do
      forM_ names check
      _2 . bindInheritFrom %= ((expr, names) :)

-- go bs is fs [] = pure (bs, is, fs)
-- -- go bs is fs (Binding name args body : t) = check name >> go bs is fs t
-- go bs is fs (Inherit ns : t) = check name >> go bs is fs t
