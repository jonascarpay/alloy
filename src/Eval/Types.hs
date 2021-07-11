{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval.Types where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Hashable
import Data.IORef
import Data.Map (Map)
import Data.Void
import Expr
import GHC.Generics

-- VVar and VLbl can never escape the scope in which they're defined, because they can only occur after a definer in a runtime expression, and so the surrounding context must evaluate to a runtime expression.
-- However, this definition makes it seem like we can have free-floating vars/lbls, similar to how a normal value can escape its lexical scope through closures.
-- Maybe there is some type-level way to make clearer that that is not possible in this case.
-- In any case, it needs to be stressed that for these, their dynamc scope is their lexical scope.
data Value f
  = VClosure Name Expr
  | VRun Deps (RTVal VarIX BlockIX FuncIX)
  | VFunc Deps FuncIX
  | VType Type
  | VVar VarIX
  | VBlk BlockIX
  | VAttr (Map Name f)

newtype Hash = Hash Int
  deriving newtype (Eq, Ord, Hashable)

newtype Deps = Deps (Map Hash (RTFunc Hash))
  deriving newtype (Semigroup, Monoid)

newtype Thunk = Thunk (IORef (Either (EvalBase Lazy) Lazy))

type Lazy = Value Thunk

newtype VarIX = VarIX Int
  deriving newtype (Eq, Enum)

newtype BlockIX = BlockIX Int
  deriving newtype (Eq, Enum)

newtype FuncIX = FuncIX Int

data Bind b a
  = Bound b
  | Free a
  deriving stock (Functor, Foldable, Traversable, Generic)
  deriving anyclass (Hashable)

data RTProg var blk fun
  = Decl Type (RTVal var blk fun) (RTProg (Bind () var) blk fun)
  | Assign (RTPlace var blk fun) (RTVal var blk fun) (RTProg var blk fun)
  | Break blk (RTVal var blk fun)
  | ExprStmt (RTVal var blk fun) (RTProg var blk fun)
  deriving stock (Functor, Foldable, Traversable, Generic)
  deriving anyclass (Hashable)

data RTVal var blk fun
  = RBin BinOp (RTVal var blk fun) (RTVal var blk fun)
  | Call fun [RTVal var blk fun]
  | PlaceVal (RTPlace var blk fun)
  | Block (RTProg var (Bind () blk) fun)
  deriving stock (Functor, Foldable, Traversable, Generic)
  deriving anyclass (Hashable)

data RTPlace var blk fun
  = Place var
  | Deref (RTVal var blk fun)
  deriving stock (Functor, Foldable, Traversable, Generic)
  deriving anyclass (Hashable)

data RTFunc fun = RTFunc
  { fnArgs :: [Type],
    fnRet :: Type,
    fnBody :: RTVal (Bind Int Void) Void fun
  }
  deriving stock (Functor, Foldable, Traversable, Generic)
  deriving anyclass (Hashable)

newtype EvalBase a = EvalBase (ExceptT String IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadError String)

-- newtype Eval a = Eval {unEval :: ReaderT EvalEnv EvalBase a}
--   deriving newtype (Functor, Applicative, Monad, MonadIO, MonadError String, MonadReader EvalEnv)
type Eval = ReaderT EvalEnv EvalBase

type Comp = WriterT Deps Eval

data EvalEnv = EvalEnv
  { _binds :: Map Name Thunk,
    _varSource :: VarIX,
    _blkSource :: BlockIX
  }
