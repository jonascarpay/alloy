{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval.Types where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Data.ByteString (ByteString)
import Data.Hashable
import Data.IORef
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Void
import Expr
import GHC.Generics

-- VVar and VLbl can never escape the scope in which they're defined, because they can only occur after a definer in a runtime expression, and so the surrounding context must evaluate to a runtime expression.
-- However, this definition makes it seem like we can have free-floating vars/lbls, similar to how a normal value can escape its lexical scope through closures.
-- Maybe there is some type-level way to make clearer that that is not possible in this case.
-- Maybe they exist as a different binding in a different transformer, it's hard to really call them thunks in the first place
-- In any case, it needs to be stressed that for these, their dynamc scope is their lexical scope.
-- Another clue is that variables should never be printable as values, which makes any handling outside the evaluator somewhat awkwars.
data Value f
  = VClosure (Thunk -> EvalBase WHNF) -- TODO params
  | VRun Deps (RTVal VarIX BlockIX Hash)
  | VFunc Deps Hash
  | VType Type
  | VPrim Prim
  | VVar VarIX
  | VString ByteString
  | VBlk BlockIX
  | VAttr (Map Name f)
  | VList (Seq f)
  deriving (Functor, Foldable, Traversable)

type WHNF = Value Thunk

newtype NF = NF {unNF :: Value NF}

newtype Hash = Hash Int
  deriving newtype (Eq, Ord, Hashable)

newtype Deps = Deps (Map Hash (RTFunc Hash))
  deriving newtype (Eq, Semigroup, Monoid)

newtype Thunk = Thunk (IORef (Either (EvalBase WHNF) WHNF))

newtype VarIX = VarIX Int
  deriving newtype (Eq, Enum, Hashable)

newtype BlockIX = BlockIX Int
  deriving newtype (Eq, Enum, Hashable)

newtype FuncIX = FuncIX Int

data Bind b a
  = Bound b
  | Free a
  deriving stock (Eq, Functor, Foldable, Traversable, Generic)
  deriving anyclass (Hashable)

data RTProg var blk fun
  = Decl Type (RTVal var blk fun) (RTProg (Bind () var) blk fun)
  | Assign (RTPlace var blk fun) (RTVal var blk fun) (RTProg var blk fun)
  | Break blk (RTVal var blk fun)
  | ExprStmt (RTVal var blk fun) (RTProg var blk fun)
  deriving stock (Eq, Functor, Foldable, Traversable, Generic)
  deriving anyclass (Hashable)

-- TODO Rename to RTExpr
data RTVal var blk fun
  = RTArith ArithOp (RTVal var blk fun) (RTVal var blk fun)
  | RTComp CompOp (RTVal var blk fun) (RTVal var blk fun)
  | RTCond (RTVal var blk fun) (RTVal var blk fun) (RTVal var blk fun)
  | Call fun [RTVal var blk fun]
  | PlaceVal (RTPlace var blk fun)
  | Block (RTProg var (Bind () blk) fun)
  deriving stock (Eq, Functor, Foldable, Traversable, Generic)
  deriving anyclass (Hashable)

data RTPlace var blk fun
  = Place var
  | Deref (RTVal var blk fun)
  deriving stock (Eq, Functor, Foldable, Traversable, Generic)
  deriving anyclass (Hashable)

data RTFunc fun = RTFunc
  { fnArgs :: [Type],
    fnRet :: Type,
    fnBody :: RTVal (Bind Int Void) Void fun
  }
  deriving stock (Eq, Functor, Foldable, Traversable, Generic)
  deriving anyclass (Hashable)

newtype EvalBase a = EvalBase {unEvalBase :: ExceptT String IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFix, MonadError String)

type Eval = ReaderT EvalEnv EvalBase

unEval :: Eval a -> EvalBase a
unEval = flip runReaderT env0
  where
    env0 = EvalEnv mempty (VarIX 0) (BlockIX 0)

type Comp = WriterT Deps Eval

data EvalEnv = EvalEnv
  { _binds :: Map Name Thunk,
    _varSource :: VarIX,
    _blkSource :: BlockIX
  }
