{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval.Types where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Hashable
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as M
import Data.Sequence (Seq)
import Data.Void
import Expr
import GHC.Generics

-- VRTPlace and VLbl can never escape the scope in which they're defined, because they can only occur after a definer in a runtime expression, and so the surrounding context must evaluate to a runtime expression.
-- However, this definition makes it seem like we can have free-floating vars/lbls, similar to how a normal value can escape its lexical scope through closures.
-- Maybe there is some type-level way to make clearer that that is not possible in this case.
-- Maybe they exist as a different binding in a different transformer, it's hard to really call them thunks in the first place
-- In any case, it needs to be stressed that for these, their dynamc scope is their lexical scope.
-- Another clue is that variables should never be printable as values, which makes any handling outside the evaluator somewhat awkwars.
data Value f
  = VClosure (Thunk -> EvalBase WHNF) -- TODO params
  | VRTValue Deps (RTValue VarIX BlockIX Hash)
  | VRTPlace Deps (RTPlace VarIX BlockIX Hash)
  | VFunc Deps Hash
  | VType Type
  | VPrim Prim
  | VString ByteString
  | VBlk BlockIX
  | VAttr (Map Name f)
  | VList (Seq f)
  deriving (Functor, Foldable, Traversable)

type WHNF = Value Thunk

newtype NF = NF {unNF :: Value NF}

newtype Hash = Hash Int
  deriving newtype (Eq, Show, Ord, Hashable)

newtype Deps = Deps (HashMap Hash (RTFunc Hash))
  deriving newtype (Eq, Semigroup, Monoid)

newtype Thunk = Thunk (IORef (Either (EvalBase WHNF) WHNF))

newtype VarIX = VarIX Int
  deriving newtype (Eq, Show, Enum, Hashable)

newtype BlockIX = BlockIX Int
  deriving newtype (Eq, Show, Enum, Hashable)

newtype FuncIX = FuncIX Int

-- TODO Since this _only_ describes the implementation, structs with the same
-- types of fields but different names shouldn't count as different values.
-- Ideally we'd find some way to do name-based access, but erase the names at
-- runtime
data Type
  = TInt
  | TDouble
  | TBool
  | TVoid
  | TStruct (Map Name Type)
  deriving stock (Eq, Show, Ord, Generic)

instance Hashable Type where
  hashWithSalt s TInt = hashWithSalt s (0 :: Int)
  hashWithSalt s TDouble = hashWithSalt s (1 :: Int)
  hashWithSalt s TBool = hashWithSalt s (2 :: Int)
  hashWithSalt s TVoid = hashWithSalt s (3 :: Int)
  hashWithSalt s (TStruct m) = hashWithSalt s (4 :: Int, M.toList m)

data Bind b a
  = Bound b
  | Free a
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (Hashable)

data RTProg var blk fun
  = Decl Type (RTValue var blk fun) (RTProg (Bind () var) blk fun)
  | Assign (RTPlace var blk fun) (RTValue var blk fun) (RTProg var blk fun)
  | Break blk (RTValue var blk fun)
  | Continue blk
  | ExprStmt (RTValue var blk fun) (RTProg var blk fun)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (Hashable)

data RTValue var blk fun
  = RTArith ArithOp (RTValue var blk fun) (RTValue var blk fun)
  | RTComp CompOp (RTValue var blk fun) (RTValue var blk fun)
  | RTPrim Prim
  | RTCond (RTValue var blk fun) (RTValue var blk fun) (RTValue var blk fun)
  | Call fun [RTValue var blk fun]
  | PlaceVal (RTPlace var blk fun)
  | Block (RTProg var (Bind () blk) fun)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (Hashable)

data RTPlace var blk fun
  = Place var
  | Deref (RTValue var blk fun)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (Hashable)

data RTFunc fun = RTFunc
  { fnArgs :: [Type],
    fnRet :: Type,
    fnBody :: RTValue (Bind Int Void) Void fun
  }
  deriving stock (Eq, Functor, Foldable, Traversable, Generic)
  deriving anyclass (Hashable)

newtype EvalBase a = EvalBase {unEvalBase :: StateT Int (ExceptT String IO) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFix, MonadError String, MonadFresh)

type Eval = ReaderT EvalEnv EvalBase

type MonadFresh = MonadState Int -- TODO proper class

unEval :: Eval a -> EvalBase a
unEval = flip runReaderT env0
  where
    env0 = EvalEnv mempty

type Comp = WriterT Deps Eval

newtype EvalEnv = EvalEnv {_binds :: Map Name Thunk} -- TODO Just `type`?
