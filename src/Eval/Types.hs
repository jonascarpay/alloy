{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Eval.Types where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Hashable
import Data.Hashable.Lifted
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as M
import Data.Proxy
import Data.Sequence (Seq)
import Data.Set (Set)
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
  | VRTValue Deps (RTValue VarIX BlockIX (Either FuncIX Hash))
  | VRTPlace Deps (RTPlace VarIX BlockIX (Either FuncIX Hash))
  | VFunc Deps (Either FuncIX Hash)
  | VType Type
  | VPrim Prim
  | VString ByteString
  | VBlk BlockIX
  | VAttr (Map Name f)
  | VList (Seq f)
  deriving (Functor, Foldable, Traversable)

type WHNF = Value Thunk

newtype NF = NF {unNF :: Value NF}

newtype Hash = Hash {unHash :: Int}
  deriving newtype (Eq, Show, Ord, Hashable)

data CallGraph = CallGraph
  { cgFunc :: FuncIX,
    cgBody :: RTFunc (Either FuncIX Hash),
    cgOpen :: Set FuncIX,
    cgBind :: Set FuncIX,
    cgDeps :: Set CallGraph
  }
  deriving stock (Show)

data NamelessCG = NamelessCG

instance Eq CallGraph where a == b = cgFunc a == cgFunc b

instance Ord CallGraph where a `compare` b = cgFunc a `compare` cgFunc b

data Deps = Deps
  { closedFuncs :: HashMap Hash (RTFunc Hash),
    openFuncs :: Set CallGraph
  }

instance Semigroup Deps where
  Deps ca oa <> Deps cb ob = Deps (ca <> cb) (oa <> ob)

instance Monoid Deps where mempty = Deps mempty mempty

newtype Thunk = Thunk (IORef (Either (EvalBase WHNF) WHNF))

-- These are not allowed to be hashable.
-- This way, we make sure that only completely nameless terms get hashed
-- TODO These are no longer IX's, but rather IDs
newtype VarIX = VarIX Int
  deriving newtype (Eq, Show, Enum)

newtype BlockIX = BlockIX Int
  deriving newtype (Eq, Show, Enum)

newtype FuncIX = FuncIX {unFuncIX :: Int}
  deriving newtype (Eq, Ord, Show, Enum)

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
  deriving stock (Eq, Show, Ord, Functor, Foldable, Traversable, Generic, Generic1)
  deriving anyclass (Hashable, Hashable1)

data RTProg var blk fun
  = Decl Type (RTValue var blk fun) (RTProg (Bind () var) blk fun)
  | Assign (RTPlace var blk fun) (RTValue var blk fun) (RTProg var blk fun)
  | Break blk (RTValue var blk fun)
  | Continue blk
  | ExprStmt (RTValue var blk fun) (RTProg var blk fun)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving anyclass (Hashable, Hashable1)

data RTValue var blk fun
  = RTArith ArithOp (RTValue var blk fun) (RTValue var blk fun)
  | RTComp CompOp (RTValue var blk fun) (RTValue var blk fun)
  | RTPrim Prim
  | RTCond (RTValue var blk fun) (RTValue var blk fun) (RTValue var blk fun)
  | Call fun [RTValue var blk fun]
  | PlaceVal (RTPlace var blk fun)
  | Block (RTProg var (Bind () blk) fun)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving anyclass (Hashable, Hashable1)

data RTPlace var blk fun
  = Place var
  | Deref (RTValue var blk fun)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving anyclass (Hashable, Hashable1)

data RTFunc fun = RTFunc
  { fnArgs :: [Type],
    fnRet :: Type,
    fnBody :: RTValue (Bind Int Void) Void fun
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving anyclass (Hashable, Hashable1)

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
