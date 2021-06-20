{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module EvalTypes where

import Bound.Scope.Simple
import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Data.Hashable
import Data.Hashable.Lifted
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Void
import Expr
import GHC.Generics
import Lens.Micro.Platform
import Lib.Fix

data ValueF f
  = VClosure (Scope () Expr ThunkID)
  | VRun Deps (RValV Void)
  | VFunc Deps FuncID
  | VType Type

newtype Hash = Hash Int
  deriving newtype (Eq, Ord, Hashable)

newtype Deps = Deps (Map Hash Fundef)
  deriving newtype (Semigroup, Monoid)

data Thunk
  = Computed WHNF
  | Deferred (Eval WHNF)

newtype ThunkID = ThunkID Int

newtype Eval a = Eval (ExceptT String (State EvalState) a)
  deriving newtype (Functor, Applicative, Monad, MonadState EvalState, MonadError String)

type Comp = WriterT Deps Eval

data EvalState = EvalState
  { _evalThunks :: IntMap Thunk,
    _evalSource :: Int
  }

type WHNF = ValueF Thunk

type NF = Fix ValueF

newtype LabelID = LabelID Int
  deriving newtype (Hashable)

newtype FuncID = FuncID Hash
  deriving newtype (Hashable)

data Fundef = Fundef
  { funcArgs :: [Type],
    funcRet :: Type,
    funcBody :: Scope (Maybe Int) RValV Void
  }
  deriving stock (Generic)
  deriving anyclass (Hashable)

newtype RValV a
  = RValV
      ( RVal
          (Const Type)
          (Place RValV)
          RValV
          (Const LabelID)
          (Const FuncID)
          a
      )
  deriving stock (Generic, Generic1)
  deriving anyclass (Hashable1)

makeLenses ''EvalState

thunk :: ThunkID -> Lens' EvalState (Maybe Thunk)
thunk (ThunkID t) = evalThunks . at t

fresh :: Eval Int
fresh = state (\(EvalState m n) -> (n, EvalState m (succ n)))

freshThunk :: Eval ThunkID
freshThunk = ThunkID <$> fresh

describeValue :: ValueF a -> String
describeValue VClosure {} = "closure"
describeValue VRun {} = "runtime expression"
describeValue VFunc {} = "runtime function"
describeValue VType {} = "type"
