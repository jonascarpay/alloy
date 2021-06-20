{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval where

import Bound.Scope.Simple
import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Data.Hashable
import Data.Hashable.Lifted
import Data.IntMap (IntMap)
import Data.Void
import Expr
import GHC.Generics
import Lib.Fix

data ValueF f
  = VClosure (Scope () Expr Void)
  | VRun (RValV Void)

data Thunk
  = Computed WHNF
  | Deferred (Eval WHNF)

newtype ThunkID = ThunkID Int

newtype Eval a = Eval (ExceptT String (State (IntMap Thunk)) a)

type WHNF = ValueF Thunk

type NF = Fix ValueF

newtype LabelID = LabelID Int
  deriving newtype (Hashable)

newtype FuncID = FuncID Int
  deriving newtype (Hashable)

data Func = Func
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

whnf :: Expr Void -> Eval WHNF
whnf = undefined
