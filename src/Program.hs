{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Program where

import Data.Hashable
import Data.Map (Map)
import Expr
import GHC.Generics

type RTBlock typ = Block typ (RTExpr typ typ)

data RTLiteral
  = RTInt Int
  | RTDouble Double
  | RTStruct (Map Name RTLiteral)
  deriving (Eq, Show, Generic)

instance Hashable RTLiteral

data RTExpr typ a
  = RTVar Name a
  | RTLiteral RTLiteral a
  | RTArith ArithOp (RTExpr typ a) (RTExpr typ a) a
  | RTBlock (Block typ (RTExpr typ a)) a
  | RTCall GUID [RTExpr typ a] a
  deriving (Eq, Show, Generic)

instance (Hashable typ, Hashable info) => Hashable (RTExpr typ info)

rtInfo :: RTExpr typ a -> a
rtInfo (RTVar _ a) = a
rtInfo (RTArith _ _ _ a) = a
rtInfo (RTBlock _ a) = a
rtInfo (RTCall _ _ a) = a
rtInfo (RTLiteral _ a) = a

newtype GUID = GUID {unGUID :: Int}
  deriving (Eq, Show, Ord, Hashable)

data Function = Function
  { _args :: [(Name, Type)],
    fnRet :: Type,
    _body :: Block Type (RTExpr Type Type),
    fnGuid :: GUID
  }
  deriving (Eq, Show)

mkFunction :: [(Name, Type)] -> Type -> RTBlock Type -> Function
mkFunction args ret body = Function args ret body (GUID $ hash (args, ret, body))

newtype RuntimeEnv = RuntimeEnv
  {rtFunctions :: Map GUID Function}
  deriving (Eq, Show)

instance Semigroup RuntimeEnv where RuntimeEnv fns <> RuntimeEnv fns' = RuntimeEnv (fns <> fns')

instance Monoid RuntimeEnv where mempty = RuntimeEnv mempty
