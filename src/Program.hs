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

newtype RuntimeEnv = RuntimeEnv
  {rtFunctions :: Map GUID FunDef}
  deriving (Eq, Show)

data FunDef = FunDef
  { fnArgs :: [(Name, Type)],
    fnRet :: Type,
    fnName :: Name,
    fnBody :: RTBlock Type
  }
  deriving (Eq, Show, Generic)

instance Hashable FunDef

instance Semigroup RuntimeEnv where RuntimeEnv fns <> RuntimeEnv fns' = RuntimeEnv (fns <> fns')

instance Monoid RuntimeEnv where mempty = RuntimeEnv mempty
