{-# LANGUAGE DeriveTraversable #-}

module Program where

import Data.Map (Map)
import Expr

type RTBlock typ = Block typ (RTExpr typ typ)

data RTLiteral
  = RTInt Int
  | RTDouble Double
  | RTStruct (Map Name RTLiteral)
  deriving (Eq, Show)

data RTExpr typ a
  = RTVar Name a
  | RTLiteral RTLiteral a
  | RTArith ArithOp (RTExpr typ a) (RTExpr typ a) a
  | RTBlock (Block typ (RTExpr typ a)) a
  | RTCall Name [RTExpr typ a] a
  deriving (Eq, Show)

rtInfo :: RTExpr typ a -> a
rtInfo (RTVar _ a) = a
rtInfo (RTArith _ _ _ a) = a
rtInfo (RTBlock _ a) = a
rtInfo (RTCall _ _ a) = a
rtInfo (RTLiteral _ a) = a

-- rtType (RTStructAcc _ _ a) = a

data Function typ expr = Function
  { _args :: [(Name, typ)],
    _ret :: typ,
    _body :: Block typ expr
  }
  deriving (Eq, Show)

newtype RuntimeEnv = RuntimeEnv
  {rtFunctions :: Map Name (Function Type (RTExpr Type Type))}
  deriving (Eq, Show)

instance Semigroup RuntimeEnv where RuntimeEnv fns <> RuntimeEnv fns' = RuntimeEnv (fns <> fns')

instance Monoid RuntimeEnv where mempty = RuntimeEnv mempty
