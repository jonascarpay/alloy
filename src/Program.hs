{-# LANGUAGE DeriveTraversable #-}

module Program where

import Data.Map (Map)
import Expr

data RTExpr
  = RTVar Name
  | RTPrim Prim
  | RTArith ArithOp RTExpr RTExpr
  | RTBlock (Block Type RTExpr)
  | RTCall Name [RTExpr]
  deriving (Eq, Show)

data Function typ expr = Function
  { _args :: [(Name, typ)],
    _ret :: typ,
    _body :: Block typ expr
  }
  deriving (Eq, Show)

newtype RuntimeEnv = RuntimeEnv
  {rtFunctions :: Map Name (Function Type RTExpr)}
  deriving (Eq, Show)

instance Semigroup RuntimeEnv where RuntimeEnv fns <> RuntimeEnv fns' = RuntimeEnv (fns <> fns')

instance Monoid RuntimeEnv where mempty = RuntimeEnv mempty
