{-# LANGUAGE DeriveTraversable #-}

module Program where

import Data.Map (Map)
import Expr

data RTExpr a
  = RTVar Name a
  | RTPrim Prim a
  | RTArith ArithOp (RTExpr a) (RTExpr a) a
  | RTBlock (Block Type (RTExpr a)) a
  | RTCall Name [RTExpr a] a
  deriving (Eq, Show)

rtType :: RTExpr a -> a
rtType (RTVar _ a) = a
rtType (RTPrim _ a) = a
rtType (RTArith _ _ _ a) = a
rtType (RTBlock _ a) = a
rtType (RTCall _ _ a) = a

data Function typ expr = Function
  { _args :: [(Name, typ)],
    _ret :: typ,
    _body :: Block typ expr
  }
  deriving (Eq, Show)

newtype RuntimeEnv = RuntimeEnv
  {rtFunctions :: Map Name (Function Type (RTExpr Type))}
  deriving (Eq, Show)

instance Semigroup RuntimeEnv where RuntimeEnv fns <> RuntimeEnv fns' = RuntimeEnv (fns <> fns')

instance Monoid RuntimeEnv where mempty = RuntimeEnv mempty
