{-# LANGUAGE DeriveTraversable #-}

module Program where

import Expr

data RTExpr
  = RTVar Name
  | RTPrim Prim
  | RTArith ArithOp RTExpr RTExpr
  | RTBlock (Block Type RTExpr)
  | RTCall Name [RTExpr]
  deriving (Eq, Show)
