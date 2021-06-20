module Eval where

import Data.Void
import Expr
import Lib.Bound

data ValueF f
  = VClosure (Scope () Expr Void)

type WHNF = ValueF Thunk

type NF = Fix ValueF
