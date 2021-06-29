{-# LANGUAGE LambdaCase #-}

module Eval where

import Bound.Scope.Simple
import Bound.Var
import Control.Applicative
import Control.Monad.Except
import Control.Monad.Writer
import Data.Void
import EvalTypes
import Expr

whnf ::
  Traversable t =>
  Expr e (Thunk (Lazy (t a))) ->
  Eval (Lazy (t a))
whnf = undefined
