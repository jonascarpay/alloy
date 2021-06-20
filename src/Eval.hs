{-# LANGUAGE LambdaCase #-}

module Eval where

import Bound.Scope.Simple
import Control.Applicative
import Control.Monad.Except
import Control.Monad.Writer
import Data.Void
import EvalTypes
import Expr
import Lens.Micro.Platform

defer :: Expr ThunkID -> Eval ThunkID
defer expr = do
  tid <- freshThunk
  thunk tid ?= Deferred (whnf expr)
  pure tid

force :: ThunkID -> Eval WHNF
force tid =
  use (thunk tid) >>= \case
    Nothing -> throwError "impossible"
    Just (Computed v) -> pure v
    Just (Deferred m) -> do
      v <- m
      thunk tid ?= Computed v
      pure v

whnf :: Expr ThunkID -> Eval WHNF
whnf (Var a) = force a
whnf (App l r) =
  whnf l >>= \case
    VClosure b -> do
      tr <- defer r
      whnf $ instantiate (const $ pure tr) b
    val -> throwError $ "Expected a closure, but got a " <> describeValue val
whnf (Lam body) = pure $ VClosure body
whnf (Type typ) = pure $ VType typ
whnf (Run run) = do
  (rv, deps) <- runWriterT $ compile run
  pure $ VRun deps rv
whnf (Func args ret body) = undefined

compileVal :: WHNF -> Comp (RValV Void)
compileVal VClosure {} = throwError "no"
compileVal (VRun deps rv) = rv <$ tell deps
compileVal (VType _) = throwError "no"
compileVal (VFunc _ _) = throwError "no"

compileFunc :: WHNF -> Comp FuncID
compileFunc (VFunc deps fid) = fid <$ tell deps
compileFunc _ = throwError "calling not a function"

compile :: RVal Expr Expr Expr Expr Expr ThunkID -> Comp (RValV Void)
compile (RBin op l r) = do
  l' <- lift (whnf l) >>= compileVal
  r' <- lift (whnf r) >>= compileVal
  pure $ RValV $ RBin op l' r'
compile (Call fn args) = do
  fn' <- lift (whnf fn) >>= compileFunc
  args' <- traverse (lift . whnf >=> compileVal) args
  pure $ RValV $ Call (Const fn') args'

-- compile (Block prog) = do
--   pure $ RValV $ Block $ hoistProg _ prog
-- compile (Place plc) = undefined

-- compileProg ::
