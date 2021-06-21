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
      thunk tid ?= Deferred (throwError "infinite recursion")
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

compileToVal :: WHNF -> Comp (RValV Void)
compileToVal VClosure {} = throwError "no"
compileToVal (VRun deps rv) = rv <$ tell deps
compileToVal (VType _) = throwError "no"
compileToVal (VFunc _ _) = throwError "no"

compileToFunc :: WHNF -> Comp FuncID
compileToFunc (VFunc deps fid) = fid <$ tell deps
compileToFunc _ = throwError "calling not a function"

compileToPlace :: WHNF -> Comp (Place RValV Void)
compileToPlace = undefined

compile ::
  RVal Expr Expr Expr Expr Expr ThunkID ->
  Comp (RValV Void)
compile (RBin op l r) = do
  l' <- lift (whnf l) >>= compileToVal
  r' <- lift (whnf r) >>= compileToVal
  pure $ RValV $ RBin op l' r'
compile (Call fn args) = do
  fn' <- lift (whnf fn) >>= compileToFunc
  args' <- traverse (lift . whnf >=> compileToVal) args
  pure $ RValV $ Call (Const fn') args'
compile (Block prog) = RValV . Block <$> compileBlock prog
compile (Place plc) = do
  plc' <- lift (whnf plc) >>= compileToPlace
  pure $ RValV $ Place plc'

compileBlock ::
  Prog
    (Scope () Expr)
    (Scope () Expr)
    (Scope () Expr)
    (Scope () Expr)
    (Scope () Expr)
    ThunkID ->
  Comp
    ( Prog
        (Scope () (Const Type))
        (Scope () (Place RValV))
        (Scope () RValV)
        (Scope () (Const LabelID))
        (Scope () (Const FuncID))
        a
    )
compileBlock = undefined
