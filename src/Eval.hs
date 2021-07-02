{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Eval where

import Bound.Scope.Simple
import Control.Lens
import Control.Monad.Except
import EvalTypes
import Expr

whnf ::
  forall s.
  Prism' s (Thunk (Lazy s)) ->
  Expr s ->
  Eval (Lazy s)
whnf prm = go
  where
    go :: Expr s -> Eval (Lazy s)
    go (Pure s) = either (pure . VExt) force $ matching prm s
    go (App l r) =
      go l >>= \case
        VClosure l' -> do
          r' <- defer $ go r
          go $ review prm <$> instantiate1 (pure r') l'
        val -> throwError $ "Cannot reduce a " <> describeValue val
    go (Lam body) = VClosure <$> traverse (either (refer . VExt) pure . matching prm) body
    go _ = undefined

compile ::
  Lazy a ->
  Comp (RVal Expr Expr Expr Expr a)
compile = undefined

-- compileBlock ::
--   Prism' a (Thunk (Lazy a)) ->
--   Prog Expr Expr Expr Expr Expr a ->
--   Comp (Prog typ plc val Identity fun a)
-- compileBlock prm (Decl typ val k) = do
--   typ' <- _ typ
--   val' <- _ val
--   k' <- _ k
--   pure $ Decl typ' val' k'
-- compileBlock prm (Break lbl val) = do
--   lbl' <-
--     lift (whnf prm lbl) >>= \case
--       _ -> undefined
--   val' <-
--     lift (whnf prm val) >>= \case
--       _ -> undefined
--   pure $ Break lbl' val'
