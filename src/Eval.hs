{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Eval where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Void
import Eval.Lenses
import Eval.Lib
import Eval.Types
import Expr
import Lens.Micro.Platform

whnf :: Expr -> Eval Lazy
whnf (Var name) = lookupName name >>= lift . force
whnf (App f x) = do
  tx <- close (whnf x) >>= lift . defer
  whnf f >>= \case
    VClosure arg body -> local (binds . at arg ?~ tx) (whnf body)
    val -> throwError $ "Applying a value to a " <> describeValue val
whnf (Lam arg body) = pure (VClosure arg body)
whnf (Type typ) = pure (VType typ)
