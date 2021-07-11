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
import Lens.Micro.Platform hiding (ix)

whnf :: Expr -> Eval Lazy
whnf (Var name) = lookupName name >>= lift . force
whnf (App f x) = do
  tx <- close (whnf x) >>= lift . defer
  whnf f >>= \case
    VClosure arg body -> local (binds . at arg ?~ tx) (whnf body)
    val -> throwError $ "Applying a value to a " <> describeValue val
whnf (Lam arg body) = pure (VClosure arg body)
whnf (Type typ) = pure (VType typ)
whnf (Run mlbl prog) = do
  (prog', deps) <- runWriterT $ case mlbl of
    Nothing -> shiftLabel <$> compileBlock prog
    Just lbl -> localLabel lbl $ \ix -> bindLabel ix <$> compileBlock prog
  pure $ VRun deps (Block prog')

compileBlock :: ProgE -> Comp (RTProg VarIX LabelIX FuncIX)
compileBlock (DeclE name typ val k) = do
  typ' <- lift (whnf typ) >>= ensureType
  val' <- compileValue val
  k' <- localVar name $ \ix ->
    bindVar ix <$> compileBlock k
  pure (Decl typ' val' k')

compileValue :: Expr -> Comp (RTVal VarIX LabelIX FuncIX)
compileValue = undefined
