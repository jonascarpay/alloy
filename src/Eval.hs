{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Eval where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
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
whnf (Run mlbl prog) =
  localBlock $ \blk -> do
    (prog', deps) <- runWriterT $
      case mlbl of
        Nothing -> compileBlock blk prog
        Just lbl -> do
          t <- lift . lift $ refer (VBlk blk)
          local (binds . at lbl ?~ t) (compileBlock blk prog)
    pure $ VRun deps (Block (bindBlock blk prog'))

compileBlock ::
  BlockIX ->
  ProgE ->
  Comp (RTProg VarIX BlockIX FuncIX)
compileBlock blk = go
  where
    go (DeclE name typ val k) = do
      typ' <- lift (whnf typ) >>= ensureType
      val' <- compileValue val
      k' <- localVar $ \ix -> do
        tix <- lift . lift $ refer (VVar ix)
        local (binds . at name ?~ tix) $
          bindVar ix <$> go k
      pure (Decl typ' val' k')
    go (AssignE lhs rhs k) = do
      lhs' <- compilePlace lhs
      rhs' <- compileValue rhs
      k' <- go k
      pure $ Assign lhs' rhs' k'
    go (BreakE mlbl val) = do
      lbl' <- case mlbl of
        Nothing -> pure blk
        Just lbl -> lift (whnf lbl) >>= ensureBlock
      val' <- compileValue val
      pure $ Break lbl' val'
    go (ExprE val k) = do
      val' <- compileValue val
      k' <- go k
      pure $ ExprStmt val' k'

compileValue :: Expr -> Comp (RTVal VarIX BlockIX FuncIX)
compileValue = undefined

compilePlace :: Expr -> Comp (RTPlace VarIX BlockIX FuncIX)
compilePlace = undefined
