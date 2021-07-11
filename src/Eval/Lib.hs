{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Eval.Lib where

import Control.Monad.Except
import Control.Monad.Reader
import Data.IORef
import Eval.Lenses
import Eval.Types
import Expr (Name, Type)
import Lens.Micro.Platform hiding (ix)

close :: Applicative n => ReaderT r m a -> ReaderT r n (m a)
close (ReaderT f) = ReaderT $ \r -> pure (f r)

describeValue :: Value f -> String
describeValue VClosure {} = "closure"
describeValue VRun {} = "runtime expression"
describeValue VFunc {} = "runtime function"
describeValue VType {} = "type"

lookupName :: Name -> Eval Thunk
lookupName name =
  view (binds . at name) >>= \case
    Nothing -> throwError $ "Unknown variable: " <> show name
    Just t -> pure t

defer :: EvalBase Lazy -> EvalBase Thunk
defer = fmap Thunk . liftIO . newIORef . Left

refer :: Lazy -> EvalBase Thunk
refer = fmap Thunk . liftIO . newIORef . Right

force :: Thunk -> EvalBase Lazy
force (Thunk ref) = do
  liftIO (readIORef ref) >>= \case
    Right a -> pure a
    Left m -> do
      a <- m
      liftIO $ writeIORef ref (Right a)
      pure a

localBlock :: MonadReader EvalEnv m => (BlockIX -> m a) -> m a
localBlock k = do
  ix <- view blkSource
  local (blkSource %~ succ) (k ix)

capture :: Eq a => a -> a -> Bind () a
capture sub a = if a == sub then Bound () else Free a

bindBlock ::
  BlockIX ->
  RTProg VarIX BlockIX FuncIX ->
  RTProg VarIX (Bind () BlockIX) FuncIX
bindBlock cap = over rtProgLabels (capture cap)

localVar ::
  Name -> (VarIX -> Comp a) -> Comp a
localVar var k = do
  ix <- view varSource
  tix <- lift . lift $ refer (VVar ix)
  flip local (k ix) $ \env ->
    env
      & binds . at var ?~ tix
      & varSource %~ succ

bindVar ::
  VarIX ->
  RTProg VarIX BlockIX FuncIX ->
  RTProg (Bind () VarIX) BlockIX FuncIX
bindVar cap = over rtProgVars (capture cap)

ensureType :: MonadError String m => Lazy -> m Type
ensureType (VType typ) = pure typ
ensureType val = throwError $ "Expected a type, but got a " <> describeValue val

ensureBlock :: MonadError String m => Lazy -> m BlockIX
ensureBlock (VBlk ix) = pure ix
ensureBlock val = throwError $ "Expected a label, but got a " <> describeValue val
