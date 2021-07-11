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

defer :: MonadIO m => EvalBase Lazy -> m Thunk
defer = fmap Thunk . liftIO . newIORef . Left

refer :: MonadIO m => Lazy -> m Thunk
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

localVar :: MonadReader EvalEnv m => (VarIX -> m a) -> m a
localVar k = do
  ix <- view varSource
  local (varSource %~ succ) (k ix)

capture :: Eq a => a -> a -> Bind () a
capture sub a = if a == sub then Bound () else Free a

bindBlock ::
  BlockIX ->
  RTProg VarIX BlockIX FuncIX ->
  RTProg VarIX (Bind () BlockIX) FuncIX
bindBlock cap = over rtProgLabels (capture cap)

bindVar ::
  VarIX ->
  RTProg VarIX BlockIX FuncIX ->
  RTProg (Bind () VarIX) BlockIX FuncIX
bindVar cap = over rtProgVars (capture cap)

-- TODO prisms?
ensureValue :: MonadError String m => String -> (Lazy -> Maybe r) -> Lazy -> m r
ensureValue ex f v = case f v of
  Just r -> pure r
  Nothing -> throwError $ "Expected a " <> ex <> ", but got a " <> describeValue v

ensureType :: MonadError String m => Lazy -> m Type
ensureType = ensureValue "type" $ \case
  VType typ -> Just typ
  _ -> Nothing

ensureBlock :: MonadError String m => Lazy -> m BlockIX
ensureBlock = ensureValue "block" $ \case
  VBlk blk -> Just blk
  _ -> Nothing
