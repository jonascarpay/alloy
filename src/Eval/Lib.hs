{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

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
describeValue VVar {} = "runtime variable"
describeValue VBlk {} = "runtime block"
describeValue VAttr {} = "attribute set"

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
      liftIO $ writeIORef ref (Left $ throwError "infinite recursion")
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

-- TODO maybe just Int -> ([VarIX] -> m a) -> m a
localVars :: MonadReader EvalEnv m => [a] -> ([(a, VarIX)] -> m r) -> m r
localVars as m = do
  let n = length as
  VarIX ix <- view varSource
  let ixs = VarIX <$> [ix ..]
  local (varSource .~ VarIX (ix + n)) (m $ zip as ixs)

abstractOver :: Traversal s t a (Bind b a) -> (a -> Maybe b) -> s -> t
abstractOver t f = over t (\a -> maybe (Free a) Bound (f a))

abstract1Over :: Eq a => Traversal s t a (Bind () a) -> a -> s -> t
abstract1Over t a = over t (\var -> if var == a then Bound () else Free var)

closedOver :: Traversal s t a b -> s -> Maybe t
closedOver t = t (const Nothing)

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
