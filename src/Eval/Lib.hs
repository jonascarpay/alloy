{-# LANGUAGE LambdaCase #-}

module Eval.Lib where

import Control.Monad.Except
import Control.Monad.Reader
import Data.IORef
import Eval.Lenses
import Eval.Types
import Expr (Name)
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

localLabel ::
  Name -> (LabelIX -> Comp a) -> Comp a
localLabel lbl k = do
  ix <- view lblSource
  tix <- lift . lift $ refer (VLbl ix)
  flip local (k ix) $ \env ->
    env
      & binds . at lbl ?~ tix
      & lblSource %~ succ

shiftLabel ::
  RTProg VarIX LabelIX FuncIX ->
  RTProg VarIX (Bind () LabelIX) FuncIX
shiftLabel = over rtProgLabels Free

bindLabel ::
  LabelIX ->
  RTProg VarIX LabelIX FuncIX ->
  RTProg VarIX (Bind () LabelIX) FuncIX
bindLabel cap = over rtProgLabels (\ix -> if ix == cap then Bound () else Free ix)
