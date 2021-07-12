{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Eval where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Hashable
import Data.List (findIndex)
import Data.Map qualified as M
import Eval.Lenses
import Eval.Lib
import Eval.Types
import Expr
import Lens.Micro.Platform hiding (ix)

whnf :: Expr -> Eval Lazy
whnf (Var name) = lookupName name >>= lift . force
whnf (App f x) =
  whnf f >>= \case
    VClosure arg body -> do
      tx <- close (whnf x) >>= defer
      local (binds . at arg ?~ tx) (whnf body)
    val -> throwError $ "Applying a value to a " <> describeValue val
whnf (Lam arg body) = pure (VClosure arg body)
whnf (Type typ) = pure (VType typ)
whnf (Run mlbl prog) =
  localBlock $ \blk -> do
    fromComp (\deps prog' -> VRun deps (Block (abstract1Over rtProgLabels blk prog'))) $
      case mlbl of
        Nothing -> compileBlock blk prog
        Just lbl -> do
          t <- refer (VBlk blk)
          local (binds . at lbl ?~ t) (compileBlock blk prog)
whnf (BinExpr op a b) = do
  a' <- whnf a
  b' <- whnf b
  binOp op a' b'
whnf (Func args ret body) = fromComp VFunc $ compileFunc args ret body

compileFunc :: [(Name, Expr)] -> Expr -> Expr -> Comp Hash
compileFunc args ret body = do
  ret' <- lift (whnf ret) >>= ensureType
  args' <- forM args $ traverse $ lift . whnf >=> ensureType
  localVars args' $ \ixs -> do
    body' <- bindVars ixs $ lift (whnf body) >>= compileValue
    let f ix = findIndex ((== ix) . snd) ixs
        scoped = abstractOver rtValVars f body'
    closedVars <- maybe (throwError "Vars would escape scope") pure $ closedOver rtValVars scoped
    closedBlks <- maybe (throwError "Labels would escape scope") pure $ closedOver rtValLabels closedVars
    let fundef = RTFunc (snd <$> args') ret' closedBlks
        guid = Hash $ hash fundef
    tell $ Deps $ M.singleton guid fundef
    pure guid
  where
    bindVars :: [((Name, Type), VarIX)] -> Comp a -> Comp a
    bindVars argIxs m = do
      argThunks <- forM argIxs $ \((name, _), ix) -> (name,) <$> refer (VVar ix)
      local (binds %~ mappend (M.fromList argThunks)) m

binOp :: BinOp -> Lazy -> Lazy -> Eval Lazy
binOp (ArithOp op) a@VRun {} b = fromComp VRun $ liftA2 (RTArith op) (compileValue a) (compileValue b)
binOp (ArithOp op) a b@VRun {} = fromComp VRun $ liftA2 (RTArith op) (compileValue a) (compileValue b)
binOp (CompOp op) a@VRun {} b = fromComp VRun $ liftA2 (RTComp op) (compileValue a) (compileValue b)
binOp (CompOp op) a b@VRun {} = fromComp VRun $ liftA2 (RTComp op) (compileValue a) (compileValue b)
binOp (ArithOp _) l r = throwError $ unwords ["cannot perform arithmetic on a", describeValue l, "and a", describeValue r]
binOp (CompOp _) l r = throwError $ unwords ["cannot compare a", describeValue l, "and a", describeValue r]
binOp Concat l r = throwError $ unwords ["cannot concatenate a", describeValue l, "and a", describeValue r]

fromComp :: (Deps -> a -> r) -> Comp a -> Eval r
fromComp f m = (\(a, dep) -> f dep a) <$> runWriterT m

compileBlock ::
  BlockIX ->
  ProgE ->
  Comp (RTProg VarIX BlockIX Hash)
compileBlock blk = go
  where
    go (DeclE name typ val k) = do
      typ' <- lift (whnf typ) >>= ensureType
      val' <- lift (whnf val) >>= compileValue
      k' <- localVar $ \ix -> do
        t <- refer (VVar ix)
        local (binds . at name ?~ t) $
          abstract1Over rtProgVars ix <$> go k
      pure (Decl typ' val' k')
    go (AssignE lhs rhs k) = do
      lhs' <- lift (whnf lhs) >>= compilePlace
      rhs' <- lift (whnf rhs) >>= compileValue
      k' <- go k
      pure $ Assign lhs' rhs' k'
    go (BreakE mlbl val) = do
      lbl' <- case mlbl of
        Nothing -> pure blk
        Just lbl -> lift (whnf lbl) >>= ensureBlock
      val' <- lift (whnf val) >>= compileValue
      pure $ Break lbl' val'
    go (ExprE val k) = do
      val' <- lift (whnf val) >>= compileValue
      k' <- go k
      pure $ ExprStmt val' k'

compileValue :: Lazy -> Comp (RTVal VarIX BlockIX Hash)
compileValue (VRun deps val) = val <$ tell deps
compileValue (VVar var) = pure $ PlaceVal $ Place var
compileValue val = throwError $ "Cannot create a runtime expression from a " <> describeValue val

-- TODO there should probably be a place value
compilePlace :: Lazy -> Comp (RTPlace VarIX BlockIX Hash)
compilePlace (VVar var) = pure $ Place var
compilePlace val = throwError $ "Cannot create a place expression from a " <> describeValue val
