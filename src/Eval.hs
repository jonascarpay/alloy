{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Eval where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Hashable
import Data.List (findIndex)
import Data.Map (Map)
import Data.Map qualified as M
import Eval.Lenses
import Eval.Lib
import Eval.Types
import Expr
import Lens.Micro.Platform hiding (ix)

runEval :: Expr -> IO (Either String NF)
runEval expr = do
  runExceptT $
    unEvalBase $
      unEval (whnf expr) >>= deepseq
  where
    deepseq :: WHNF -> EvalBase NF
    deepseq = fmap NF . traverse (force >=> deepseq)

whnf :: Expr -> Eval WHNF
whnf (Var name) = lookupName name >>= lift . force
whnf (App f x) =
  whnf f >>= \case
    VClosure arg body env -> do
      tx <- close (whnf x) >>= defer
      local (binds .~ (env & at arg ?~ tx)) (whnf body)
    val -> throwError $ "Applying a value to a " <> describeValue val
whnf (Lam arg body) = VClosure arg body <$> view binds
whnf (Type typ) = pure (VType typ)
whnf (Prim prim) = pure (VPrim prim)
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
whnf (Let bindings body) = do
  env <- resolveBindings bindings
  local (binds %~ mappend env) (whnf body)
whnf (Attr bindings) = VAttr <$> resolveBindings bindings
whnf (Acc attr field) =
  whnf attr >>= \case
    VAttr m -> case m ^. at field of
      Nothing -> throwError $ "Attribute set does not contain field " <> show field
      Just t -> lift $ force t
    val -> throwError $ "Accessing field " <> show field <> " of a " <> describeValue val <> " instead of an attribute set"

resolveBindings :: [Binding] -> Eval (Map Name Thunk)
resolveBindings bindings = mfix $ \env -> -- witchcraft
  flip execStateT mempty $
    forM bindings $
      binding
        (\name body -> lift (close (local (binds %~ mappend env) (whnf body)) >>= defer) >>= tell1 name)
        (mapM_ $ \name -> lift (lookupName name) >>= tell1 name)
        -- Note that this implementation does not allow for inherit-from expressions to check whether the field is present.
        -- That would simplify the code, but also force evaluation of `env`, and therefore cause infinite recursion.
        -- So, to introduce the required indirection, we instead construct a new thunk for every name.
        ( \attrExpr attrs -> do
            tAttr <- lift (close (local (binds %~ mappend env) (whnf attrExpr)) >>= defer)
            forM_ attrs $ \name ->
              let acc =
                    lift (force tAttr) >>= \case
                      VAttr m -> case m ^. at name of
                        Nothing -> throwError $ "Attribute set did not contain attribute " <> show name
                        Just t -> lift (force t)
                      val -> throwError $ "Expression in a inherit-from did not evaluate to an attribute set but a " <> describeValue val
               in lift (close acc >>= defer) >>= tell1 name
        )
  where
    tell1 :: Name -> Thunk -> StateT (Map Name Thunk) Eval ()
    tell1 name thunk = do
      use (at name) >>= \case
        Nothing -> at name ?= thunk
        Just _ -> throwError $ "Double name: " <> show name

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

binOp :: BinOp -> WHNF -> WHNF -> Eval WHNF
binOp op a@VRun {} b = fromComp VRun $ join $ liftA2 (rtBinOp op) (compileValue a) (compileValue b)
binOp op a b@VRun {} = fromComp VRun $ join $ liftA2 (rtBinOp op) (compileValue a) (compileValue b)
binOp op (VPrim a) (VPrim b) = binPrim op a b
binOp (ArithOp _) l r = throwError $ unwords ["cannot perform arithmetic on a", describeValue l, "and a", describeValue r]
binOp (CompOp _) l r = throwError $ unwords ["cannot compare a", describeValue l, "and a", describeValue r]

rtBinOp :: BinOp -> RTVal VarIX BlockIX Hash -> RTVal VarIX BlockIX Hash -> Comp (RTVal VarIX BlockIX Hash)
rtBinOp (ArithOp op) l r = pure $ RTArith op l r
rtBinOp (CompOp op) l r = pure $ RTComp op l r

binPrim :: BinOp -> Prim -> Prim -> Eval WHNF
binPrim op (PInt a) (PInt b) = VPrim <$> binInt op a b
binPrim op (PDouble a) (PDouble b) = VPrim <$> binDouble op a b
binPrim op (PDouble a) (PInt b) = VPrim <$> binDouble op a (fromIntegral b)
binPrim op (PInt a) (PDouble b) = VPrim <$> binDouble op (fromIntegral a) b

binInt :: BinOp -> Int -> Int -> Eval Prim
binInt (ArithOp op) l r = pure . PInt $ arithInt op l r
  where
    arithInt :: ArithOp -> Int -> Int -> Int
    arithInt Add = (+)
    arithInt Sub = (-)
    arithInt Mul = (*)
    arithInt Div = div
binInt (CompOp op) l r = pure . PBool $ compOp op l r

binDouble :: BinOp -> Double -> Double -> Eval Prim
binDouble (ArithOp op) l r = pure . PDouble $ arithDouble op l r
  where
    arithDouble :: ArithOp -> Double -> Double -> Double
    arithDouble Add = (+)
    arithDouble Sub = (-)
    arithDouble Mul = (*)
    arithDouble Div = (/)
binDouble (CompOp op) l r = pure . PBool $ compOp op l r

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

compileValue :: WHNF -> Comp (RTVal VarIX BlockIX Hash)
compileValue (VRun deps val) = val <$ tell deps
compileValue (VVar var) = pure $ PlaceVal $ Place var
compileValue val = throwError $ "Cannot create a runtime expression from a " <> describeValue val

-- TODO there should probably be a place value
compilePlace :: WHNF -> Comp (RTPlace VarIX BlockIX Hash)
compilePlace (VVar var) = pure $ Place var
compilePlace val = throwError $ "Cannot create a place expression from a " <> describeValue val
