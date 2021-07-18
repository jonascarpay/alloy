{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Eval where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.ByteString.Short qualified as BSS
import Data.Hashable
import Data.List (findIndex)
import Data.Map (Map)
import Data.Map qualified as M
import Eval.BinOp
import Eval.Lenses
import Eval.Lib
import Eval.Types
import Expr
import Lens.Micro.Platform hiding (ix)

runEval :: Expr -> IO (Either String NF)
runEval expr = do
  runExceptT $
    unEvalBase $ do
      val <- unEval $ do
        tBuiltins <- traverse refer builtins >>= refer . VAttr
        local (binds . at "builtins" ?~ tBuiltins) (whnf expr)
      deepseq val
  where
    deepseq :: WHNF -> EvalBase NF
    deepseq = fmap NF . traverse (force >=> deepseq)

whnf :: Expr -> Eval WHNF
whnf (Var name) = lookupName name >>= lift . force
whnf (App f x) =
  whnf f >>= \case
    VClosure k -> close (whnf x) >>= defer >>= lift . k
    val -> throwError $ "Applying a value to a " <> describeValue val
whnf (Lam arg body) = do
  env <- ask
  pure $ VClosure $ \t -> runReaderT (whnf body) (env & binds . at arg ?~ t)
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
whnf (With attrs body) =
  whnf attrs >>= \case
    VAttr m -> local (binds %~ mappend m) (whnf body)
    val -> throwError $ "Inner expression in with-expression did not evaluate to an attribute set but a " <> describeValue val
whnf (Cond cond true false) =
  whnf cond >>= \case
    (VPrim (PBool True)) -> whnf true
    (VPrim (PBool False)) -> whnf false
    (VRun deps cond') -> fromComp VRun $ do
      tell deps
      true' <- lift (whnf true) >>= compileValue
      false' <- lift (whnf false) >>= compileValue
      pure $ RTCond cond' true' false'
    val -> throwError $ "Scrutinee is not a boolean or a runtime expression, but a " <> describeValue val
whnf (String str) = pure $ VString str
whnf (List l) = VList <$> traverse (whnf >=> refer) l

binOp :: BinOp -> WHNF -> WHNF -> Eval WHNF
binOp op a@VRun {} b = fromComp VRun $ join $ liftA2 (rtBinOp op) (compileValue a) (compileValue b)
binOp op a b@VRun {} = fromComp VRun $ join $ liftA2 (rtBinOp op) (compileValue a) (compileValue b)
binOp op (VPrim a) (VPrim b) = binPrim op a b
binOp op (VString l) (VString r) = binString op l r
binOp (ArithOp _) l r = throwError $ unwords ["cannot perform arithmetic on a", describeValue l, "and a", describeValue r]
binOp (CompOp _) l r = throwError $ unwords ["cannot compare a", describeValue l, "and a", describeValue r]

resolveBindings :: [Binding] -> Eval (Map Name Thunk)
resolveBindings bindings = mfix $ \env -> -- witchcraft
  flip execStateT mempty $
    forM bindings $ \case
      Simple name args body ->
        let body' = foldr Lam body args
         in lift (close (local (binds %~ mappend env) (whnf body')) >>= defer) >>= tell1 name
      Inherit names -> forM_ names $ \name -> lift (lookupName name) >>= tell1 name
      InheritFrom attrExpr names -> do
        tAttr <- lift (close (local (binds %~ mappend env) (whnf attrExpr)) >>= defer)
        -- Note that this implementation does not allow checking whether the field is actually present.
        -- That was the first thing I tried, and it was shorter, but also forced evaluation of `env`, and therefore caused infinite recursion.
        -- So, to introduce the required indirection, we instead construct a new thunk for every name.
        -- Nix seems to have the same behaviour, so I'm OK with it for now, plus I'm not convinced it could feasibly work any other way.
        forM_ names $ \name ->
          let acc =
                lift (force tAttr) >>= \case
                  VAttr m -> case m ^. at name of
                    Nothing -> throwError $ "Attribute set did not contain attribute " <> show name
                    Just t -> lift (force t)
                  val -> throwError $ "Expression in a inherit-from did not evaluate to an attribute set but a " <> describeValue val
           in lift (close acc >>= defer) >>= tell1 name
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

-- TODO these should be renamed (forceValue?) and moved since they don't have any nested evaluation
compileValue :: WHNF -> Comp (RTVal VarIX BlockIX Hash)
compileValue (VRun deps val) = val <$ tell deps
compileValue (VVar var) = pure $ PlaceVal $ Place var
compileValue val = throwError $ "Cannot create a runtime expression from a " <> describeValue val

-- TODO there should probably be a place value
compilePlace :: WHNF -> Comp (RTPlace VarIX BlockIX Hash)
compilePlace (VVar var) = pure $ Place var
compilePlace val = throwError $ "Cannot create a place expression from a " <> describeValue val

-- TODO Fixpoint, since this can contain builtins.types.int
{-# ANN builtins ("hlint: ignore" :: String) #-}
builtins :: Map Name (Value a)
builtins =
  M.fromList
    [ ("nine", VPrim $ PInt 9),
      ("lookup", vlookup)
    ]
  where
    vlookup = VClosure $ \tAttr ->
      force tAttr >>= \case
        VAttr attrs -> pure $
          VClosure $ \tStr ->
            force tStr >>= \case
              VString str -> case M.lookup (BSS.toShort str) attrs of
                Nothing -> throwError $ "Attribute set does not contain field " <> show str
                Just t -> force t
              val -> throwError $ "Second argument to builtins.lookup was not a string set but a " <> describeValue val
        val -> throwError $ "First argument to builtins.lookup was not an attribute set but a " <> describeValue val
