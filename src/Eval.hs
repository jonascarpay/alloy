{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Eval (runEval) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable
import Data.List (elemIndex)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text qualified as T
import Eval.BinOp
import Eval.Builtins (builtins)
import Eval.Lib
import Eval.Typecheck
import Eval.Types
import Expr
import Lens.Micro.Platform hiding (ix)
import Rebound

runEval :: Expr -> IO (Either String NF)
runEval expr = do
  runExceptT $
    flip evalStateT 0 $
      flip runReaderT mempty $
        unEvalBase $ do
          val <- unEval $ do
            tBuiltins <- lift $ deepRefer builtins >>= refer
            local (binds . at "builtins" ?~ tBuiltins) (whnf expr)
          deepseq val
  where
    deepseq :: WHNF -> EvalBase NF
    deepseq = fmap NF . traverse (force >=> deepseq)
    deepRefer :: NF -> EvalBase WHNF
    deepRefer = traverse (deepRefer >=> refer) . unNF

whnf :: Expr -> Eval WHNF
whnf (Var name) = lookupName name >>= lift . force
whnf (App f x) =
  whnf f >>= \case
    VClosure k -> close (whnf x) >>= defer >>= lift . k
    VFunc deps func ->
      whnf x >>= \case
        VList args -> fromComp VRTValue $ do
          tell deps
          args' <- forM (toList args) $ \arg ->
            (lift . lift) (force arg) >>= coerceRTValue
          pure $ Call func args' ()
        val -> throwError $ "Calling a runtime function with " <> describeValue val <> " as an argument instead of a list"
    val -> throwError $ "Applying a value to " <> describeValue val
whnf (Lam arg body) = do
  env <- ask
  pure $ VClosure $ \t -> runReaderT (whnf body) (env & binds . at arg ?~ t)
whnf (Prim prim) = pure (VPrim prim)
whnf (Run mlbl prog) = do
  blk <- freshBlock
  fromComp (\deps prog' -> VRTValue deps (rewriteBlockExpr (abstract1Over labels blk prog'))) $
    case mlbl of
      Nothing -> compileBlock prog
      Just lbl -> do
        t <- refer (VBlk blk)
        local (binds . at lbl ?~ t) (compileBlock prog)
whnf (BinExpr op a b) = do
  a' <- whnf a
  b' <- whnf b
  binOp op a' b'
whnf (Func mlbl args ret body) = uncurry VFunc <$> compileFunc mlbl args ret body
whnf (Let bindings body) = do
  env <- resolveBindings bindings
  local (binds %~ mappend env) (whnf body)
whnf (Attr bindings) = VAttr <$> resolveBindings bindings
whnf (Sel haystack needle) =
  whnf haystack >>= \case
    VList l ->
      whnf needle >>= \case
        VPrim (PInt ix) -> case Seq.lookup ix l of
          Nothing -> throwError $ "List index " <> show ix <> " is out of bounds"
          Just t -> lift $ force t
        val -> throwError $ "list indexing requires an integer, but got " <> describeValue val
    VString bs ->
      whnf needle >>= \case
        VPrim (PInt ix) -> case indexMaybe bs ix of
          Nothing -> throwError $ "String index " <> show ix <> " is out of bounds"
          Just t -> pure . VString $ T.singleton t
        val -> throwError $ "indexing into strings requires an integer, but got " <> describeValue val
    VAttr attrs ->
      whnf needle >>= \case
        VString str -> case M.lookup str attrs of
          Nothing -> throwError $ "Attribute set does not contain field " <> show str
          Just t -> lift $ force t
        val -> throwError $ "indexing into an attr set requires a string, but got " <> describeValue val
    VRTPlace deps plc ->
      whnf needle >>= \case
        VPrim (PInt ix) -> pure $ VRTPlace deps (PlaceSel plc ix ())
        val -> throwError $ "indexing into a runtime lvalue set requires an int, but got " <> describeValue val
    VRTValue deps val ->
      whnf needle >>= \case
        VPrim (PInt ix) -> pure $ VRTValue deps (ValueSel val ix ())
        val -> throwError $ "indexing into a runtime rvalue set requires an int, but got " <> describeValue val
    val -> throwError $ "Indexing into " <> describeValue val <> " instead of a list, attribute set, string, or runtime expression"
whnf (With attrs body) =
  whnf attrs >>= \case
    VAttr m -> local (binds %~ mappend m) (whnf body)
    val -> throwError $ "Inner expression in with-expression did not evaluate to an attribute set but " <> describeValue val
whnf (Cond cond true false) =
  whnf cond >>= \case
    VPrim (PBool True) -> whnf true
    VPrim (PBool False) -> whnf false
    -- TODO throw a better error message in the case of something that's not a runtime variable
    val -> fromComp VRTValue $ do
      cond' <- coerceRTValue val
      true' <- lift (whnf true) >>= coerceRTValue
      false' <- lift (whnf false) >>= coerceRTValue
      pure $ RTCond cond' true' false' ()
whnf (String str) = pure $ VString str
whnf (List l) = VList <$> traverse (whnf >=> refer) l
whnf (Ref expr) = fromComp VRTValue $ fmap (`RTRef` ()) $ lift (whnf expr) >>= coerceRTPlace
whnf (Deref expr) = fromComp VRTPlace $ fmap (`RTDeref` ()) $ lift (whnf expr) >>= coerceRTValue

binOp :: BinOp -> WHNF -> WHNF -> Eval WHNF
binOp op (VPrim a) (VPrim b) = binPrim op a b
binOp op (VString l) (VString r) = binString op l r
binOp op (VList l) (VList r) = binList op l r
binOp (ArithOp Mul) (VList l) (VPrim (PInt n)) = pure $ VList $ fold $ replicate n l
binOp (ArithOp Mul) (VPrim (PInt n)) (VList l) = pure $ VList $ fold $ replicate n l
binOp op a b =
  withError ("Could not make a binary expression for " <> describeValue a <> " and " <> describeValue b) $
    fromComp VRTValue $ do
      a' <- coerceRTValue a
      b' <- coerceRTValue b
      rtBinOp op a' b'

resolveBindings :: [Binding] -> Eval (Map Symbol Thunk)
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
                  val -> throwError $ "Expression in a inherit-from did not evaluate to an attribute set but " <> describeValue val
           in lift (close acc >>= defer) >>= tell1 name
  where
    tell1 :: Symbol -> Thunk -> StateT (Map Symbol Thunk) Eval ()
    tell1 name thunk = do
      use (at name) >>= \case
        Nothing -> at name ?= thunk
        Just _ -> throwError $ "Double name: " <> show name

compileFunc :: Maybe Symbol -> [(Symbol, Expr)] -> Expr -> Expr -> Eval (Deps, Either FuncIX Hash)
compileFunc mlbl args ret body = do
  retType <- whnf ret >>= ensureType
  args' <- forM args $ \(name, expr) -> do
    typ <- whnf expr >>= ensureType
    ix <- freshVar
    pure (name, typ, ix)
  let argTypes = view _2 <$> args'
      sig = (argTypes, retType)
  fun <- freshFunc
  liftLocal (M.insert fun sig) $ do
    (body', Deps closed open) <-
      runWriterT . bindFunction fun . bindVars args' $
        lift (whnf body) >>= coerceRTValue
    closedBody <- do
      let scoped = abstractOver vars (`elemIndex` (view _3 <$> args')) body'
      closedVars <- maybe (throwError "Vars would escape scope") pure $ maybeClosedOver (vars . traverse) scoped
      maybe (throwError "Labels would escape scope") pure $ maybeClosedOver labels closedVars
    typeChecked <- do
      let closedSigs = funcSig <$> closed
          subSigs = mkSubSigs open
      superSigs <- lift ask
      typeChecked <- liftEither $ typeCheck closedBody argTypes retType closedSigs (superSigs <> subSigs)
      pure $ RTFunc argTypes retType typeChecked
    pure $
      let cg = mkCallGraph fun typeChecked open
       in case closeFunc cg of
            Nothing -> (Deps closed (S.singleton cg), Left fun)
            Just (guid, closed') -> (Deps (closed <> closed') mempty, Right guid)
  where
    funcSig :: RTFunc fun -> Sig
    funcSig fn = (fnArgs fn, fnRet fn)
    mkSubSigs :: Set CallGraph -> Map FuncIX Sig
    mkSubSigs = foldMap $ \(CallGraph ix fn _ _ sub) -> M.singleton ix (funcSig fn) <> mkSubSigs sub
    bindVars :: [(Symbol, Type, VarIX)] -> Comp a -> Comp a
    bindVars argIxs m = do
      argThunks <- forM argIxs $ \(name, _, ix) -> (name,) <$> refer (VRTPlace mempty $ Place ix ())
      local (binds %~ mappend (M.fromList argThunks)) m
    bindFunction :: FuncIX -> Comp a -> Comp a
    bindFunction fun k = do
      tnk <- refer $ VFunc mempty (Left fun)
      case mlbl of
        Nothing -> k
        Just lbl -> local (binds . at lbl ?~ tnk) k

-- _@{expr} -> expr
-- TODO
-- This function traverses all labels in the program to see if it binds any.
-- That happens shortly after it allocated and abstracted the labels, so
-- they're traversed twice. Worse, this happens every time we enter a block, so
-- it's at least quadratic.  It even happens if the block was never labeled,
-- since we forget that a label might not have been applied.  This is currently
-- also annoying when printing, since we print way too many labels.  An easy
-- improvement would be to split labeled and non-labeled cases, but maybe
-- there's a better, more holistic solution.
rewriteBlockExpr ::
  RTProg VarIX (Bind () BlockIX) (Either FuncIX Hash) Prim () ->
  EvalPhase RTValue
rewriteBlockExpr (ExprStmt val Nothing) | Just val' <- maybeUnusedOver labels val = val'
rewriteBlockExpr (Break (Bound ()) val) | Just val' <- maybeUnusedOver labels val = val'
rewriteBlockExpr prog = Block prog ()

-- {void; b} -> {b}
-- {_@{foo; bar;}; baz} -> {foo; bar; baz}
-- {_@{foo; bar}} -> {foo; bar}
-- Essentially means that if you have {a; b}, we try replacing the tail of a with b.
rewriteExprStmt ::
  EvalPhase RTValue ->
  Maybe (EvalPhase RTProg) ->
  EvalPhase RTProg
rewriteExprStmt (RTLit PVoid ()) (Just k) = k
rewriteExprStmt (Block val ()) k | Just val' <- maybeUnusedOver labels val >>= go k = val'
  where
    go :: Maybe (EvalPhase RTProg) -> EvalPhase RTProg -> Maybe (EvalPhase RTProg)
    go Nothing val = pure val
    go (Just k) val = splice val k
    splice :: EvalPhase RTProg -> EvalPhase RTProg -> Maybe (EvalPhase RTProg)
    splice (ExprStmt (RTLit PVoid ()) Nothing) k = pure k
    splice (ExprStmt val Nothing) k = pure $ ExprStmt val (Just k)
    splice (ExprStmt val (Just k')) k = ExprStmt val . Just <$> splice k' k
    splice (Assign lhs rhs k') k = Assign lhs rhs <$> splice k' k
    splice Decl {} _ = Nothing
    splice Continue {} _ = Nothing
    splice Break {} _ = Nothing
rewriteExprStmt val k = ExprStmt val k

compileBlock ::
  ProgE ->
  Comp (EvalPhase RTProg)
compileBlock = go
  where
    go (DeclE name mtyp val k) = do
      mtyp' <- forM mtyp $ \typ -> lift (whnf typ) >>= ensureType
      val' <- lift (whnf val) >>= coerceRTValue
      k' <- do
        ix <- freshVar
        t <- refer (VRTPlace mempty $ Place ix ())
        local (binds . at name ?~ t) $
          abstract1Over vars ix <$> go k
      pure (Decl mtyp' val' k')
    go (AssignE lhs rhs k) = do
      lhs' <- lift (whnf lhs) >>= coerceRTPlace
      rhs' <- lift (whnf rhs) >>= coerceRTValue
      k' <- go k
      pure $ Assign lhs' rhs' k'
    go (BreakE lbl mexpr) = do
      lbl' <- lift (whnf lbl) >>= ensureBlock
      expr' <- case mexpr of
        Nothing -> pure $ RTLit PVoid ()
        Just expr -> lift (whnf expr) >>= coerceRTValue
      pure $ Break lbl' expr'
    go (ContinueE lbl) = fmap Continue $ lift (whnf lbl) >>= ensureBlock
    go (ExprE val k) = do
      val' <- lift (whnf val) >>= coerceRTValue
      k' <- traverse go k
      pure $ rewriteExprStmt val' k'
