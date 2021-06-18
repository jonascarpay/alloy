{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Evaluate (eval) where

import Builtins
import Control.Applicative
import Control.Monad.Except
import Control.Monad.RWS as RWS
import Control.Monad.Reader
import Control.Monad.State (State, evalState)
import Control.Monad.Writer
import Coroutine
import Data.Bifunctor
import Data.Bitraversable
import Data.ByteString.Char8 qualified as BS8
import Data.Foldable
import Data.Hashable
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Eval
import Expr
import Lens.Micro.Platform
import Program

eval :: FilePath -> Expr -> IO (Either String Value)
eval fp eRoot =
  runEval $ do
    tbuiltin <- mkBuiltins
    fmap Fix $
      step
        (se0 & statBinds . at "builtins" ?~ tbuiltin)
        emptyEE
        eRoot
        >>= traverse deepEval
  where
    se0 = StaticEnv mempty Nothing fp

deferExpr ::
  StaticEnv ->
  ExpressionEnv ->
  Expr ->
  Eval ThunkID
deferExpr expr se de = do
  tid <- freshThunkId
  thunks . at tid ?= Deferred (step expr se de)
  pure tid

-- TODO once we only have non-Expr closures, this can be moved to Eval
stepApp ::
  LazyValue ->
  ThunkID ->
  ExpressionEnv ->
  Eval LazyValue
stepApp (VClosure arg body se) tid de = step (se & statBinds . at arg ?~ tid) de body
stepApp (VClosure' m) tid de = m de tid
stepApp (VRecCall n) tid de = throwError "TODO: recursive function application"
stepApp (VFunc deps call) tid de =
  force tid >>= \case
    VList args ->
      pure $
        VRT $ do
          tv <- view eeExpr >>= forceTv
          tell deps
          (argTypes, retType) <- undefined

          pure $ RTCall (either CallTemp CallKnown call) _args tv
    args -> throwError $ "Applying a " <> describeValue args <> " to a runtime function"
stepApp val _ _ = throwError $ "Calling a value to a non-function " <> describeValue val

step ::
  StaticEnv ->
  -- TODO the ExpressionEnv argument might just be a collection of integers instead
  ExpressionEnv ->
  Expr ->
  Eval LazyValue
step _ _ (Prim p) = pure $ VPrim p
step se de (App f x) = do
  tf <- step se de f
  tx <- deferExpr se de x
  stepApp tf tx de
step se _ (Var x) =
  let err = (throwError $ "unknown variable " <> show x)
   in maybe err force $ se ^. statBinds . at x
step se _ (Lam arg body) = pure $ VClosure arg body se
step se de (Let binds body) = step se de (With (Attr binds) body)
step se de (Attr binds) = VAttr <$> stepAttrs binds se de
step se de (Acc f em) = step se de em >>= accessor f >>= force
step se de (BinExpr bop a b) = do
  va <- step se de a
  vb <- step se de b
  stepBinExpr bop va vb
step se de (With bind body) =
  step se de bind >>= \case
    VAttr m -> step (se & statBinds %~ (m <>)) de body
    _ -> throwError "Binder in `with` expression did not evaluate to an attrset"
step se de (List l) = VList <$> traverse (deferExpr se de) l
step se de (Cond cond tr fl) = do
  step se de cond >>= \case
    VPrim (PBool True) -> step se de tr
    VPrim (PBool False) -> step se de fl
    _ -> throwError "Did not evaluate to a boolean"
step _ _ (BlockExpr _) = throwError "TODO: block expression"
step _ _ (Func _ _ _) = throwError "TODO: func expression"

-- step (Func args ret bodyExpr) = do
--   args' <- forM args $ \(name, expr) -> do
--     typ <- evalType expr
--     var <- freshVarId
--     thunk <- deferVal $ VRTVar var
--     tv <- tvar typ
--     pure (name, typ, thunk, var, tv)
--   retType <- evalType ret
--   retVar <- tvar retType
--   recDepth <- length <$> view envFnStack
--   self <- deferVal $ VSelf recDepth
--   name <- view $ envName . to (fromMaybe "fn")
--   (blk, deps) <-
--     local (bindThunk "self" self) $
--       local (functionBodyEnv args' retVar) $
--         step bodyExpr >>= \case
--           VBlock env blk -> local (staticEnv .~ env) $ runWriterT $ compileBlock blk
--           _ -> throwError "Function body did not evaluate to a block expression"
--   let getTypeVoid = getType (pure TVoid)
--   typedBlk <- typecheckFunction getTypeVoid blk
--   let farg (_, typ, _, var, _) = (var, typ)
--       funDef = FunDef (farg <$> args') retType name typedBlk
--   uncurry VFunc <$> mkFunction freshFuncId recDepth deps funDef

arith :: ArithOp -> LazyValue -> LazyValue -> Eval LazyValue
arith op (VPrim (PInt pa)) (VPrim (PInt pb)) = pure . VPrim . PInt $ arithInt op pa pb
arith op (VPrim (PDouble pa)) (VPrim (PDouble pb)) = pure . VPrim . PDouble $ arithFloat op pa pb
arith op (VRT lhs) rhs = do
  rhs' <- compileVal rhs
  pure $
    VRT $ do
      a <- view eeExpr >>= forceTv
      l <- lhs
      r <- rhs'
      pure $ RTBin (ArithOp op) l r a
arith op lhs (VRT rhs) = do
  lhs' <- compileVal lhs
  pure $
    VRT $ do
      a <- view eeExpr >>= forceTv
      l <- lhs'
      r <- rhs
      pure $ RTBin (ArithOp op) l r a
arith _ lhs rhs = throwError $ "Error performing arithmatic on a " <> describeValue lhs <> " and a " <> describeValue rhs

-- TODO just pattern match in the arguments directly
stepBinExpr ::
  BinOp ->
  LazyValue ->
  LazyValue ->
  Eval LazyValue
stepBinExpr bop va vb =
  case (bop, va, vb) of
    (ArithOp op, lhs, rhs) -> arith op lhs rhs
    (CompOp Eq, VPrim pa, VPrim pb) -> pure . VPrim . PBool $ pa == pb
    (CompOp Neq, VPrim pa, VPrim pb) -> pure . VPrim . PBool $ pa /= pb
    (CompOp Eq, VType ta, VType tb) -> pure . VPrim . PBool $ ta == tb
    (CompOp Neq, VType ta, VType tb) -> pure . VPrim . PBool $ ta /= tb
    (CompOp op, VPrim (PInt pa), VPrim (PInt pb)) -> pure . VPrim . PBool $ comp op pa pb
    (CompOp op, VPrim (PDouble pa), VPrim (PDouble pb)) -> pure . VPrim . PBool $ comp op pa pb
    (CompOp _, lhs, rhs) -> throwError $ unwords ["Cannot compare a", describeValue lhs, "to a", describeValue rhs]
    (Concat, VList la, VList lb) -> pure $ VList $ la <> lb
    (Concat, VPrim (PString sa), VPrim (PString sb)) -> pure $ VPrim $ PString $ sa <> sb
    (Concat, lhs, rhs) -> throwError $ unwords ["Cannot concatenate a", describeValue lhs, "and a", describeValue rhs]

compileVal :: LazyValue -> Eval RTExprV
compileVal (VRT e) = pure e
compileVal (VPrim p) = compilePrim p
compileVal (VVar vid) = pure $ do
  -- This feels weird, this is always resolved at runtime expression evaluation time  even though it could be done statically?
  tv <- stVar vid
  pure $ RTVar vid tv

compilePrim :: Prim -> Eval RTExprV
compilePrim = undefined

stepAttrs ::
  [Binding] ->
  StaticEnv ->
  ExpressionEnv ->
  Eval (Map Name ThunkID)
stepAttrs bindings se de =
  case desugarBinds bindings of
    Left name -> throwError $ "Double declaration of name " <> show name
    Right (DesugaredBindings binds inherits inheritFroms) -> do
      inherits' :: [(Name, ThunkID)] <- forM inherits $ \name ->
        case se ^. statBinds . at name of
          Nothing -> throwError $ "inherited variable " <> show name <> " is not present in the surrounding scope"
          Just thunk -> pure (name, thunk)
      binds' :: [((Name, ThunkID), Expr)] <- forM binds $ \(name, expr) -> do
        thunk <- freshThunkId
        pure ((name, thunk), expr)
      inheritFroms' :: [((Expr, ThunkID), [(Name, ThunkID)])] <- forM inheritFroms $ \(expr, attrs) -> do
        exprThunk <- freshThunkId
        attrs' <- forM attrs $ \attr -> do
          thunk <- freshThunkId
          pure (attr, thunk)
        pure ((expr, exprThunk), attrs')
      let env' :: [(Name, ThunkID)] = inherits' <> (fst <$> binds') <> (inheritFroms' >>= snd)
      let se' = se & statBinds %~ (M.fromList env' <>)
      forM_ inheritFroms' $ \((expr, exprThunk), attrs) -> do
        setThunk exprThunk $ Deferred $ step se' de expr
        forM_ attrs $ \(attr, attrThunk) ->
          setThunk attrThunk $ Deferred $ force exprThunk >>= accessor attr >>= force
      forM_ binds' $ \((name, thunk), expr) ->
        setThunk thunk $ Deferred $ step (se' & statName ?~ name) de expr
      pure $ M.fromList env'

accessor :: Name -> LazyValue -> Eval ThunkID
accessor attr (VAttr attrs) =
  case M.lookup attr attrs of
    Just t -> pure t
    _ -> throwError $ "field " <> show attr <> " not present"
accessor attr _ = throwError $ "Accessing field " <> show attr <> " of something that's not an attribute set"

-- functionBodyEnv :: [(Name, typ, ThunkID, VarID, TypeVar)] -> TypeVar -> Environment -> Environment
-- functionBodyEnv typedArgs ret (Environment (StaticEnv ctx ctxName fp) (DynamicEnv fns _ _ _ _)) =
--   Environment
--     (StaticEnv ctx' ctxName fp)
--     (DynamicEnv fns' Nothing (Just ret) (M.fromList dyn) mempty)
--   where
--     (argBindings, dyn, namedArgs) = unzip3 $ fmap f typedArgs
--       where
--         f (name, _, thunk, var, tv) = ((name, thunk), (var, tv), (name, tv))
--     ctx' = M.fromList argBindings <> ctx
--     fns' = (namedArgs, ret) : fns

-- typecheckFunction ::
--   (TypeVar -> Eval Type) ->
--   RTBlock var lbl call TypeVar ->
--   Eval (RTBlock var lbl call Type)
-- typecheckFunction getType (Block lbl stmts typ) = do
--   stmts' <- (traverse . types) getType stmts
--   typ' <- getType typ
--   pure $ Block lbl stmts' typ'
--   where
--     types ::
--       Traversal
--         (Stmt var lbl typ1 (RTExpr var lbl call typ1 typ1))
--         (Stmt var lbl typ2 (RTExpr var lbl call typ2 typ2))
--         typ1
--         typ2
--     types f = stmtMasterTraversal pure pure f (rtExprMasterTraversal pure pure pure f f pure)

-- evalType :: Expr -> Eval Type
-- evalType expr =
--   step expr >>= \case
--     VType tp -> pure tp
--     _ -> throwError "Expected type, got something else"

-- mkFunction ::
--   Monad m =>
--   m TempFuncID ->
--   RecIndex ->
--   Dependencies ->
--   FunDef VarID LabelID PreCall ->
--   m (Dependencies, Either TempFuncID GUID)
-- mkFunction fresh recDepth transDeps funDef =
--   case unresolvedCalls of
--     [] -> pure mkClosedFunction
--     l | any (< recDepth) l -> mkTempFunction
--     _ -> pure closeTempFunction
--   where
--     mkClosedFunction :: (Dependencies, Either TempFuncID GUID)
--     mkClosedFunction =
--       let unPreCall (CallKnown guid) = guid
--           unPreCall _ = error "impossible" -- TODO throwError
--           funDef' :: FunDef Slot LabelID GUID
--           funDef' =
--             funDef & funCalls %~ unPreCall
--               & assignSlots
--           guid = GUID $ hash funDef'
--           deps = transDeps & depKnownFuncs . at guid ?~ funDef'
--        in (deps, Right guid)

--     assignSlots :: FunDef VarID lbl call -> FunDef Slot lbl call
--     assignSlots (FunDef args ret nm body) = flip evalState (mempty, 0, 0) $ do
--       args' <- (traverse . _1) arg args
--       body' <- blkVars decl body
--       pure $ FunDef args' ret nm body'
--       where
--         arg :: VarID -> State (Map VarID Slot, Int, Int) Slot
--         arg td =
--           use (_1 . at td) >>= \case
--             Just slot -> pure slot
--             Nothing -> state $ \(m, na, nd) ->
--               let slot = Argument na
--                in (slot, (M.insert td slot m, na + 1, nd))
--         decl :: VarID -> State (Map VarID Slot, Int, Int) Slot
--         decl td =
--           use (_1 . at td) >>= \case
--             Just slot -> pure slot
--             Nothing -> state $ \(m, na, nd) ->
--               let slot = Local nd
--                in (slot, (M.insert td slot m, na, nd + 1))

--     mkTempFunction = do
--       tempId <- fresh
--       let temp = TempFunc funDef (transDeps ^. depTempFuncs)
--           deps = transDeps & depTempFuncs .~ M.singleton tempId temp
--       pure (deps, Left tempId)

--     closeTempFunction =
--       let callGraph = TempFunc funDef (transDeps ^. depTempFuncs)
--           (guid, deps) = close recDepth callGraph
--        in (Dependencies (deps <> view depKnownFuncs transDeps) mempty, Right guid)

--     -- Renumber all temporary function call id's from 0 for deduplication purposes
--     normalize :: TempFunc -> TempFunc
--     normalize = flip evalState (mempty, 0) . tempCalls call
--       where
--         call :: TempFuncID -> State (Map TempFuncID Int, Int) TempFuncID
--         call old =
--           use (_1 . at old) >>= \case
--             Just new -> pure $ TempFuncID new
--             Nothing -> state $ \(m, next) -> (TempFuncID next, (M.insert old next m, next + 1))
--         mapWithKeys :: Ord k' => Applicative m => (k -> m k') -> (v -> m v') -> (Map k v -> m (Map k' v'))
--         mapWithKeys fk fv m = M.fromList <$> traverse (bitraverse fk fv) (M.toList m)
--         tempCalls :: Traversal' TempFunc TempFuncID
--         tempCalls f = go
--           where
--             go (TempFunc fn deps) = TempFunc <$> (funCalls . precallTemp) f fn <*> mapWithKeys f go deps

--     close :: RecIndex -> TempFunc -> (GUID, Map GUID (FunDef Slot LabelID GUID))
--     close depth tf@(TempFunc funDef deps) =
--       let guid = GUID $ hash (normalize tf)
--           replaceSelf (CallRec n) | n == depth = CallKnown guid
--           replaceSelf c = c

--           deps' = over (traverse . tempFuncs . funCalls) replaceSelf deps

--           children :: Map TempFuncID (GUID, Map GUID (FunDef Slot LabelID GUID))
--           children = fmap (close (depth + 1)) deps'

--           funDef' :: FunDef Slot LabelID GUID
--           funDef' = assignSlots funDef & funCalls %~ replaceAll

--           replaceAll :: PreCall -> GUID
--           replaceAll (CallRec n) | n == depth = guid
--           replaceAll (CallTemp t) | Just (guid, _) <- M.lookup t children = guid
--           replaceAll (CallKnown guid) = guid
--           replaceAll _ = error "impossible" -- TODO throwError
--           transDeps :: Map GUID (FunDef Slot LabelID GUID)
--           transDeps = fold (toListOf (traverse . _2) children)
--        in (guid, transDeps & at guid ?~ funDef')

--     unresolvedCalls = toListOf selfCalls funDef ++ toListOf (depTempFuncs . traverse . tempFuncs . selfCalls) transDeps
--     selfCalls :: Traversal' (FunDef VarID LabelID PreCall) RecIndex
--     selfCalls = funCalls . precallRec

-- compileStmt ::
--   [Stmt Name Name (Maybe Expr) Expr] ->
--   RTEval [Stmt VarID LabelID TypeVar (RTExpr VarID LabelID PreCall TypeVar TypeVar)]
-- compileStmt (Return expr : r) = do
--   tv <-
--     view envFnStack >>= \case
--       (_, rtv) : _ -> pure rtv
--       _ -> throwError "returning but no return type found?"
--   liftP2
--     (\e' r' -> Return e' : r')
--     (atVar tv $ compileExpr expr)
--     (compileStmt r)
-- compileStmt (Break mname mexpr : r) = do
--   mlbl <- traverse resolveToBlockLabel mname
--   tv <- case mlbl of
--     Just (_, ltv) -> pure ltv
--     Nothing ->
--       view envBlockVar >>= \case
--         Just ltv -> pure ltv
--         _ -> throwError "No current block type variable?"
--   liftP2
--     (\e' r' -> Break (fst <$> mlbl) e' : r')
--     ( case mexpr of
--         Just expr -> Just <$> atVar tv (compileExpr expr)
--         Nothing -> Nothing <$ setType tv TVoid
--     )
--     (compileStmt r)
-- compileStmt (Continue mname : r) =
--   liftP2
--     (\ml' r' -> Continue (fst <$> ml') : r')
--     (traverse resolveToBlockLabel mname)
--     (compileStmt r)
-- compileStmt (Decl name mtypExpr expr : r) = do
--   tv <- lift fresh
--   liftP3
--     (\_ e' (tmpid, r') -> Decl tmpid tv e' : r')
--     (lift $ forM_ mtypExpr (evalType >=> setType tv))
--     (atVar tv $ compileExpr expr)
--     (bindRtvar name tv (compileStmt r))
-- compileStmt (Assign name expr : r) = do
--   (name', tv) <- resolveToRuntimeVar name
--   liftP2
--     (\e' r' -> Assign name' e' : r')
--     (atVar tv $ compileExpr expr)
--     (compileStmt r)
-- compileStmt (ExprStmt expr : r) = do
--   tv <- fresh
--   liftP2
--     (\e' r' -> ExprStmt e' : r')
--     (atVar tv $ compileExpr expr)
--     (compileStmt r)
-- compileStmt [] = pure []

-- compileBlock ::
--   Block Name Name (Maybe Expr) Expr ->
--   RTEval (RTBlock VarID LabelID PreCall TypeVar)
-- compileBlock (Block mlbl stmts typ) = do
--   tv <-
--     view envExprVar >>= \case
--       Just tv -> pure tv
--       Nothing -> fresh
--   forM_ typ (lift . evalType >=> setType tv)
--   -- TODO the envExprVar shouldn't be necessary
--   (mtid, stmts') <-
--     local ((envExprVar .~ Nothing) . (envBlockVar ?~ tv)) $
--       case mlbl of
--         Nothing -> (Nothing,) <$> compileStmt stmts
--         Just lbl -> bindLabel lbl tv $ \tid -> (Just tid,) <$> compileStmt stmts
--   pure (Block mtid stmts' tv)

-- askVar :: (MonadError String m, MonadReader Environment m) => m TypeVar
-- askVar =
--   view envExprVar >>= \case
--     Nothing -> throwError "No type variable defined for the current expression"
--     Just tv -> pure tv

-- setLocalType :: (MonadIO m, MonadError String m, MonadReader Environment m) => Type -> m ()
-- setLocalType typ = askVar >>= \tv -> setType tv typ

-- atVar :: MonadReader Environment m => TypeVar -> m a -> m a
-- atVar tv = local (envExprVar ?~ tv)

-- atType :: Type -> RTEval a -> RTEval a
-- atType typ m = do
--   tv <- tvar typ
--   atVar tv m

-- compileExpr ::
--   Expr ->
--   RTEval (RTExpr VarID LabelID PreCall TypeVar TypeVar)
-- compileExpr (BinExpr (ArithOp op) a b) =
--   liftP3
--     (RTBin (ArithOp op))
--     (compileExpr a)
--     (compileExpr b)
--     askVar
-- compileExpr (BinExpr (CompOp op) a b) = do
--   setLocalType TBool
--   tvExp <- fresh
--   liftP3
--     (RTBin (CompOp op))
--     (atVar tvExp $ compileExpr a)
--     (atVar tvExp $ compileExpr b)
--     askVar
-- compileExpr (BinExpr Concat _ _) = throwError "concatenation doesn't work for runtime expressions"
-- compileExpr (Cond cond t f) =
--   liftP4
--     RTCond
--     (atType TBool $ compileExpr cond)
--     (compileExpr t)
--     (compileExpr f)
--     askVar
-- compileExpr (Var n) = lookupVar n (\tid -> lift (force tid) >>= compileVal)
-- compileExpr (App f x) = do
--   -- TODO Nothing here uses `par`, or `liftP`, which means that this is not as
--   -- parallel as it potentially could be. Let's first see a test case though.
--   tf <- lift $ deferExpr f
--   lift (force tf) >>= \case
--     VFunc tdeps funId ->
--       case x of
--         List argExprs -> do
--           (argVars, retVar) <- lift $ callSig tdeps (either CallTemp CallKnown funId)
--           view envExprVar >>= \case
--             Nothing -> error "not in an expr" -- TODO
--             Just var -> unify_ var retVar
--           tell tdeps
--           rtArgExprs <-
--             safeZipWithM
--               (throwError "Argument length mismatch")
--               (\expr tv -> atVar tv $ compileExpr expr)
--               (toList argExprs)
--               argVars
--           pure $ RTCall (either CallTemp CallKnown funId) rtArgExprs retVar
--         _ -> throwError "Trying to call a function with a non-list-like-thing"
--     VSelf n ->
--       case x of
--         List argExprs -> do
--           (argVars, retVar) <- lift $ callSig mempty (CallRec n)
--           view envExprVar >>= \case
--             Nothing -> error "not in an expr" -- TODO
--             Just var -> unify_ var retVar
--           rtArgExprs <-
--             safeZipWithM
--               (throwError "Argument length mismatch")
--               (\expr tv -> atVar tv $ compileExpr expr)
--               (toList argExprs)
--               argVars
--           pure $ RTCall (CallRec n) rtArgExprs retVar
--         _ -> throwError "Trying to call a function with a non-list-like-thing"
--     val -> lift (deferExpr x >>= stepApp val) >>= compileVal
-- compileExpr (Acc field attrExpr) = do
--   fieldType <- askVar
--   structType <- fresh
--   -- TODO when compileVal is lazier, don't evaluate this as deeply
--   -- TODO unify type stuff for fieldType and structType
--   rtExpr <- atVar structType (compileExpr attrExpr)
--   lift (getTypeSuspend structType) >>= \case
--     TStruct fieldTypes ->
--       case M.lookup field fieldTypes of
--         Nothing -> throwError $ "struct type did not contain field " <> show field
--         Just t -> RTAccessor rtExpr field fieldType <$ setType fieldType t
--     _ -> throwError "accessing field of something that's not a struct"
-- compileExpr (Attr binds) = do
--   binds' <- lift $ stepAttrs binds
--   tv <- askVar
--   typ <- lift $ getTypeSuspend tv
--   rtFields <- mkStruct (\t -> lift (force t) >>= compileVal) typ binds'
--   pure $ RTStruct rtFields tv
-- compileExpr expr@Prim {} = lift (step expr) >>= compileVal -- TODO remove the step here, keep things runtime
-- compileExpr expr@BlockExpr {} = lift (step expr) >>= compileVal -- TODO remove the step here, keep things runtime
-- compileExpr _ = throwError "invalid runtime expression"

-- varsFromSig :: [Type] -> Type -> Eval ([TypeVar], TypeVar)
-- varsFromSig args ret = (,) <$> traverse tvar args <*> tvar ret

-- -- TODO split into selfcallsig and id call sig, same as it's actually used
-- callSig :: Dependencies -> PreCall -> Eval ([TypeVar], TypeVar)
-- callSig deps (CallKnown guid) = do
--   case deps ^. depKnownFuncs . at guid of
--     Nothing -> error "impossible" -- TODO throwError
--     Just fn -> varsFromSig (fn ^.. fnArgs . traverse . _2) (fn ^. fnRet)
-- callSig deps (CallTemp tid) =
--   case deps ^. depTempFuncs . at tid of
--     Nothing -> throwError "impossible"
--     Just (TempFunc fn _) -> varsFromSig (fn ^.. fnArgs . traverse . _2) (fn ^. fnRet)
-- callSig _ (CallRec n) = do
--   -- TODO clean this up, views envFnStack many times
--   depth <- view (envFnStack . to length)
--   -- TODO not a fan of the (-1) here
--   view (envFnStack . to (safeLookup (depth - n -1))) >>= \case
--     Nothing -> throwError $ "impossible: " <> show (n, depth, depth - n)
--     Just t -> pure (first (fmap snd) t)

-- safeLookup :: Int -> [a] -> Maybe a
-- safeLookup 0 (a : _) = Just a
-- safeLookup n' (_ : as) = safeLookup (n' -1) as
-- safeLookup _ _ = Nothing

-- -- TODO argument length mismatch tests
-- safeZipWithM :: Applicative m => m [c] -> (a -> b -> m c) -> [a] -> [b] -> m [c]
-- safeZipWithM err f as bs
--   | length as /= length bs = err
--   | otherwise = zipWithM f as bs

-- mkLit :: Type -> Prim -> RTEval RTLiteral
-- mkLit TInt (PInt n) = pure $ RTInt n
-- mkLit TDouble (PInt n) = pure $ RTDouble (fromIntegral n)
-- mkLit TDouble (PDouble n) = pure $ RTDouble n
-- mkLit TBool (PBool b) = pure $ RTBool b
-- mkLit t p = throwError $ "Cannot instantiate literal " <> show p <> " at type " <> show t

-- mkStruct :: (a -> RTEval b) -> Type -> Map Name a -> RTEval (Map Name b)
-- mkStruct f (TStruct fields) attrs = flip M.traverseWithKey fields $ \fieldName typ ->
--   case M.lookup fieldName attrs of
--     Just val -> atType typ (f val)
--     _ -> throwError $ "Field " <> show fieldName <> " not present in the supplied struct"
-- mkStruct _ t _ = throwError $ "Cannot create a type " <> show t <> " from a attrset literal"

-- -- TODO this value can be lazy, that way structs only evaluate relevant members
-- compileVal :: LazyValue -> RTEval (RTExpr VarID LabelID PreCall TypeVar TypeVar)
-- compileVal (VPrim p) = do
--   tv <- askVar
--   typ <- lift $ getTypeSuspend tv
--   lit <- mkLit typ p
--   pure $ RTLiteral lit tv
-- compileVal (VAttr m) = do
--   tv <- askVar
--   typ <- lift $ getTypeSuspend tv
--   s <- mkStruct (\tid -> lift (force tid) >>= compileVal) typ m
--   pure $ RTStruct s tv
-- compileVal (VRTVar tpid) =
--   view (envRTVar tpid) >>= \case
--     Nothing -> throwError "variable not in scope"
--     Just tv -> RTVar tpid tv <$ (askVar >>= unify_ tv) -- TODO Combine with Var handling as expr on line 375 somehow
-- compileVal VClosure {} = throwError "partially applied closure in runtime expression"
-- compileVal VClosure' {} = throwError "partially applied closure' in runtime expression"
-- compileVal VSelf {} = throwError "naked `self` in runtime expression"
-- compileVal VList {} = throwError "can't handle list values yet"
-- compileVal VType {} = throwError "can't handle type"
-- compileVal VFunc {} = throwError "Function values don't make sense here"
-- compileVal VBlockLabel {} = throwError "Block labels don't make sense here"
-- compileVal (VBlock env blk) = do
--   blk' <- local (staticEnv .~ env) $ compileBlock blk
--   tv <- askVar
--   unify_ tv (blk' ^. blkType)
--   pure $ RTBlock blk' tv

-- TODO Builtins are in this module only because matchType -> stepApp -> step -> Expr
mkBuiltins :: Eval ThunkID
mkBuiltins = do
  ts <- traverse sequenceA binds
  deferVal . VAttr $ M.fromList ts
  where
    binds :: [(Name, Eval ThunkID)]
    binds =
      [ ("types", deferAttrs (fmap VType <$> types)),
        ("struct", fn1 bStruct),
        -- ("typeOf", fn1 bTypeOf),
        ("matchType", fn2 matchType),
        ("error", fn1 bError),
        -- ("import", fn1 (bImport step)),
        ("attrNames", fn1 attrNames),
        ("listToAttrs", fn1 bListToAttrs),
        ("lookup", fn2 bLookup),
        ("index", fn2 bIndex),
        ("length", fn1 bLength)
      ]
    types :: [(Name, Type)]
    types =
      [ ("int", TInt),
        ("double", TDouble),
        ("void", TVoid),
        ("bool", TBool)
      ]
    fn1 :: Builtin1 -> Eval ThunkID
    fn1 f = deferVal $ VClosure' $ \de t -> force t >>= f de
    fn2 :: Builtin2 -> Eval ThunkID
    fn2 f = deferVal $
      VClosure' $ \_ t1 ->
        pure $
          VClosure' $ \_ t2 -> do
            v1 <- force t1
            v2 <- force t2
            f v1 v2

-- TODO _is_ this a type? i.e. church-encode types?
matchType :: Builtin2
matchType (VAttr attrs) (VType typ) =
  let getAttr attr = case M.lookup attr attrs of
        Just x -> pure x
        Nothing -> case M.lookup "default" attrs of
          Nothing -> throwError $ "Attr " <> show attr <> " not present, no default case"
          Just x -> pure x
   in case typ of
        TInt -> getAttr "int" >>= force
        TDouble -> getAttr "double" >>= force
        TVoid -> getAttr "void" >>= force
        TBool -> getAttr "bool" >>= force
        TStruct fields -> do
          k <- getAttr "struct" >>= force
          -- TODO re-deferring the fields of a fully evaluated type here is kinda ugly
          fields' <- traverse (deferVal . VType) fields >>= deferVal . VAttr
          stepApp k fields' emptyEE
matchType (VAttr _) val = throwError $ "second argument to matchType was a " <> describeValue val <> ", but expected a type"
matchType val _ = throwError $ "first argument to matchType was a " <> describeValue val <> ", but expected an attribute set"

-- bTypeOf :: LazyValue -> Eval LazyValue
-- bTypeOf (VRTVar tid) =
--   view (envRTVar tid) >>= \case
--     Just tv -> VType <$> getTypeSuspend tv
--     Nothing -> throwError "Cannot get type of this var"
-- bTypeOf (VBlockLabel tid) =
--   view (envRTLabel tid) >>= \case
--     Just tv -> VType <$> getTypeSuspend tv
--     Nothing -> throwError "Cannot get type of this label"
-- bTypeOf (VBlock env blk) = do
--   (blk', _) <- local (staticEnv .~ env) $ runWriterT $ compileBlock blk
--   let tv = blk' ^. blkType
--   VType <$> getTypeSuspend tv
-- bTypeOf val = throwError $ "Cannot get bTypeOf of a " <> describeValue val
