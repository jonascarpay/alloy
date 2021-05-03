{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Evaluate (ValueF (..), Value, eval, Dependencies (..)) where

import Builtins
import Control.Monad.Except
import Control.Monad.RWS as RWS
import Control.Monad.State (State, evalState)
import Control.Monad.Writer
import Coroutine
import Data.Bifunctor
import Data.Bitraversable
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
eval fp eRoot = runEval fp $ withBuiltins $ deepEvalExpr eRoot

deferExpr :: Expr -> Eval ThunkID
deferExpr expr = do
  env <- view staticEnv
  deferM $ local (staticEnv .~ env) (step expr)

-- TODO once we only have non-Expr closures, this can be moved to Eval
reduce :: ValueF ThunkID -> ThunkID -> Eval (ValueF ThunkID)
reduce (VClosure arg body env) tid = do
  local (staticEnv .~ env) $
    local (bindThunk arg tid) (step body)
reduce (VClosure' m) tid = m tid -- FIXME this will eventually break since it does not properly handle the environment
reduce _ _ = throwError "Calling a non-function"

step :: Expr -> Eval (ValueF ThunkID)
step (Prim p) = pure $ VPrim p
step (App f x) = do
  tf <- step f
  tx <- deferExpr x
  reduce tf tx
step (Var x) =
  lookupVar
    x
    force
    (\tid _ -> pure $ VRTVar tid)
    (\tid _ -> pure $ VBlockLabel tid)
    (pure . VSelf)
step (Lam arg body) = VClosure arg body <$> view staticEnv
step (Let binds body) = do
  n0 <- use idSource
  let predictedThunks = zipWith (\name n -> (name, ThunkID n)) (fst <$> binds) [n0 ..]
  local (bindThunks predictedThunks) $ do
    forM_ binds $ \(name, expr) -> withName name $ deferExpr expr
    step body
step (BinExpr bop a b) = do
  va <- step a
  vb <- step b
  case (bop, va, vb) of
    (ArithOp op, VPrim (PInt pa), VPrim (PInt pb)) -> pure . VPrim . PInt $ arith op pa pb
    (ArithOp op, VPrim (PDouble pa), VPrim (PDouble pb)) -> pure . VPrim . PDouble $ arith op pa pb
    (ArithOp _, _, _) -> throwError "Arithmetic on not a pair of numbers"
    (CompOp op, VPrim (PInt pa), VPrim (PInt pb)) -> pure . VPrim . PBool $ comp op pa pb
    (CompOp op, VPrim (PDouble pa), VPrim (PDouble pb)) -> pure . VPrim . PBool $ comp op pa pb
    (CompOp Eq, VType ta, VType tb) -> pure . VPrim . PBool $ ta == tb
    (CompOp Neq, VType ta, VType tb) -> pure . VPrim . PBool $ ta /= tb
    (CompOp _, _, _) -> throwError "Cannot compare the thing you're trying to compare"
    (Concat, VList la, VList lb) -> pure $ VList $ la <> lb
    (Concat, VPrim (PString sa), VPrim (PString sb)) -> pure $ VPrim $ PString $ sa <> sb
    (Concat, _, _) -> throwError "Cannot concat the thing you're trying to concat"
step (Attr m) = do
  m' <- forM (M.toList m) $ \(key, expr) -> do
    tid <- withName key $ deferExpr expr
    pure (ordValueString key, tid)
  pure $ VAttr (M.fromList m')
step (Acc f em) =
  step em >>= \case
    VAttr m ->
      case M.lookup (ordValueString f) m of
        Nothing -> throwError $ "field " <> f <> " not present"
        Just tid -> force tid
    _ -> throwError $ unwords ["Accessing field", show f, "of not an attribute set"]
step (BlockExpr b) = do
  env <- view staticEnv
  pure $ VBlock env b
step (List l) = VList <$> traverse deferExpr l
step (With bind body) = do
  step bind >>= \case
    VAttr m -> local (bindThunks (M.toList $ namedAttrs m)) (step body) -- TODO namedAttrs constructs a map only to have it turned back into a list
    _ -> throwError "Expression in `with` expression did not evaluate to an attrset"
step (Cond cond tr fl) = do
  step cond >>= \case
    VPrim (PBool True) -> step tr
    VPrim (PBool False) -> step fl
    _ -> throwError "Did not evaluate to a boolean"
step (Func args ret bodyExpr) = do
  args' <- forM args $ \(name, expr) -> do
    typ <- evalType expr
    tid <- freshVarId
    tv <- tvar typ
    pure (name, typ, tid, tv)
  retType <- evalType ret
  retVar <- tvar retType
  recDepth <- length <$> view envFnStack
  name <- view $ envName . to (fromMaybe "fn")
  (blk, deps) <-
    local (functionBodyEnv args' retVar) $
      step bodyExpr >>= \case
        VBlock env blk -> local (staticEnv .~ env) $ runWriterT $ compileBlock blk
        _ -> throwError "Function body did not evaluate to a block expression"
  let getTypeVoid = getType (pure tVoid)
  typedBlk <- typecheckFunction getTypeVoid blk
  let farg (_, typ, tid, _) = (tid, typ)
      funDef = FunDef (farg <$> args') retType name typedBlk
  uncurry VFunc <$> mkFunction freshFuncId recDepth deps funDef

functionBodyEnv :: [(Name, typ, VarID, TypeVar)] -> TypeVar -> Environment -> Environment
functionBodyEnv typedArgs ret env =
  Environment
    (StaticEnv ctx' ctxName fp)
    (DynamicEnv fns' Nothing (Just ret) (M.fromList dyn) mempty)
  where
    Environment (StaticEnv ctx ctxName fp) (DynamicEnv fns _ _ _ _) = env
    (argBindings, dyn, namedArgs) = unzip3 $ fmap f typedArgs
    f (nm, _, tempid, tv) = ((nm, BRTVar tempid), (tempid, tv), (nm, tv))
    depth = length fns
    ctx' = M.fromList argBindings <> M.singleton "self" (BSelf depth) <> ctx
    fns' = (namedArgs, ret) : fns

typecheckFunction ::
  (TypeVar -> Eval Type) ->
  RTBlock var lbl call TypeVar ->
  Eval (RTBlock var lbl call Type)
typecheckFunction getType (Block lbl stmts typ) = do
  stmts' <- (traverse . types) getType stmts
  typ' <- getType typ
  pure $ Block lbl stmts' typ'
  where
    types ::
      Traversal
        (Stmt var lbl typ1 (RTExpr var lbl call typ1 typ1))
        (Stmt var lbl typ2 (RTExpr var lbl call typ2 typ2))
        typ1
        typ2
    types f = stmtMasterTraversal pure pure f (rtExprMasterTraversal pure pure pure f f pure)

evalType :: Expr -> Eval Type
evalType expr =
  step expr >>= \case
    VType tp -> pure tp
    _ -> throwError "Expected type, got not-a-type"

mkFunction ::
  Monad m =>
  m TempFuncID ->
  RecIndex ->
  Dependencies ->
  FunDef VarID LabelID PreCall ->
  m (Dependencies, Either TempFuncID GUID)
mkFunction fresh recDepth transDeps funDef =
  case unresolvedCalls of
    [] -> pure mkClosedFunction
    l | any (< recDepth) l -> mkTempFunction
    _ -> pure closeTempFunction
  where
    mkClosedFunction :: (Dependencies, Either TempFuncID GUID)
    mkClosedFunction =
      let unPreCall (CallKnown guid) = guid
          unPreCall _ = error "impossible" -- TODO throwError
          funDef' :: FunDef Slot LabelID GUID
          funDef' =
            funDef & funCalls %~ unPreCall
              & assignSlots
          guid = GUID $ hash funDef'
          deps = transDeps & depKnownFuncs . at guid ?~ funDef'
       in (deps, Right guid)

    assignSlots :: FunDef VarID lbl call -> FunDef Slot lbl call
    assignSlots (FunDef args ret nm body) = flip evalState (mempty, 0, 0) $ do
      args' <- (traverse . _1) arg args
      body' <- blkVars decl body
      pure $ FunDef args' ret nm body'
      where
        arg :: VarID -> State (Map VarID Slot, Int, Int) Slot
        arg td =
          use (_1 . at td) >>= \case
            Just slot -> pure slot
            Nothing -> state $ \(m, na, nd) ->
              let slot = Argument na
               in (slot, (M.insert td slot m, na + 1, nd))
        decl :: VarID -> State (Map VarID Slot, Int, Int) Slot
        decl td =
          use (_1 . at td) >>= \case
            Just slot -> pure slot
            Nothing -> state $ \(m, na, nd) ->
              let slot = Local nd
               in (slot, (M.insert td slot m, na, nd + 1))

    mkTempFunction = do
      tempId <- fresh
      let temp = TempFunc funDef (transDeps ^. depTempFuncs)
          deps = transDeps & depTempFuncs .~ M.singleton tempId temp
      pure (deps, Left tempId)

    closeTempFunction =
      let callGraph = TempFunc funDef (transDeps ^. depTempFuncs)
          (guid, deps) = close recDepth callGraph
       in (Dependencies (deps <> view depKnownFuncs transDeps) mempty, Right guid)

    -- Renumber all temporary function call id's from 0 for deduplication purposes
    normalize :: TempFunc -> TempFunc
    normalize = flip evalState (mempty, 0) . tempCalls call
      where
        call :: TempFuncID -> State (Map TempFuncID Int, Int) TempFuncID
        call old =
          use (_1 . at old) >>= \case
            Just new -> pure $ TempFuncID new
            Nothing -> state $ \(m, next) -> (TempFuncID next, (M.insert old next m, next + 1))
        mapWithKeys :: Ord k' => Applicative m => (k -> m k') -> (v -> m v') -> (Map k v -> m (Map k' v'))
        mapWithKeys fk fv m = M.fromList <$> traverse (bitraverse fk fv) (M.toList m)
        tempCalls :: Traversal' TempFunc TempFuncID
        tempCalls f = go
          where
            go (TempFunc fn deps) = TempFunc <$> (funCalls . precallTemp) f fn <*> mapWithKeys f go deps

    close :: RecIndex -> TempFunc -> (GUID, Map GUID (FunDef Slot LabelID GUID))
    close depth tf@(TempFunc funDef deps) =
      let guid = GUID $ hash (normalize tf)
          replaceSelf (CallRec n) | n == depth = CallKnown guid
          replaceSelf c = c

          deps' = over (traverse . tempFuncs . funCalls) replaceSelf deps

          children :: Map TempFuncID (GUID, Map GUID (FunDef Slot LabelID GUID))
          children = fmap (close (depth + 1)) deps'

          funDef' :: FunDef Slot LabelID GUID
          funDef' = assignSlots funDef & funCalls %~ replaceAll

          replaceAll :: PreCall -> GUID
          replaceAll (CallRec n) | n == depth = guid
          replaceAll (CallTemp t) | Just (guid, _) <- M.lookup t children = guid
          replaceAll (CallKnown guid) = guid
          replaceAll _ = error "impossible" -- TODO throwError
          transDeps :: Map GUID (FunDef Slot LabelID GUID)
          transDeps = fold (toListOf (traverse . _2) children)
       in (guid, transDeps & at guid ?~ funDef')

    unresolvedCalls = toListOf selfCalls funDef ++ toListOf (depTempFuncs . traverse . tempFuncs . selfCalls) transDeps
    selfCalls :: Traversal' (FunDef VarID LabelID PreCall) RecIndex
    selfCalls = funCalls . precallRec

compileStmt ::
  [Stmt Name Name (Maybe Expr) Expr] ->
  RTEval [Stmt VarID LabelID TypeVar (RTExpr VarID LabelID PreCall TypeVar TypeVar)]
compileStmt (Return expr : r) = do
  tv <-
    view envFnStack >>= \case
      (_, rtv) : _ -> pure rtv
      _ -> throwError "returning but no return type found?"
  liftP2
    (\e' r' -> Return e' : r')
    (atVar tv $ compileExpr expr)
    (compileStmt r)
compileStmt (Break mname mexpr : r) = do
  mlbl <- traverse resolveToBlockLabel mname
  tv <- case mlbl of
    Just (_, ltv) -> pure ltv
    Nothing ->
      view envBlockVar >>= \case
        Just ltv -> pure ltv
        _ -> throwError "No current block type variable?"
  liftP2
    (\e' r' -> Break (fst <$> mlbl) e' : r')
    ( case mexpr of
        Just expr -> Just <$> atVar tv (compileExpr expr)
        Nothing -> Nothing <$ setType tv tVoid
    )
    (compileStmt r)
compileStmt (Continue mname : r) =
  liftP2
    (\ml' r' -> Continue (fst <$> ml') : r')
    (traverse resolveToBlockLabel mname)
    (compileStmt r)
compileStmt (Decl name mtypExpr expr : r) = do
  tv <- lift fresh
  liftP3
    (\_ e' (tmpid, r') -> Decl tmpid tv e' : r')
    (lift $ forM_ mtypExpr (evalType >=> setType tv))
    (atVar tv $ compileExpr expr)
    (bindRtvar name tv (compileStmt r))
compileStmt (Assign name expr : r) = do
  (name', tv) <- resolveToRuntimeVar name
  liftP2
    (\e' r' -> Assign name' e' : r')
    (atVar tv $ compileExpr expr)
    (compileStmt r)
compileStmt (ExprStmt expr : r) = do
  tv <- fresh
  liftP2
    (\e' r' -> ExprStmt e' : r')
    (atVar tv $ compileExpr expr)
    (compileStmt r)
compileStmt [] = pure []

compileBlock ::
  Block Name Name (Maybe Expr) Expr ->
  RTEval (RTBlock VarID LabelID PreCall TypeVar)
compileBlock (Block mlbl stmts typ) = do
  tv <-
    view envExprVar >>= \case
      Just tv -> pure tv
      Nothing -> fresh
  forM_ typ (lift . evalType >=> setType tv)
  -- TODO the envExprVar shouldn't be necessary
  (mtid, stmts') <-
    local ((envExprVar .~ Nothing) . (envBlockVar ?~ tv)) $
      case mlbl of
        Nothing -> (Nothing,) <$> compileStmt stmts
        Just lbl -> bindLabel lbl tv $ \tid -> (Just tid,) <$> compileStmt stmts
  pure (Block mtid stmts' tv)

-- TODO this technically creates an unnecessary thunk since we defer and then
-- immediately evaluate but I don't think we care
deepEvalExpr :: Expr -> Eval Value
deepEvalExpr = deferExpr >=> deepEval

askVar :: (MonadError String m, MonadReader Environment m) => m TypeVar
askVar =
  view envExprVar >>= \case
    Nothing -> throwError "No type variable defined for the current expression"
    Just tv -> pure tv

setLocalType :: (MonadIO m, MonadError String m, MonadReader Environment m) => Type -> m ()
setLocalType typ = askVar >>= \tv -> setType tv typ

atVar :: MonadReader Environment m => TypeVar -> m a -> m a
atVar tv = local (envExprVar ?~ tv)

atType :: Type -> RTEval a -> RTEval a
atType typ m = do
  tv <- tvar typ
  atVar tv m

compileExpr ::
  Expr ->
  RTEval (RTExpr VarID LabelID PreCall TypeVar TypeVar)
compileExpr (BinExpr (ArithOp op) a b) =
  liftP3
    (RTBin (ArithOp op))
    (compileExpr a)
    (compileExpr b)
    askVar
compileExpr (BinExpr (CompOp op) a b) = do
  setLocalType tBool
  tvExp <- fresh
  liftP3
    (RTBin (CompOp op))
    (atVar tvExp $ compileExpr a)
    (atVar tvExp $ compileExpr b)
    askVar
compileExpr (BinExpr Concat _ _) = throwError "concatenation doesn't work for runtime expressions"
compileExpr (Cond cond t f) =
  liftP4
    RTCond
    (atType tBool $ compileExpr cond)
    (compileExpr t)
    (compileExpr f)
    askVar
compileExpr (Var n) =
  lookupVar
    n
    (\tid -> lift (force tid) >>= compileVal)
    (\tpid tv -> RTVar tpid tv <$ (askVar >>= unify_ tv))
    (\_ _ -> throwError "referencing block label as expression")
    (const $ throwError "referencing self variable as expression")
compileExpr (App f x) = do
  -- TODO Nothing here uses `par`, or `liftP`, which means that this is not as
  -- parallel as it potentially could be. Let's first see a test case though.
  tf <- lift $ deferExpr f
  lift (force tf) >>= \case
    VFunc tdeps funId ->
      case x of
        List argExprs -> do
          (argVars, retVar) <- lift $ callSig tdeps (either CallTemp CallKnown funId)
          view envExprVar >>= \case
            Nothing -> error "not in an expr" -- TODO
            Just var -> unify_ var retVar
          tell tdeps
          rtArgExprs <-
            safeZipWithM
              (throwError "Argument length mismatch")
              (\expr tv -> atVar tv $ compileExpr expr)
              (toList argExprs)
              argVars
          pure $ RTCall (either CallTemp CallKnown funId) rtArgExprs retVar
        _ -> throwError "Trying to call a function with a non-list-like-thing"
    VSelf n ->
      case x of
        List argExprs -> do
          (argVars, retVar) <- lift $ callSig mempty (CallRec n)
          view envExprVar >>= \case
            Nothing -> error "not in an expr" -- TODO
            Just var -> unify_ var retVar
          rtArgExprs <-
            safeZipWithM
              (throwError "Argument length mismatch")
              (\expr tv -> atVar tv $ compileExpr expr)
              (toList argExprs)
              argVars
          pure $ RTCall (CallRec n) rtArgExprs retVar
        _ -> throwError "Trying to call a function with a non-list-like-thing"
    val -> lift (deferExpr x >>= reduce val) >>= compileVal
compileExpr (Acc field attrExpr) = do
  fieldType <- askVar
  structType <- fresh
  -- TODO when compileVal is lazier, don't evaluate this as deeply
  -- TODO unify type stuff for fieldType and structType
  rtExpr <- atVar structType (compileExpr attrExpr)
  lift (typeRep <$> getTypeSuspend structType) >>= \case
    RStruct fieldTypes ->
      case M.lookup field fieldTypes of
        Nothing -> throwError $ "struct type did not contain field " <> field
        Just t -> RTAccessor rtExpr field fieldType <$ setType fieldType t
    _ -> throwError "accessing field of something that's not a struct"
compileExpr (Attr fields) = do
  tv <- askVar
  typ <- lift $ getTypeSuspend tv
  rtFields <- mkStruct compileExpr (typeRep typ) (M.mapKeys ordValueString fields)
  pure $ RTStruct rtFields tv
compileExpr expr@Prim {} = lift (step expr) >>= compileVal -- TODO remove the step here, keep things runtime
compileExpr expr@BlockExpr {} = lift (step expr) >>= compileVal -- TODO remove the step here, keep things runtime
compileExpr _ = throwError "invalid runtime expression"

varsFromSig :: [Type] -> Type -> Eval ([TypeVar], TypeVar)
varsFromSig args ret = (,) <$> traverse tvar args <*> tvar ret

-- TODO split into selfcallsig and id call sig, same as it's actually used
callSig :: Dependencies -> PreCall -> Eval ([TypeVar], TypeVar)
callSig deps (CallKnown guid) = do
  case deps ^. depKnownFuncs . at guid of
    Nothing -> error "impossible" -- TODO throwError
    Just fn -> varsFromSig (fn ^.. fnArgs . traverse . _2) (fn ^. fnRet)
callSig deps (CallTemp tid) =
  case deps ^. depTempFuncs . at tid of
    Nothing -> throwError "impossible"
    Just (TempFunc fn _) -> varsFromSig (fn ^.. fnArgs . traverse . _2) (fn ^. fnRet)
callSig _ (CallRec n) = do
  -- TODO clean this up, views envFnStack many times
  depth <- view (envFnStack . to length)
  -- TODO not a fan of the (-1) here
  view (envFnStack . to (safeLookup (depth - n -1))) >>= \case
    Nothing -> throwError $ "impossible: " <> show (n, depth, depth - n)
    Just t -> pure (first (fmap snd) t)

safeLookup :: Int -> [a] -> Maybe a
safeLookup 0 (a : _) = Just a
safeLookup n' (_ : as) = safeLookup (n' -1) as
safeLookup _ _ = Nothing

-- TODO argument length mismatch tests
safeZipWithM :: Applicative m => m [c] -> (a -> b -> m c) -> [a] -> [b] -> m [c]
safeZipWithM err f as bs
  | length as /= length bs = err
  | otherwise = zipWithM f as bs

mkLit :: Repr -> Prim -> RTEval RTLiteral
mkLit RInt (PInt n) = pure $ RTInt n
mkLit RDouble (PInt n) = pure $ RTDouble (fromIntegral n)
mkLit RDouble (PDouble n) = pure $ RTDouble n
mkLit RBool (PBool b) = pure $ RTBool b
mkLit t p = throwError $ "Cannot instantiate literal " <> show p <> " at type " <> show t

mkStruct :: (a -> RTEval b) -> Repr -> Map OrdValue a -> RTEval (Map Name b)
mkStruct f (RStruct fields) attrs = flip M.traverseWithKey fields $ \fieldName typ ->
  case M.lookup (ordValueString fieldName) attrs of
    Just val -> atType typ (f val)
    _ -> throwError $ "Field " <> fieldName <> " not present in the supplied struct"
mkStruct _ t _ = throwError $ "Cannot create a type " <> show t <> " from a attrset literal"

-- TODO this value can be lazy, that way structs only evaluate relevant members
compileVal :: ValueF ThunkID -> RTEval (RTExpr VarID LabelID PreCall TypeVar TypeVar)
compileVal (VPrim p) = do
  tv <- askVar
  typ <- lift $ getTypeSuspend tv
  lit <- mkLit (typeRep typ) p
  pure $ RTLiteral lit tv
compileVal (VAttr m) = do
  tv <- askVar
  typ <- lift $ getTypeSuspend tv
  s <- mkStruct (\tid -> lift (force tid) >>= compileVal) (typeRep typ) m
  pure $ RTStruct s tv
compileVal (VRTVar tpid) =
  view (envRTVar tpid) >>= \case
    Nothing -> throwError "variable not in scope"
    Just tv -> RTVar tpid tv <$ (askVar >>= unify_ tv) -- TODO Combine with Var handling as expr on line 375 somehow
compileVal VClosure {} = throwError "partially applied closure in runtime expression"
compileVal VClosure' {} = throwError "partially applied closure' in runtime expression"
compileVal VSelf {} = throwError "naked `self` in runtime expression"
compileVal VList {} = throwError "can't handle list values yet"
compileVal VType {} = throwError "can't handle type"
compileVal VFunc {} = throwError "Function values don't make sense here"
compileVal VBlockLabel {} = throwError "Block labels don't make sense here"
compileVal (VBlock env blk) = do
  blk' <- local (staticEnv .~ env) $ compileBlock blk
  tv <- askVar
  unify_ tv (blk' ^. blkType)
  pure $ RTBlock blk' tv

-- TODO Builtins are in this module only because matchType -> reduce -> step -> Expr
withBuiltins :: Eval a -> Eval a
withBuiltins m = do
  ts <- traverse sequenceA binds
  tBuiltins <- deferVal . VAttr $ M.mapKeys ordValueString $ M.fromList ts -- TODO don't construct a map twice
  local (bindThunk "builtins" tBuiltins) m
  where
    binds :: [(Name, Eval ThunkID)]
    binds =
      [ ("types", deferAttrs (fmap VType <$> types)),
        ("struct", fn1 bStruct),
        ("typeOf", fn1 bTypeOf),
        ("matchType", fn2 matchType),
        ("error", fn1 bError),
        ("import", fn1 (bImport step)),
        ("attrNames", fn1 attrNames),
        ("listToAttrs", fn1 bListToAttrs),
        ("lookup", fn2 bLookup),
        ("index", fn2 bIndex),
        ("length", fn1 bLength)
      ]
    types :: [(Name, Type)]
    types =
      [ ("int", tInt),
        ("double", tDouble),
        ("void", tVoid),
        ("bool", tBool)
      ]
    fn1 :: (ValueF ThunkID -> Eval (ValueF ThunkID)) -> Eval ThunkID
    fn1 f = deferVal $ VClosure' (force >=> f)
    -- TODO express in terms of fn1
    fn2 :: (ValueF ThunkID -> ValueF ThunkID -> Eval (ValueF ThunkID)) -> Eval ThunkID
    fn2 f = deferVal $
      VClosure' $ \t1 ->
        pure $
          VClosure' $ \t2 -> do
            v1 <- force t1
            v2 <- force t2
            f v1 v2

-- TODO _is_ this a type? i.e. church-encode types?
matchType :: ValueF ThunkID -> ValueF ThunkID -> Eval (ValueF ThunkID)
matchType (VAttr attrs) (VType typ) =
  let getAttr attr = case M.lookup (ordValueString attr) attrs of
        Just x -> pure x
        Nothing -> case M.lookup (ordValueString "default") attrs of
          Nothing -> throwError $ "Attr " <> attr <> " not present, no default case"
          Just x -> pure x
   in case typeRep typ of
        RInt -> getAttr "int" >>= force
        RDouble -> getAttr "double" >>= force
        RVoid -> getAttr "void" >>= force
        RBool -> getAttr "bool" >>= force
        RStruct fields -> do
          k <- getAttr "struct" >>= force
          -- TODO re-deferring the fields of a fully evaluated type here is kinda ugly
          fields' <- traverse (deferVal . VType) (M.mapKeys ordValueString fields) >>= deferVal . VAttr
          reduce k fields'
matchType _ (VType _) = throwError "first argument to matchType was not an attr set"
matchType _ _ = throwError "matching on not-a-type"

bTypeOf :: ValueF ThunkID -> Eval (ValueF ThunkID)
bTypeOf (VRTVar tid) =
  view (envRTVar tid) >>= \case
    Just tv -> VType <$> getTypeSuspend tv
    Nothing -> throwError "Cannot get bTypeOf"
bTypeOf (VBlockLabel tid) =
  view (envRTLabel tid) >>= \case
    Just tv -> VType <$> getTypeSuspend tv
    Nothing -> throwError "Cannot get bTypeOf"
bTypeOf (VBlock env blk) = do
  (blk', _) <- local (staticEnv .~ env) $ runWriterT $ compileBlock blk
  let tv = blk' ^. blkType
  VType <$> getTypeSuspend tv
bTypeOf _ = throwError "Cannot get bTypeOf"
