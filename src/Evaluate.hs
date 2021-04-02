{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Evaluate (ValueF (..), Value, eval, Dependencies (..)) where

import Control.Monad.Except
import Control.Monad.RWS as RWS
import Control.Monad.Writer
import Coroutine
import Data.Foldable
import Data.Hashable
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Eval
import Expr
import Lens.Micro.Platform
import Parse (pToplevel)
import Program
import System.FilePath
import Text.Megaparsec qualified as MP

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
step (Var x) = lookupVar x force (pure . VRTVar x) (pure . VBlockLabel x) (pure . VSelf)
step (Lam arg body) = VClosure arg body <$> view staticEnv
step (Let binds body) = do
  n0 <- use thunkSource
  let predictedThunks = zip (fst <$> binds) [n0 ..]
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
step (Attr m) = VAttr <$> M.traverseWithKey (\name expr -> withName name $ deferExpr expr) m
step (Acc f em) =
  step em >>= \case
    VAttr m ->
      case M.lookup f m of
        Nothing -> throwError $ "field " <> f <> " not present"
        Just tid -> force tid
    _ -> throwError "Accessing field of not an attribute set"
step (BlockExpr b) = fmap (uncurry VBlock) $ do
  tv <-
    view envExprVar >>= \case
      Just tv -> pure tv
      Nothing -> fresh
  local ((envExprVar .~ Nothing) . (envBlockVar ?~ tv)) $
    case b ^. blkLabel of
      Nothing -> genBlock b
      Just lbl ->
        local
          (envBinds . at lbl ?~ BBlockLabel tv)
          (genBlock b)
step (List l) = VList <$> traverse deferExpr l
step (With bind body) = do
  step bind >>= \case
    VAttr m -> local (bindThunks (M.toList m)) (step body)
    _ -> throwError "Expression in `with` expression did not evaluate to an attrset"
step (Cond cond tr fl) = do
  step cond >>= \case
    VPrim (PBool True) -> step tr
    VPrim (PBool False) -> step fl
    _ -> throwError "Did not evaluate to a boolean"
step (Func args ret bodyExpr) = do
  argVars <- (traverse . traverse) (evalType >=> tvar) args
  retVar <- evalType ret >>= tvar
  recDepth <- view envFnDepth
  -- the scope of `local` here encompasses the construction of the final function,
  -- but the `localState` only contains the evaluation of the nested body
  -- TODO make `local` scope smaller
  local (functionBodyEnv argVars retVar) $ do
    localState (tempSource .~ 0) (step bodyExpr) >>= \case
      VBlock deps body -> do
        unify_ retVar (body ^. blkType)
        name <- view $ envName . to (fromMaybe "fn")
        let getTypeVoid = getType (pure TVoid)
        body' <- typecheckFunction getTypeVoid body
        argsTys <- (traverse . traverse) getTypeVoid argVars
        retTy <- getTypeVoid retVar
        let funDef = FunDef argsTys retTy name body'
        uncurry VFunc <$> mkFunction freshTempId recDepth deps funDef
      _ -> throwError "Function body did not evaluate to a block expression"

typecheckFunction ::
  (TypeVar -> Eval Type) ->
  RTBlock PreCall TypeVar ->
  Eval (RTBlock PreCall Type)
typecheckFunction getType (Block lbl stmts typ) = do
  stmts' <- (traverse . types) getType stmts
  typ' <- getType typ
  pure $ Block lbl stmts' typ'
  where
    types :: Traversal (Stmt typ (RTExpr call typ typ)) (Stmt typ' (RTExpr call typ' typ')) typ typ'
    types f = stmtMasterTraversal (rtExprMasterTraversal pure f f pure pure) f pure

evalType :: Expr -> Eval Type
evalType expr =
  step expr >>= \case
    VType tp -> pure tp
    _ -> throwError "Expected type, got not-a-type"

mkFunction ::
  Monad m =>
  m TempID ->
  RecIndex ->
  Dependencies ->
  FunDef PreCall ->
  m (Dependencies, Either TempID GUID)
mkFunction fresh recDepth transDeps funDef =
  case unresolvedCalls of
    [] -> pure mkClosedFunction
    l | any (< recDepth) l -> mkTempFunction
    _ -> pure closeTempFunction
  where
    mkClosedFunction =
      let unPreCall (CallKnown guid) = guid
          unPreCall _ = error "impossible" -- TODO throwError
          funDef' = funDef & funCalls %~ unPreCall
          guid = GUID $ hash funDef'
          deps = transDeps & depKnownFuncs . at guid ?~ funDef'
       in (deps, Right guid)

    mkTempFunction = do
      tempId <- fresh
      let temp = TempFunc funDef (transDeps ^. depTempFuncs)
          deps = transDeps & depTempFuncs .~ M.singleton tempId temp
      pure (deps, Left tempId)

    closeTempFunction =
      let (guid, deps) = close recDepth (TempFunc funDef (transDeps ^. depTempFuncs))
       in (Dependencies (deps <> view depKnownFuncs transDeps) mempty, Right guid)

    close :: RecIndex -> TempFunc -> (GUID, Map GUID (FunDef GUID))
    close depth (TempFunc funDef deps) =
      let guid = GUID $ hash (deps, funDef)
          replaceSelf (CallRec n) | n == depth = CallKnown guid
          replaceSelf c = c

          deps' = over (traverse . tempFuncs . funCalls) replaceSelf deps

          children :: Map TempID (GUID, Map GUID (FunDef GUID))
          children = fmap (close (depth + 1)) deps'

          funDef' = funDef & funCalls %~ replaceAll

          replaceAll (CallRec n) | n == depth = guid
          replaceAll (CallTemp t) | Just (guid, _) <- M.lookup t children = guid
          replaceAll (CallKnown guid) = guid
          replaceAll _ = error "impossible" -- TODO throwError
          transDeps = fold (toListOf (traverse . _2) children)
       in (guid, transDeps & at guid ?~ funDef')

    unresolvedCalls = toListOf selfCalls funDef ++ toListOf (depTempFuncs . traverse . tempFuncs . selfCalls) transDeps
    selfCalls :: Traversal' (FunDef PreCall) RecIndex
    selfCalls = funCalls . precallRec

genStmt ::
  [Stmt (Maybe Expr) Expr] ->
  RTEval [Stmt TypeVar (RTExpr PreCall TypeVar TypeVar)]
genStmt (Return expr : r) = do
  tv <-
    view envFnStack >>= \case
      (_, rtv) : _ -> pure rtv
      _ -> throwError "returning but no return type found?"
  liftP2
    (\e' r' -> Return e' : r')
    (atVar tv $ rtFromExpr expr)
    (genStmt r)
genStmt (Break mname mexpr : r) = do
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
        Just expr -> Just <$> atVar tv (rtFromExpr expr)
        Nothing -> Nothing <$ setType tv TVoid
    )
    (genStmt r)
genStmt (Continue mname : r) =
  liftP2
    (\ml' r' -> Continue (fst <$> ml') : r')
    (traverse resolveToBlockLabel mname)
    (genStmt r)
genStmt (Decl name mtypExpr expr : r) = do
  tv <- lift fresh
  liftP3
    (\_ e' r' -> Decl name tv e' : r')
    (lift $ forM_ mtypExpr (evalType >=> setType tv))
    (atVar tv $ rtFromExpr expr)
    (local (envBinds %~ bindRtvar name tv) (genStmt r))
genStmt (Assign name expr : r) = do
  (name', tv) <- resolveToRuntimeVar name
  liftP2
    (\e' r' -> Assign name' e' : r')
    (atVar tv $ rtFromExpr expr)
    (genStmt r)
genStmt (ExprStmt expr : r) = do
  tv <- fresh
  liftP2
    (\e' r' -> ExprStmt e' : r')
    (atVar tv $ rtFromExpr expr)
    (genStmt r)
genStmt [] = pure []

genBlock ::
  Block (Maybe Expr) Expr ->
  Eval (Dependencies, RTBlock PreCall TypeVar)
genBlock (Block lbl stmts typ) = do
  tv <- view envBlockVar >>= maybe (throwError "impossible") pure
  (stmts', deps) <- runWriterT $ genStmt stmts
  forM_ typ (evalType >=> setType tv) -- Note that you currently cannot actually give a type expression for a block yet
  pure (deps, Block lbl stmts' tv)

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
  local (envExprVar ?~ tv) m

rtFromExpr ::
  Expr ->
  RTEval (RTExpr PreCall TypeVar TypeVar)
rtFromExpr (BinExpr (ArithOp op) a b) =
  liftP3
    (RTBin (ArithOp op))
    (rtFromExpr a)
    (rtFromExpr b)
    askVar
rtFromExpr (BinExpr (CompOp op) a b) = do
  setLocalType TBool
  tvExp <- fresh
  liftP3
    (RTBin (CompOp op))
    (atVar tvExp $ rtFromExpr a)
    (atVar tvExp $ rtFromExpr b)
    askVar
rtFromExpr (Cond cond t f) =
  liftP4
    RTCond
    (atType TBool $ rtFromExpr cond)
    (rtFromExpr t)
    (rtFromExpr f)
    askVar
rtFromExpr (Var n) =
  lookupVar
    n
    (lift . deepEval >=> rtFromVal)
    (\tv -> RTVar n tv <$ (askVar >>= unify_ tv))
    (const $ throwError "referencing block label as expression")
    (const $ throwError "referencing self variable as expression")
rtFromExpr (App f x) = do
  -- TODO Nothing here uses `par`, or `liftP`, which means that this is not as
  -- parallel as it potentially could be. Let's first see a test case though.
  tf <- lift $ deferExpr f
  lift (force tf) >>= \case
    VFunc tdeps funId ->
      case x of
        List argExprs -> do
          rtArgs <- traverse rtFromExpr (toList argExprs)
          tell tdeps
          (argVars, retVar) <- lift $ callSig tdeps (either CallTemp CallKnown funId)
          safeZipWithM_
            (throwError "Argument length mismatch")
            (\expr tv -> lift $ unify (rtInfo expr) tv)
            rtArgs
            argVars
          pure $ RTCall (either CallTemp CallKnown funId) rtArgs retVar
        _ -> throwError "Trying to call a function with a non-list-like-thing"
    VSelf n ->
      case x of
        List argExprs -> do
          rtArgs <- traverse rtFromExpr (toList argExprs)
          (argVars, retVar) <- lift $ callSig mempty (CallRec n)
          safeZipWithM_
            (throwError "Argument length mismatch")
            (\expr tv -> lift $ unify (rtInfo expr) tv)
            rtArgs
            argVars
          pure $ RTCall (CallRec n) rtArgs retVar
        _ -> throwError "Trying to call a function with a non-list-like-thing"
    val -> lift (deferExpr x >>= reduce val >>= traverse deepEval) >>= rtFromVal . Fix -- TODO a little ugly
rtFromExpr expr@BlockExpr {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@With {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@Acc {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@Lam {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@Prim {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@Let {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@Attr {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@List {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@Func {} = lift (deepEvalExpr expr) >>= rtFromVal

varsFromSig :: [Type] -> Type -> Eval ([TypeVar], TypeVar)
varsFromSig args ret = (,) <$> traverse tvar args <*> tvar ret

-- TODO split into selfcallsig and id call sig, same as it's actually used
callSig :: Dependencies -> PreCall -> Eval ([TypeVar], TypeVar)
callSig deps (CallKnown guid) = do
  case deps ^. depKnownFuncs . at guid of
    Nothing -> error "impossible" -- TODO throwError
    Just fn -> varsFromSig (fn ^.. fnArgs . traverse . _2) (fn ^. fnRet)
callSig _ (CallRec n) = do
  -- TODO clean this up, views envFnStack many times
  depth <- view (envFnStack . to length)
  -- TODO not a fan of the (-1) here
  view (envFnStack . to (safeLookup (depth - n -1))) >>= \case
    Nothing -> throwError $ "impossible: " <> show (n, depth, depth - n)
    Just t -> pure t
callSig deps (CallTemp tid) =
  case deps ^. depTempFuncs . at tid of
    Nothing -> throwError "impossible"
    Just (TempFunc fn _) -> varsFromSig (fn ^.. fnArgs . traverse . _2) (fn ^. fnRet)

safeLookup :: Int -> [a] -> Maybe a
safeLookup 0 (a : _) = Just a
safeLookup n' (_ : as) = safeLookup (n' -1) as
safeLookup _ _ = Nothing

-- TODO argument length mismatch tests
safeZipWithM_ :: Applicative m => m () -> (a -> b -> m c) -> [a] -> [b] -> m ()
safeZipWithM_ err f as bs
  | length as /= length bs = err
  | otherwise = zipWithM_ f as bs

rtLit :: Type -> Prim -> RTEval RTLiteral
rtLit TInt (PInt n) = pure $ RTInt n
rtLit TDouble (PInt n) = pure $ RTInt n
rtLit TDouble (PDouble n) = pure $ RTDouble n
rtLit TBool (PBool b) = pure $ RTBool b
rtLit t p = throwError $ "Cannot instantiate literal " <> show p <> " at type " <> show t

rtStruct :: Type -> Map Name Value -> RTEval (Map Name RTLiteral)
rtStruct (TStruct fields) attrs = flip M.traverseWithKey fields $ \fieldName typ ->
  case M.lookup fieldName attrs of
    Just val ->
      atType typ (rtFromVal val) >>= \case
        RTLiteral lit _ -> pure lit
        _ -> throwError "Expression did not evaluate to a literal"
    _ -> throwError $ "Field " <> fieldName <> " not present in the supplied struct"
rtStruct t _ = throwError $ "Cannot create a type " <> show t <> " from a attrset literal"

-- TODO this value can be lazy, that way structs only evaluate relevant members
rtFromVal :: Value -> RTEval (RTExpr PreCall TypeVar TypeVar)
rtFromVal (Fix (VPrim p)) = do
  tv <- askVar
  typ <- lift $ getTypeSuspend tv
  lit <- rtLit typ p
  pure $ RTLiteral lit tv
rtFromVal (Fix (VAttr m)) = do
  tv <- askVar
  typ <- lift $ getTypeSuspend tv
  s <- rtStruct typ m
  pure $ RTLiteral (RTStruct s) tv
rtFromVal (Fix (VRTVar n tv)) = pure $ RTVar n tv -- TODO PROPER RT VARIABLE HANDLING
rtFromVal (Fix VClosure {}) = throwError "partially applied closure in runtime expression"
rtFromVal (Fix VClosure' {}) = throwError "partially applied closure' in runtime expression"
rtFromVal (Fix VSelf {}) = throwError "naked `self` in runtime expression"
rtFromVal (Fix VList {}) = throwError "can't handle list values yet"
rtFromVal (Fix VType {}) = throwError "can't handle type"
rtFromVal (Fix VFunc {}) = throwError "Function values don't make sense here"
rtFromVal (Fix VBlockLabel {}) = throwError "Block labels don't make sense here"
rtFromVal (Fix (VBlock deps b)) = do
  -- TODO block typevar
  tell deps
  tv <- lift fresh
  pure $ RTBlock b tv

functionBodyEnv :: [(Name, TypeVar)] -> TypeVar -> Environment -> Environment
functionBodyEnv typedArgs ret (Environment (StaticEnv binds name fp) (DynamicEnv depth fns _ _)) =
  Environment (StaticEnv binds' name fp) (DynamicEnv depth' fns' Nothing (Just ret))
  where
    depth' = depth + 1
    fns' = (snd <$> typedArgs, ret) : fns
    binds' =
      flip (foldr (uncurry bindRtvar)) typedArgs
        . (at "self" ?~ BSelf depth)
        . M.filter isThunk
        $ binds

-- TODO Builtins are in this module only because matchType -> reduce -> step -> Expr
withBuiltins :: Eval a -> Eval a
withBuiltins m = do
  tUndefined <- deferM $ throwError "undefined"
  tNine <- deferVal . VPrim $ PInt 9
  tStruct <- deferVal $ VClosure' (force >=> struct)
  tTypeOf <- deferVal $ VClosure' (force >=> typeOf)
  tMatchType <- deferVal $ VClosure' (force >=> matchType)
  tError <- deferVal $ VClosure' (force >=> bError)
  tImport <- deferVal $ VClosure' (force >=> biImport)
  tTypes <-
    deferAttrs
      [ ("int", VType TInt),
        ("double", VType TDouble),
        ("void", VType TVoid),
        ("bool", VType TBool)
      ]
  tBuiltins <-
    deferVal . VAttr $
      M.fromList
        [ ("undefined", tUndefined),
          ("nine", tNine),
          ("types", tTypes),
          ("struct", tStruct),
          ("typeOf", tTypeOf),
          ("matchType", tMatchType),
          ("error", tError),
          ("import", tImport)
        ]
  local (bindThunk "builtins" tBuiltins) m

-- TODO this could be lazier if VClosure' were lazier
biImport :: ValueF ThunkID -> Eval (ValueF ThunkID)
biImport (VPrim (PString rel)) = do
  file <- view envFile
  let file' = takeDirectory file </> rel
  input <- liftIO $ readFile file'
  case MP.parse pToplevel file' input of
    Left err -> throwError $ MP.errorBundlePretty err
    Right expr -> local (envFile .~ file') $ step expr
biImport _ = throwError "import needs a filepath"

typeOf :: ValueF ThunkID -> Eval (ValueF ThunkID)
typeOf (VRTVar _ tv) = VType <$> getTypeSuspend tv
typeOf (VBlockLabel _ tv) = VType <$> getTypeSuspend tv
typeOf (VBlock _ blk) = VType <$> getTypeSuspend (blk ^. blkType)
typeOf _ = throwError "Cannot get typeOf"

bError :: ValueF ThunkID -> Eval (ValueF ThunkID)
bError (VPrim (PString msg)) = throwError $ "error: " <> msg
bError _ = throwError "builtins.error was passed a non-string"

getTypeSuspend :: TypeVar -> Eval Type
getTypeSuspend tv = go retries
  where
    retries = 10
    go :: Int -> Eval Type
    go 0 = throwError "Underdetermined type variable"
    go n = getType (suspend $ go (n -1)) tv

struct :: ValueF ThunkID -> Eval (ValueF ThunkID)
struct (VAttr m) = do
  let forceType tid = do
        force tid >>= \case
          (VType t) -> pure t
          _ -> throwError "Struct member was not a type expression"
  types <- traverse forceType m
  pure $ VType $ TStruct types
struct _ = throwError "Making a struct typedef from something that's not an attrset"

-- TODO _is_ this a type? i.e. church-encode types?
matchType :: ValueF ThunkID -> Eval (ValueF ThunkID)
matchType (VType typ) =
  pure $
    VClosure' $
      force >=> \case
        VAttr attrs ->
          let getAttr attr = case M.lookup attr attrs of
                Just x -> pure x
                Nothing -> case M.lookup "default" attrs of
                  Nothing -> throwError $ "Attr " <> attr <> " not present, no default case"
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
                  reduce k fields'
        _ -> throwError "matchType did not get an attr of type matchers"
matchType _ = throwError "matching on not-a-type"
