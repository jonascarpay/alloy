{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Evaluate (ValueF (..), Value, eval, Dependencies (..)) where

import Control.Monad.Except
import Control.Monad.RWS as RWS
import Control.Monad.Writer
import Data.Foldable
import Data.Hashable
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Eval
import Expr
import Lens.Micro.Platform
import Program

eval :: Expr -> IO (Either String Value)
eval eRoot = runEval $ withBuiltins $ deepEvalExpr eRoot

deferExpr :: Expr -> Eval ThunkID
deferExpr expr = ask >>= \env -> deferM $ local (const env) (step expr)

-- TODO once we only have non-Expr closures, this can be moved to Eval
reduce :: ValueF ThunkID -> ThunkID -> Eval (ValueF ThunkID)
reduce (VClosure arg body env) tid = do
  local (ctx .~ bindThunk arg tid env) (step body)
reduce (VClosure' m) tid = m tid -- FIXME this will eventually break since it does not properly handle the environment
reduce _ _ = throwError "Calling a non-function"

step :: Expr -> Eval (ValueF ThunkID)
step (Prim p) = pure $ VPrim p
step (App f x) = do
  tf <- step f
  tx <- deferExpr x
  reduce tf tx
step (Var x) = lookupVar x force (pure . VRTVar x) (pure . VBlockLabel x) (pure . VSelf)
step (Lam arg body) = VClosure arg body <$> view ctx
step (Let binds body) = do
  n0 <- use thunkSource
  let predictedThunks = zip (fst <$> binds) [n0 ..]
  local (ctx %~ bindThunks predictedThunks) $ do
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
  tv <- fresh
  local (envBlockStack %~ (tv :)) $
    case b ^. blkLabel of
      Nothing -> genBlock tv b
      Just lbl ->
        local
          (ctx . ctxBinds . at lbl ?~ BBlockLabel tv)
          $ genBlock tv b
step (List l) = VList <$> traverse deferExpr l
step (With bind body) = do
  step bind >>= \case
    VAttr m -> local (ctx %~ bindThunks (M.toList m)) (step body)
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
        name <- view $ ctx . ctxName . to (fromMaybe "fn")
        body' <- typecheckFunction body
        argsTys <- (traverse . traverse) getType argVars
        retTy <- getType retVar
        let funDef = FunDef argsTys retTy name body'
        uncurry VFunc <$> mkFunction freshTempId recDepth deps funDef
      _ -> throwError "Function body did not evaluate to a block expression"

typecheckFunction ::
  RTBlock PreCall TypeVar ->
  Eval (RTBlock PreCall Type)
typecheckFunction (Block lbl stmts typ) = do
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
  expr' <- rtFromExpr expr
  r' <- genStmt r
  view envFnStack >>= \case
    (_, tv) : _ -> lift $ unify_ (rtInfo expr') tv
    _ -> pure ()
  pure (Return expr' : r')
genStmt (Break mname mexpr : r) = do
  mexpr' <- traverse rtFromExpr mexpr
  mlbl <- traverse resolveToBlockLabel mname
  tv <- case mlbl of
    Just (_, tv) -> pure tv
    Nothing ->
      view envBlockStack >>= \case
        tv : _ -> pure tv
        _ -> throwError "impossible empty block stack"
  lift $ case mexpr' of
    Just expr -> unify_ tv (rtInfo expr)
    Nothing -> setType tv TVoid
  r' <- genStmt r
  pure (Break (fst <$> mlbl) mexpr' : r')
genStmt (Continue mname : r) = do
  r' <- genStmt r
  mname' <- traverse resolveToBlockLabel mname
  pure (Continue (fst <$> mname') : r')
genStmt (Decl name typ expr : r) = do
  typ' <- lift $ mapM evalType typ
  expr' <- rtFromExpr expr
  tv <- lift $ tvarMay typ'
  lift $ unify_ tv (rtInfo expr')
  r' <- local (ctx . ctxBinds %~ bindRtvar name tv) (genStmt r)
  pure (Decl name tv expr' : r')
genStmt (Assign name expr : r) = do
  (name', tv) <- resolveToRuntimeVar name
  expr' <- rtFromExpr expr
  lift $ unify_ tv (rtInfo expr')
  r' <- genStmt r
  pure (Assign name' expr' : r')
genStmt (ExprStmt expr : r) = do
  expr' <- rtFromExpr expr
  r' <- genStmt r
  pure (ExprStmt expr' : r')
genStmt [] = pure []

genBlock ::
  TypeVar ->
  Block (Maybe Expr) Expr ->
  Eval (Dependencies, RTBlock PreCall TypeVar)
genBlock tv (Block lbl stmts typ) = do
  (stmts', deps) <- runWriterT $ genStmt stmts
  forM_ typ (evalType >=> setType tv) -- Note that you currently cannot actually give a type expression for a block yet
  pure (deps, Block lbl stmts' tv)

-- TODO this technically creates an unnecessary thunk since we defer and then
-- immediately evaluate but I don't think we care
deepEvalExpr :: Expr -> Eval Value
deepEvalExpr = deferExpr >=> deepEval

rtFromExpr ::
  Expr ->
  RTEval (RTExpr PreCall TypeVar TypeVar)
rtFromExpr (BinExpr (ArithOp op) a b) = do
  rta <- rtFromExpr a
  rtb <- rtFromExpr b
  tv <- lift $ unify (rtInfo rta) (rtInfo rtb)
  pure $ RTBin (ArithOp op) rta rtb tv
rtFromExpr (BinExpr (CompOp op) a b) = do
  rta <- rtFromExpr a
  rtb <- rtFromExpr b
  lift $ unify_ (rtInfo rta) (rtInfo rtb)
  tv <- lift $ tvar TBool
  pure $ RTBin (CompOp op) rta rtb tv
rtFromExpr (Var n) =
  lookupVar
    n
    (lift . deepEval >=> rtFromVal)
    (pure . RTVar n)
    (const $ throwError "referencing block label as expressiong")
    (const $ throwError "referencing self variable as expression")
rtFromExpr (App f x) = do
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
rtFromExpr (Cond cond t f) = do
  rtc <- rtFromExpr cond
  lift $ setType (rtInfo rtc) TBool
  rtt <- rtFromExpr t
  rtf <- rtFromExpr f
  tv <- lift $ unify (rtInfo rtt) (rtInfo rtf)
  pure $ RTCond rtc rtt rtf tv
rtFromExpr expr@With {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@Acc {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@Lam {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@Prim {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@Let {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@Attr {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@List {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@BlockExpr {} = lift (deepEvalExpr expr) >>= rtFromVal
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

safeZipWithM_ :: Applicative m => m () -> (a -> b -> m c) -> [a] -> [b] -> m ()
safeZipWithM_ err f as bs
  | length as /= length bs = err
  | otherwise = zipWithM_ f as bs

-- TODO type constraints
rtLitFromPrim :: Prim -> RTEval RTLiteral
rtLitFromPrim (PInt n) = pure $ RTInt n
rtLitFromPrim (PDouble n) = pure $ RTDouble n
rtLitFromPrim (PBool b) = pure $ RTBool b

rtFromVal :: Value -> RTEval (RTExpr PreCall TypeVar TypeVar)
rtFromVal (Fix (VPrim p)) = do
  lit <- rtLitFromPrim p
  tv <- lift fresh -- TODO appropriate type constraint
  pure $ RTLiteral lit tv
rtFromVal (Fix (VAttr m)) = do
  m' <- flip M.traverseWithKey m $ \key field ->
    rtFromVal field >>= \case
      RTLiteral l _ -> pure l
      _ -> throwError $ "Field " <> key <> " did not evaluate to a literal"
  tv <- lift fresh -- TODO appropriate type constraint
  pure $ RTLiteral (RTStruct m') tv
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

functionBodyEnv :: [(Name, TypeVar)] -> TypeVar -> EvalEnv -> EvalEnv
functionBodyEnv typedArgs ret (EvalEnv (Context binds name) depth fns _) =
  EvalEnv (Context binds' name) depth' fns' []
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
          ("matchType", tMatchType)
        ]
  local (ctx %~ bindThunk "builtins" tBuiltins) m

typeOf :: ValueF ThunkID -> Eval (ValueF ThunkID)
typeOf (VRTVar _ tv) = VType <$> getType tv
typeOf (VBlockLabel _ tv) = VType <$> getType tv
typeOf (VBlock _ blk) = VType <$> getType (blk ^. blkType)
typeOf _ = throwError "Cannot get typeOf"

struct :: ValueF ThunkID -> Eval (ValueF ThunkID)
struct (VAttr m) = do
  let forceType tid = do
        force tid >>= \case
          (VType t) -> pure t
          _ -> throwError "Struct member was not a type expression"
  types <- traverse forceType m
  pure $ VType $ TStruct types
struct _ = throwError "Con/struct/ing a struct from s'thing other than an attr set"

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