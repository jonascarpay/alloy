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

step :: Expr -> Eval (ValueF ThunkID)
step (Prim p) = pure $ VPrim p
step (App f x) = do
  step f >>= \case
    (VClosure arg body env) -> do
      tx <- deferExpr x
      local (ctx .~ bindThunk arg tx env) (step body)
    (VClosure' m) -> deferExpr x >>= m -- FIXME this will eventually break since it does not properly handle the environment
    _ -> throwError "Calling a non-function"
step (Var x) = lookupVar x force (const $ pure $ VRTVar x) (const $ pure $ VBlockLabel x) (pure . VSelf)
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
  case b ^. blkLabel of
    Nothing -> genBlock b
    Just lbl -> local (ctx . ctxBinds . at lbl ?~ BBlockLabel tv) $ genBlock b
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
typecheckFunction blk = do
  (blkStmts . traverse . types) getType blk
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
          unPreCall _ = error "impossible"
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
          replaceAll _ = error "impossible"
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
  mname' <- traverse resolveToBlockLabel mname
  r' <- genStmt r
  pure (Break mname' mexpr' : r')
genStmt (Continue mname : r) = do
  r' <- genStmt r
  mname' <- traverse resolveToBlockLabel mname
  pure (Continue mname' : r')
genStmt (Decl name typ expr : r) = do
  typ' <- lift $ mapM evalType typ
  expr' <- rtFromExpr expr
  tv <- lift $ tvarMay typ'
  r' <- local (ctx . ctxBinds %~ bindRtvar name tv) (genStmt r)
  pure (Decl name tv expr' : r')
genStmt (Assign name expr : r) = do
  name' <- resolveToRuntimeVar name
  expr' <- rtFromExpr expr
  r' <- genStmt r
  pure (Assign name' expr' : r')
genStmt (ExprStmt expr : r) = do
  expr' <- rtFromExpr expr
  r' <- genStmt r
  pure (ExprStmt expr' : r')
genStmt [] = pure []

genBlock ::
  Block (Maybe Expr) Expr ->
  Eval (Dependencies, RTBlock PreCall TypeVar)
genBlock (Block lbl stmts) = do
  (stmts', deps) <- runWriterT $ genStmt stmts
  pure (deps, Block lbl stmts')

-- TODO this technically creates an unnecessary thunk since we defer and then
-- immediately evaluate but I don't think we care
deepEvalExpr :: Expr -> Eval Value
deepEvalExpr = deferExpr >=> deepEval

rtFromExpr ::
  Expr ->
  RTEval (RTExpr PreCall TypeVar TypeVar)
rtFromExpr (BinExpr op a b) = do
  rta <- rtFromExpr a
  rtb <- rtFromExpr b
  tv <- lift $ unify (rtInfo rta) (rtInfo rtb)
  pure $ RTBin op rta rtb tv
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
    VClosure arg body env -> do
      tx <- lift $ deferExpr x
      local (ctx .~ bindThunk arg tx env) $
        lift (deepEvalExpr body) >>= rtFromVal
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
    _ -> throwError "Calling a non-function"
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
    Nothing -> error "impossible"
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

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a : _) = Just a

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
rtFromVal (Fix (VRTVar n)) = RTVar n <$> lift fresh -- TODO PROPER RT VARIABLE HANDLING
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
functionBodyEnv typedArgs ret (EvalEnv (Context binds name) depth stack) =
  EvalEnv (Context binds' name) depth' stack'
  where
    (names, types) = unzip typedArgs
    depth' = depth + 1
    stack' = (types, ret) : stack
    binds' =
      flip (foldr (uncurry bindRtvar)) typedArgs
        . (at "self" ?~ BSelf depth)
        . M.filter isThunk
        $ binds
