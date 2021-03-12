{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Eval (ValueF (..), Value, eval, Dependencies (..)) where

import Control.Monad.Except
import Control.Monad.RWS as RWS
import Control.Monad.Writer
import Data.Foldable
import Data.Functor.Identity
import Data.Hashable
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Sequence (Seq)
import Data.Void
import Expr
import Lens.Micro.Platform
import Program
import Typecheck

type ThunkID = Int

--TODO Closure Args (p -> m r)
--TODO How to properly handle RT Vars
--TODO I think spliced variables can escape their scope?
-- maybe just tag them from fresh to make sure
data ValueF val
  = VPrim Prim
  | VClosure Name Expr Context -- TODO benchmark if we can safely remove this
  | VClosure' (ThunkID -> Eval (ValueF ThunkID))
  | VType Type
  | VAttr (Map Name val)
  | VRTVar Name
  | VSelf Int
  | VBlock Dependencies (RTBlock PreCall (Maybe Type))
  | VFunc Dependencies (Either TempID GUID)
  | VList (Seq val)
  deriving (Functor, Foldable, Traversable)

arith :: Num n => ArithOp -> n -> n -> n
arith Add = (+)
arith Sub = (-)
arith Mul = (*)

type Value = Fix ValueF

data ThunkF m v = Deferred (m v) | Computed v

type Thunk = ThunkF Eval (ValueF ThunkID)

newtype EvalT m a = EvalT
  { _unLazyT ::
      RWST
        EvalEnv
        Dependencies
        EvalState
        (ExceptT String m)
        a
  }
  deriving (Functor, Applicative, Monad, MonadReader EvalEnv, MonadError String, MonadState EvalState, MonadWriter Dependencies)

data EvalState = EvalState
  { _thunkSource :: Int,
    _thunks :: Map ThunkID Thunk,
    _tempSource :: Int
  }

data Binding
  = BThunk ThunkID
  | BRTVar
  | BSelf Int
  deriving (Eq, Show)

isThunk :: Binding -> Bool
isThunk (BThunk _) = True
isThunk _ = False

type Eval = EvalT Identity

type Env = Map Name Binding

data Context = Context
  { _ctxBinds :: Map Name Binding,
    _ctxName :: Maybe Name
  }
  deriving (Eq, Show)

data EvalEnv = EvalEnv
  { _ctx :: Context,
    _envFnDepth :: Int,
    _envFnStack :: [([Type], Type)]
  }
  deriving (Eq, Show)

makeLenses ''Context
makeLenses ''EvalEnv
makeLenses ''EvalState

withName :: Monad m => Name -> EvalT m a -> EvalT m a
withName name = local (ctx . ctxName ?~ name)

lookupVar ::
  (MonadReader EvalEnv m, MonadError String m) =>
  Name ->
  (ThunkID -> m r) ->
  m r ->
  (Int -> m r) ->
  m r
lookupVar name kct krt kself = do
  view (ctx . ctxBinds . at name) >>= \case
    Nothing -> throwError $ "undefined variable " <> show name
    Just (BThunk tid) -> kct tid
    Just BRTVar -> krt
    Just (BSelf n) -> kself n

bindThunk :: Name -> ThunkID -> Context -> Context
bindThunk name tid = ctxBinds . at name ?~ BThunk tid

bindThunks :: [(Name, ThunkID)] -> Context -> Context
bindThunks = appEndo . mconcat . fmap (Endo . uncurry bindThunk)

bindRtvar :: Name -> Env -> Env
bindRtvar n = M.insert n BRTVar

eval :: Expr -> Either String Value
eval eRoot = runEval $ withBuiltins $ deepEvalExpr eRoot

runEval :: Eval a -> Either String a
runEval (EvalT m) = fmap fst $ runExcept $ evalRWST m (EvalEnv (Context mempty Nothing) 0 mempty) (EvalState 0 mempty 0)

deferAttrs :: [(Name, ValueF Void)] -> Eval ThunkID
deferAttrs attrs = do
  attrs' <- (traverse . traverse) (deferVal . fmap absurd) attrs
  deferVal $ VAttr $ M.fromList attrs'

withBuiltins :: Eval a -> Eval a
withBuiltins m = do
  tUndefined <- deferM $ throwError "undefined"
  tNine <- deferVal . VPrim $ PInt 9
  tStruct <- deferVal $ VClosure' (force >=> struct)
  tTypes <-
    deferAttrs
      [ ("int", VType TInt),
        ("double", VType TDouble),
        ("void", VType TVoid)
      ]
  tBuiltins <-
    deferVal . VAttr $
      M.fromList
        [ ("undefined", tUndefined),
          ("nine", tNine),
          ("types", tTypes),
          ("struct", tStruct)
        ]
  local (ctx %~ bindThunk "builtins" tBuiltins) m

struct :: ValueF ThunkID -> Eval (ValueF ThunkID)
struct (VAttr m) = do
  let forceType tid = do
        force tid >>= \case
          (VType t) -> pure t
          _ -> throwError "Struct member was not a type expression"
  types <- traverse forceType m
  pure $ VType $ TStruct types
struct _ = throwError "Con/struct/ing a struct from s'thing other than an attr set"

-- TODO this technically creates an unnecessary thunk since we defer and then
-- immediately evaluate but I don't think we care
deepEvalExpr :: Expr -> Eval Value
deepEvalExpr = deferExpr >=> deepEval

deepEval :: ThunkID -> Eval Value
deepEval tid = Fix <$> (force tid >>= traverse deepEval)

step :: Expr -> Eval (ValueF ThunkID)
step (Prim p) = pure $ VPrim p
step (App f x) = do
  step f >>= \case
    (VClosure arg body env) -> do
      tx <- deferExpr x
      local (ctx .~ bindThunk arg tx env) (step body)
    (VClosure' m) -> deferExpr x >>= m -- FIXME this will eventually break since it does not properly handle the environment
    _ -> throwError "Calling a non-function"
step (Var x) = lookupVar x force (pure $ VRTVar x) (pure . VSelf)
step (Lam arg body) = VClosure arg body <$> view ctx
step (Let binds body) = do
  n0 <- use thunkSource
  let predictedThunks = zip (fst <$> binds) [n0 ..]
  local (ctx %~ bindThunks predictedThunks) $ do
    forM_ binds $ \(name, expr) -> withName name $ deferExpr expr
    step body
step (Arith op a b) = do
  va <- step a
  vb <- step b
  case (va, vb) of
    (VPrim (PInt pa), VPrim (PInt pb)) -> pure . VPrim . PInt $ arith op pa pb
    (VPrim (PDouble pa), VPrim (PDouble pb)) -> pure . VPrim . PDouble $ arith op pa pb
    _ -> throwError "Arithmetic on a non-integer"
step (Attr m) = VAttr <$> M.traverseWithKey (\name expr -> withName name $ deferExpr expr) m
step (Acc f em) =
  step em >>= \case
    VAttr m ->
      case M.lookup f m of
        Nothing -> throwError $ "field " <> f <> " not present"
        Just tid -> force tid
    _ -> throwError "Accessing field of not an attribute set"
step (BlockExpr b) =
  uncurry VBlock <$> genBlock b
step (List l) = VList <$> traverse deferExpr l
step (With bind body) = do
  step bind >>= \case
    VAttr m -> local (ctx %~ bindThunks (M.toList m)) (step body)
    _ -> throwError "Expression in `with` expression did not evaluate to an attrset"
step (Func args ret bodyExpr) = do
  typedArgs <- (traverse . traverse) evalType args
  retType <- evalType ret
  recDepth <- view envFnDepth
  local (functionBodyEnv typedArgs retType) $ do
    step bodyExpr >>= \case
      VBlock deps body -> do
        fnStack <- view envFnStack
        name <- view $ ctx . ctxName . to (fromMaybe "fn")
        body' <- liftEither $ typecheckFunction deps recDepth fnStack typedArgs retType body
        let funDef = FunDef typedArgs retType name body'
        uncurry VFunc <$> mkFunction freshTempId recDepth deps funDef
      _ -> throwError "Function body did not evaluate to a block expression"

freshTempId :: Eval TempID
freshTempId = state (\(EvalState us un ts) -> (ts, EvalState us un (ts + 1)))

-- TODO I don't think TempIDs are necessary at all, thunks already provide deduplication
-- Maybe we need them for hashing?
-- In that case, locally reset to properly deduplicate?
mkFunction ::
  Monad m =>
  m TempID ->
  RecIndex ->
  Dependencies ->
  FunDef PreCall ->
  m (Dependencies, Either TempID GUID)
mkFunction fresh recDepth transDeps funDef = do
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
    funCalls :: Traversal (FunDef call) (FunDef call') call call'
    funCalls = fnBody . blkStmts . traverse . stmtExpr . rtExprCalls

functionBodyEnv :: [(Name, Type)] -> Type -> EvalEnv -> EvalEnv
functionBodyEnv typedArgs ret (EvalEnv (Context binds name) depth stack) =
  EvalEnv (Context binds' name) depth' stack'
  where
    (names, types) = unzip typedArgs
    depth' = depth + 1
    stack' = (types, ret) : stack
    binds' =
      flip (foldr bindRtvar) names
        . (at "self" ?~ BSelf depth)
        . M.filter isThunk
        $ binds

-- Dependencies -> Dependencies

evalType :: Expr -> Eval Type
evalType expr =
  step expr >>= \case
    VType tp -> pure tp
    _ -> throwError "Expected type, got not-a-type"

genBlock ::
  Block (Maybe Expr) Expr ->
  Eval (Dependencies, RTBlock PreCall (Maybe Type))
genBlock (Block lbl stmts) = do
  (stmts', deps) <- runWriterT $ genStmt stmts
  pure (deps, Block lbl stmts')

type RTEval = WriterT Dependencies Eval -- TODO rename this

-- TODO This processes statements as a list because a declaration is relevant in the tail of the list
-- but that's kinda hacky and might overlap with type checking
genStmt ::
  [Stmt (Maybe Expr) Expr] ->
  RTEval [Stmt (Maybe Type) (RTExpr PreCall (Maybe Type) (Maybe Type))]
genStmt (Return expr : r) = do
  expr' <- rtFromExpr expr
  r' <- genStmt r
  pure (Return expr' : r')
genStmt (Decl name typ expr : r) = do
  typ' <- lift $ mapM evalType typ
  expr' <- rtFromExpr expr
  r' <- local (ctx . ctxBinds %~ bindRtvar name) (genStmt r)
  pure (Decl name typ' expr' : r')
genStmt (Assign name expr : r) = do
  name' <- do
    let ct tid =
          force tid >>= \case
            VRTVar v -> pure v
            _ -> throwError $ "Assigning to non-runtime-variable " <> show name
        rt = pure name
     in lift $ lookupVar name ct rt (error "self")
  expr' <- rtFromExpr expr
  r' <- genStmt r
  pure (Assign name' expr' : r')
genStmt (ExprStmt expr : r) = do
  expr' <- rtFromExpr expr
  r' <- genStmt r
  pure (ExprStmt expr' : r')
genStmt [] = pure []

rtFromExpr ::
  Expr ->
  RTEval (RTExpr PreCall (Maybe Type) (Maybe Type))
rtFromExpr (Arith op a b) = do
  rta <- rtFromExpr a
  rtb <- rtFromExpr b
  pure $ RTArith op rta rtb Nothing
rtFromExpr (Var n) =
  lookupVar
    n
    (lift . deepEval >=> rtFromVal)
    (pure (RTVar n Nothing))
    (error "self")
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
          pure $ RTCall (either CallTemp CallKnown funId) rtArgs Nothing
        _ -> throwError "Trying to call a function with a non-list-like-thing"
    VSelf n ->
      case x of
        List argExprs -> do
          rtArgs <- traverse rtFromExpr (toList argExprs)
          pure $ RTCall (CallRec n) rtArgs Nothing
        _ -> throwError "Trying to call a function with a non-list-like-thing"
    _ -> throwError "Calling a non-function"
rtFromExpr expr@With {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@Acc {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@Lam {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@Prim {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@Let {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@Attr {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@List {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@BlockExpr {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@Func {} = lift (deepEvalExpr expr) >>= rtFromVal

rtLitFromPrim :: Prim -> RTEval RTLiteral
rtLitFromPrim (PInt n) = pure $ RTInt n
rtLitFromPrim (PDouble n) = pure $ RTDouble n
rtLitFromPrim (PBool _) = throwError "runtime bools aren't a thing"

rtFromVal :: Value -> RTEval (RTExpr PreCall (Maybe Type) (Maybe Type))
rtFromVal (Fix (VPrim p)) = fmap (`RTLiteral` Nothing) (rtLitFromPrim p)
rtFromVal (Fix (VAttr m)) = do
  m' <- flip M.traverseWithKey m $ \key field ->
    rtFromVal field >>= \case
      RTLiteral l _ -> pure l
      _ -> throwError $ "Field " <> key <> " did not evaluate to a literal"
  pure $ RTLiteral (RTStruct m') Nothing
rtFromVal (Fix (VRTVar n)) = pure $ RTVar n Nothing
rtFromVal (Fix VClosure {}) = throwError "partially applied closure in runtime expression"
rtFromVal (Fix VClosure' {}) = throwError "partially applied closure' in runtime expression"
rtFromVal (Fix VSelf {}) = throwError "naked `self` in runtime expression"
rtFromVal (Fix VList {}) = throwError "can't handle list values yet"
rtFromVal (Fix VType {}) = throwError "can't handle type"
rtFromVal (Fix VFunc {}) = throwError "Function values don't make sense here"
rtFromVal (Fix (VBlock deps b)) = RTBlock b Nothing <$ tell deps

mkThunk :: Thunk -> Eval ThunkID
mkThunk thunk = state $ \(EvalState n m t) -> (n, EvalState (n + 1) (M.insert n thunk m) t)

deferM :: Eval (ValueF ThunkID) -> Eval ThunkID
deferM = mkThunk . Deferred

deferVal :: ValueF ThunkID -> Eval ThunkID
deferVal = mkThunk . Computed

deferExpr :: Expr -> Eval ThunkID
deferExpr expr = ask >>= \env -> deferM $ local (const env) (step expr)

force :: ThunkID -> Eval (ValueF ThunkID)
force tid =
  use (thunks . at tid) >>= \case
    Just (Deferred m) -> do
      thunks . at tid .= Just (Deferred $ throwError "Infinite recursion")
      v <- m
      thunks . at tid .= Just (Computed v)
      pure v
    Just (Computed x) -> pure x
    Nothing -> throwError "Looking up invalid thunk?"
