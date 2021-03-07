{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Eval (ValueF (..), Value, eval, Dependencies (..)) where

import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.Writer
import Data.Foldable
import Data.Functor.Identity
import Data.Hashable
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Set qualified as S
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
  | VClosure Name Expr EvalEnv -- TODO benchmark if we can safely remove this
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

data EvalEnv = EvalEnv
  { _envBinds :: Env,
    _envName :: Maybe Name,
    _envFnDepth :: Int
  }
  deriving (Eq, Show)

makeLenses ''EvalEnv
makeLenses ''EvalState

withName :: Monad m => Name -> EvalT m a -> EvalT m a
withName name = local (envName ?~ name)

askName :: Monad m => EvalT m (Maybe Name)
askName = view envName

localEnv :: MonadReader EvalEnv m => (Env -> Env) -> (m a -> m a)
localEnv f = local (over envBinds f)

lookupVar ::
  (MonadReader EvalEnv m, MonadError String m) =>
  Name ->
  (ThunkID -> m r) ->
  m r ->
  (Int -> m r) ->
  m r
lookupVar name kct krt kself = do
  view (envBinds . at name) >>= \case
    Nothing -> throwError $ "undefined variable " <> show name
    Just (BThunk tid) -> kct tid
    Just BRTVar -> krt
    Just (BSelf n) -> kself n

bindThunk :: Name -> ThunkID -> EvalEnv -> EvalEnv
bindThunk name tid = envBinds . at name ?~ BThunk tid

bindThunks :: [(Name, ThunkID)] -> EvalEnv -> EvalEnv
bindThunks = appEndo . mconcat . fmap (Endo . uncurry bindThunk)

bindRtvar :: Name -> Env -> Env
bindRtvar n = M.insert n BRTVar

eval :: Expr -> Either String Value
eval eRoot = runEval $ withBuiltins $ deepEvalExpr eRoot

runEval :: Eval a -> Either String a
runEval (EvalT m) = fmap fst $ runExcept $ evalRWST m (EvalEnv mempty mempty 0) (EvalState 0 mempty 0)

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
  local (bindThunk "builtins" tBuiltins) m

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
      local (const $ bindThunk arg tx env) (step body)
    (VClosure' m) -> deferExpr x >>= m -- FIXME this will eventually break, cannot handle proper closures
    _ -> throwError "Calling a non-function"
step (Var x) = lookupVar x force (pure $ VRTVar x) (pure . VSelf)
step (Lam arg body) = VClosure arg body <$> ask
step (Let binds body) = do
  n0 <- use thunkSource
  let predictedThunks = zip (fst <$> binds) [n0 ..]
  local (bindThunks predictedThunks) $ do
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
    VAttr m -> local (bindThunks $ M.toList m) (step body)
    _ -> throwError "Expression in `with` expression did not evaluate to an attrset"
step (Func args ret bodyExpr) = do
  typedArgs <- (traverse . traverse) evalType args
  local (functionBodyEnv typedArgs) (step bodyExpr) >>= \case
    VBlock deps body -> do
      retType <- evalType ret
      recDepth <- view envFnDepth
      name <- fromMaybe "fn" <$> view envName
      body' <- liftEither $ typecheckFunction deps name typedArgs retType body
      -- let guid = GUID $ hash funDef
      let funDef = FunDef typedArgs retType name body'
      uncurry VFunc <$> mkFunction freshTempId recDepth deps funDef
    _ -> throwError "Function body did not evaluate to a block expression"

freshTempId :: Eval TempID
freshTempId = state (\(EvalState us un ts) -> (ts, EvalState us un (ts + 1)))

mkFunction ::
  Monad m =>
  m TempID ->
  RecIndex ->
  Dependencies ->
  FunDef PreCall ->
  m (Dependencies, Either TempID GUID)
mkFunction fresh recDepth transDeps funDef = do
  case calls of
    [] -> pure mkClosedFunction
    l | any (< recDepth) l -> mkTempFunction
    _ -> pure closeTempFunction
  where
    mkClosedFunction =
      let unPreCall (CallKnown guid) = guid
          unPreCall _ = error "impossible"
          funDef' = funDef & funCalls %~ unPreCall
          guid = GUID $ hash funDef'
          deps = transDeps & depFuncs . at guid ?~ funDef'
       in (deps, Right guid)

    mkTempFunction = do
      temp <- fresh
      let deps = transDeps & depTempFuncs . at temp ?~ funDef
      pure (deps, Left temp)

    closeTempFunction = undefined

    calls = toListOf selfCalls funDef ++ toListOf (depTempFuncs . traverse . selfCalls) transDeps
    selfCalls :: Traversal' (FunDef PreCall) RecIndex
    selfCalls = funCalls . precallRec
    funCalls :: Traversal (FunDef call) (FunDef call') call call'
    funCalls = fnBody . blkStmts . traverse . stmtExpr . rtExprCalls

functionBodyEnv :: [(Name, Type)] -> EvalEnv -> EvalEnv
functionBodyEnv typedArgs (EvalEnv binds name depth) = EvalEnv binds' name depth'
  where
    argNames = fst <$> typedArgs
    depth' = depth + 1
    binds' =
      flip (foldr bindRtvar) argNames
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
genBlock (Block stmts) = do
  (stmts', deps) <- runWriterT $ genStmt stmts
  pure (deps, Block stmts')

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
  r' <- localEnv (bindRtvar name) (genStmt r)
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
      local (const $ bindThunk arg tx env) $
        lift (deepEvalExpr body) >>= rtFromVal
    VFunc tdeps funId ->
      case x of
        List argExprs -> do
          rtArgs <- traverse rtFromExpr (toList argExprs)
          tell tdeps
          pure $ RTCall (either CallRec CallKnown funId) rtArgs Nothing
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
