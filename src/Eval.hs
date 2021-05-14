{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Eval where

import Control.Monad.Except
import Control.Monad.RWS as RWS
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Coroutine
import Data.Map (Map)
import Data.Map qualified as M
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Set qualified as S
import Data.UnionFind.IO qualified as UF
import Data.Void
import Expr
import Lens.Micro.Platform
import Program

data ValueF val
  = VPrim Prim
  | VClosure Name Expr StaticEnv -- TODO benchmark if we can safely remove this
  | VClosure' (ThunkID -> Eval (ValueF ThunkID)) -- TODO can this be Eval ThunkID, not force evaluation?
  | VType Type
  | VAttr (Map Name val)
  | VRTVar VarID
  | VBlockLabel LabelID
  | VSelf Int
  | VBlock StaticEnv (Block Name Name (Maybe Expr) Expr)
  | VFunc Dependencies (Either TempFuncID GUID)
  | VList (Seq val)
  deriving (Functor, Foldable, Traversable)

arith :: Num n => ArithOp -> n -> n -> n
arith Add = (+)
arith Sub = (-)
arith Mul = (*)

comp :: Ord n => CompOp -> n -> n -> Bool
comp Eq = (==)
comp Neq = (/=)
comp Lt = (<)
comp Gt = (>)
comp Leq = (<=)
comp Geq = (>=)

type Value = Fix ValueF

data ThunkF m v = Deferred (m v) | Computed v

type Thunk = ThunkF Eval (ValueF ThunkID)

newtype ThunkID = ThunkID Int deriving (Eq, Show, Ord)

-- TODO Eval is no longer a transformer, remove the m argument
newtype Eval a = Eval
  { _unLazyT ::
      ReaderT
        Environment
        ( Coroutine
            (StateT EvalState (ExceptT String IO))
        )
        a
  }
  deriving (Functor, MonadIO, Applicative, Monad, MonadReader Environment, MonadError String, MonadState EvalState, MonadCoroutine)

data EvalState = EvalState
  { _thunks :: Map ThunkID Thunk,
    _idSource :: Int
  }

newtype TypeVar = TypeVar (UF.Point (Set Type))

data Binding
  = BThunk ThunkID
  | BRTVar VarID
  | BBlockLabel LabelID
  | BSelf Int
  deriving (Eq, Show)

type Env = Map Name Binding

data StaticEnv = StaticEnv
  { _statBinds :: Map Name Binding,
    _statName :: Maybe Name,
    _statFile :: FilePath
  }

type FunctionSig = ([(Name, TypeVar)], TypeVar)

-- TODO Remove Maybes, make entire _dynamicEnv :: Maybe DynamicEnv
data DynamicEnv = DynamicEnv
  { _dynFnStack :: [FunctionSig],
    _dynBlockVar :: Maybe TypeVar,
    _dynExprVar :: Maybe TypeVar,
    _dynVars :: Map VarID TypeVar,
    _dynLabels :: Map LabelID TypeVar
  }

data Environment = Environment
  { _staticEnv :: StaticEnv,
    _dynamicEnv :: DynamicEnv
  }

makeLenses ''StaticEnv
makeLenses ''DynamicEnv
makeLenses ''Environment
makeLenses ''EvalState

envBinds :: Lens' Environment (Map Name Binding)
envBinds = staticEnv . statBinds

envName :: Lens' Environment (Maybe Name)
envName = staticEnv . statName

envExprVar :: Lens' Environment (Maybe TypeVar)
envExprVar = dynamicEnv . dynExprVar

envBlockVar :: Lens' Environment (Maybe TypeVar)
envBlockVar = dynamicEnv . dynBlockVar

envFnStack :: Lens' Environment [FunctionSig]
envFnStack = dynamicEnv . dynFnStack

envFile :: Lens' Environment FilePath
envFile = staticEnv . statFile

envRTVar :: VarID -> Lens' Environment (Maybe TypeVar)
envRTVar tid = dynamicEnv . dynVars . at tid

envRTLabel :: LabelID -> Lens' Environment (Maybe TypeVar)
envRTLabel tid = dynamicEnv . dynLabels . at tid

withName :: Name -> Eval a -> Eval a
withName name = local (envName ?~ name)

lookupVar ::
  (MonadReader Environment m, MonadError String m) =>
  Name ->
  (ThunkID -> m r) ->
  (VarID -> TypeVar -> m r) ->
  (LabelID -> TypeVar -> m r) ->
  (Int -> m r) ->
  m r
lookupVar name kct krt klbl kself = do
  view (envBinds . at name) >>= \case
    Nothing -> throwError $ "undefined variable " <> show name
    Just (BThunk tid) -> kct tid
    Just (BRTVar tpid) ->
      view (dynamicEnv . dynVars . at tpid) >>= \case
        Nothing -> throwError $ name <> " refers to a variable that's not currently in scope"
        Just tv -> krt tpid tv
    Just (BBlockLabel tpid) ->
      view (dynamicEnv . dynLabels . at tpid) >>= \case
        Nothing -> throwError $ name <> " refers to a label that's not currently in scope"
        Just tv -> klbl tpid tv
    Just (BSelf n) -> kself n

bindThunk :: Name -> ThunkID -> Environment -> Environment
bindThunk name tid = envBinds . at name ?~ BThunk tid

bindThunks :: [(Name, ThunkID)] -> Environment -> Environment
bindThunks = appEndo . mconcat . fmap (Endo . uncurry bindThunk)

bindRtvar ::
  (MonadReader Environment m, MonadState EvalState m) =>
  Name ->
  TypeVar ->
  m a ->
  m (VarID, a)
bindRtvar name tv k = do
  tmpid <- freshVarId
  a <- flip local k $ \env ->
    env & dynamicEnv . dynVars . at tmpid ?~ tv
      & staticEnv . statBinds . at name ?~ BRTVar tmpid
  pure (tmpid, a)

bindLabel ::
  (MonadReader Environment m, MonadState EvalState m) =>
  Name ->
  TypeVar ->
  (LabelID -> m a) ->
  m a
bindLabel name tv k = do
  tmpid <- freshLblId
  flip local (k tmpid) $ \env ->
    env & dynamicEnv . dynLabels . at tmpid ?~ tv
      & staticEnv . statBinds . at name ?~ BBlockLabel tmpid

runEval :: FilePath -> Eval a -> IO (Either String a)
runEval fp (Eval m) =
  (fmap . fmap) fst $
    runExceptT $
      flip runStateT st0 $
        runCoroutine $
          runReaderT m env0
  where
    env0 =
      Environment
        (StaticEnv mempty Nothing fp)
        (DynamicEnv mempty Nothing Nothing mempty mempty)
    st0 = EvalState mempty 0

deferAttrs :: [(Name, ValueF Void)] -> Eval ThunkID
deferAttrs attrs = do
  attrs' <- (traverse . traverse) (deferVal . fmap absurd) attrs
  deferVal $ VAttr $ M.fromList attrs'

deepEval :: ThunkID -> Eval Value
deepEval tid = Fix <$> (force tid >>= traverse deepEval)

localState :: MonadState s m => (s -> s) -> m a -> m a
localState f m = do
  s <- get
  put (f s)
  a <- m
  put s
  pure a

freshId :: MonadState EvalState m => m Int
freshId = state (\(EvalState ts n) -> (n, EvalState ts (n + 1)))

freshVarId :: MonadState EvalState m => m VarID
freshVarId = VarID <$> freshId

freshLblId :: MonadState EvalState m => m LabelID
freshLblId = LabelID <$> freshId

freshFuncId :: MonadState EvalState m => m TempFuncID
freshFuncId = TempFuncID <$> freshId

type RTEval = WriterT Dependencies Eval -- TODO rename this

resolveToRuntimeVar :: Name -> RTEval (VarID, TypeVar)
resolveToRuntimeVar name =
  lift $
    lookupVar name kComp (curry pure) (\_ _ -> err) (const err)
  where
    err = throwError $ "Variable " <> show name <> " did not resolve to runtime variable"
    kComp tid =
      force tid >>= \case
        VRTVar tpid ->
          view (envRTVar tpid) >>= \case
            Just tv -> pure (tpid, tv)
            Nothing -> err
        _ -> err

resolveToBlockLabel :: Name -> RTEval (LabelID, TypeVar)
resolveToBlockLabel name = lift $ lookupVar name kComp (\_ _ -> err) (curry pure) (const err)
  where
    err = throwError $ "Variable " <> show name <> " did not resolve to block label"
    kComp tid =
      force tid >>= \case
        VBlockLabel tpid ->
          view (envRTLabel tpid) >>= \case
            Just tv -> pure (tpid, tv)
            Nothing -> err
        _ -> err

mkThunk :: Thunk -> Eval ThunkID
mkThunk thunk = state $ \(EvalState ts n) -> (ThunkID n, EvalState (M.insert (ThunkID n) thunk ts) (n + 1))

deferM :: Eval (ValueF ThunkID) -> Eval ThunkID
deferM = mkThunk . Deferred

fresh :: MonadIO m => m TypeVar
fresh = liftIO $ TypeVar <$> UF.fresh mempty

setType :: MonadIO m => TypeVar -> Type -> m ()
setType (TypeVar tv) ty = liftIO $ UF.modifyDescriptor tv (S.insert ty)

tvar :: MonadIO m => Type -> m TypeVar
tvar ty = fresh >>= \var -> var <$ setType var ty

tvarMay :: MonadIO m => Maybe Type -> m TypeVar
tvarMay mty = do
  tv <- fresh
  forM_ mty $ setType tv
  pure tv

unify :: TypeVar -> TypeVar -> Eval TypeVar
unify a b = a <$ unify_ a b

unify_ :: MonadIO m => TypeVar -> TypeVar -> m ()
unify_ (TypeVar a) (TypeVar b) = liftIO $ UF.union' a b (\sa sb -> pure (sa <> sb))

getType :: Eval Type -> TypeVar -> Eval Type
getType def (TypeVar tv) = do
  tys <- liftIO $ UF.descriptor tv
  case S.toList tys of
    [a] -> pure a
    [] -> def
    l -> throwError $ "Overdetermined: " <> show l

deferVal :: ValueF ThunkID -> Eval ThunkID
deferVal = mkThunk . Computed

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

getTypeSuspend :: TypeVar -> Eval Type
getTypeSuspend tv = go retries
  where
    retries = 10
    go :: Int -> Eval Type
    go 0 = throwError "Underdetermined type variable"
    go n = getType (suspend $ go (n -1)) tv
