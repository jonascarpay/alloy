-- TODO expot list
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
  -- TODO can this be Eval ThunkID, not force evaluation?
  | VClosure' (ThunkID -> Eval LazyValue)
  | VType Type
  | VAttr (Map Name val)
  | VRTVar VarID
  | VBlockLabel LabelID
  | VSelf Int
  | VBlock StaticEnv (Block Name Name (Maybe Expr) Expr)
  | VFunc Dependencies (Either TempFuncID GUID)
  | VList (Seq val)
  deriving (Functor, Foldable, Traversable)

arithInt :: ArithOp -> Int -> Int -> Int
arithInt Add = (+)
arithInt Sub = (-)
arithInt Mul = (*)
arithInt Div = div

arithFloat :: ArithOp -> Double -> Double -> Double
arithFloat Add = (+)
arithFloat Sub = (-)
arithFloat Mul = (*)
arithFloat Div = (/)

comp :: Ord n => CompOp -> n -> n -> Bool
comp Eq = (==)
comp Neq = (/=)
comp Lt = (<)
comp Gt = (>)
comp Leq = (<=)
comp Geq = (>=)

type Value = Fix ValueF

type LazyValue = ValueF ThunkID

describeValue :: ValueF f -> String
describeValue (VPrim (PInt _)) = "integer"
describeValue (VPrim (PDouble _)) = "double"
describeValue (VPrim (PBool _)) = "boolean"
describeValue (VPrim (PString _)) = "string"
describeValue VClosure {} = "closure"
describeValue VClosure' {} = "closure"
describeValue VType {} = "type"
describeValue VAttr {} = "attribute set"
describeValue VRTVar {} = "runtime variable"
describeValue VBlockLabel {} = "block label"
describeValue VSelf {} = "recursive function reference"
describeValue VBlock {} = "code block"
describeValue VFunc {} = "runtime function"
describeValue VList {} = "list"

-- | The evaluation monad, minus the environment
-- It is used in two places, first with an Environment reader to create the Eval monad proper.
-- Second, when we defer a value, in which case we never want make sure we don't accidentally
-- inherit the environment of the evaluation site.
newtype EvalControl a = EvalControl
  { unEvalControl :: Coroutine (StateT EvalState (ExceptT String IO)) a
  }
  deriving (Functor, MonadIO, Applicative, Monad, MonadError String, MonadState EvalState, MonadCoroutine)

newtype ThunkID = ThunkID Int deriving (Eq, Show, Ord)

class Monad m => MonadEval m where
  liftEval :: EvalControl a -> m a

instance MonadEval EvalControl where
  liftEval = id

instance MonadEval m => MonadEval (ReaderT r m) where
  liftEval = lift . liftEval

instance (Monoid w, MonadEval m) => MonadEval (WriterT w m) where
  liftEval = lift . liftEval

instance (Monoid w, MonadEval m) => MonadEval (RWST r w s m) where
  liftEval = lift . liftEval

data EvalState = EvalState
  { _thunks :: Map ThunkID Thunk,
    _idSource :: Int
  }

data Thunk
  = Deferred (EvalControl LazyValue)
  | Computed LazyValue

newtype Eval a = Eval {unEval :: ReaderT Environment EvalControl a}
  deriving (Functor, MonadIO, Applicative, Monad, MonadReader Environment, MonadError String, MonadEval, MonadCoroutine)

newtype TypeVar = TypeVar (UF.Point (Set Type))

type Env = Map Name ThunkID

data StaticEnv = StaticEnv
  { _statBinds :: Map Name ThunkID,
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

envBinds :: Lens' Environment (Map Name ThunkID)
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
  m r
lookupVar name kct = do
  view (envBinds . at name) >>= \case
    Nothing -> throwError $ "undefined variable " <> show name
    Just tid -> kct tid

bindThunk :: Name -> ThunkID -> Environment -> Environment
bindThunk name tid = envBinds . at name ?~ tid

bindThunks :: [(Name, ThunkID)] -> Environment -> Environment
bindThunks = appEndo . mconcat . fmap (Endo . uncurry bindThunk)

bindRtvar ::
  Name ->
  TypeVar ->
  RTEval a ->
  RTEval (VarID, a)
bindRtvar name tv k = do
  tmpid <- freshVarId
  thunk <- lift $ deferVal (VRTVar tmpid)
  a <- flip local k $ \env ->
    env & dynamicEnv . dynVars . at tmpid ?~ tv
      & staticEnv . statBinds . at name ?~ thunk
  pure (tmpid, a)

bindLabel ::
  Name ->
  TypeVar ->
  (LabelID -> RTEval a) ->
  RTEval a
bindLabel name tv k = do
  tmpid <- freshLblId
  thunk <- lift $ deferVal (VBlockLabel tmpid)
  flip local (k tmpid) $ \env ->
    env & dynamicEnv . dynLabels . at tmpid ?~ tv
      & staticEnv . statBinds . at name ?~ thunk

runEvalControl :: EvalControl a -> IO (Either String a)
runEvalControl (EvalControl m) =
  runExceptT $
    flip evalStateT st0 $
      runCoroutine m
  where
    st0 = EvalState mempty 0

runEval :: FilePath -> Eval a -> IO (Either String a)
runEval fp (Eval m) = runEvalControl $ runReaderT m env0
  where
    env0 =
      Environment
        (StaticEnv mempty Nothing fp)
        (DynamicEnv mempty Nothing Nothing mempty mempty)

deferAttrs :: [(Name, ValueF Void)] -> Eval ThunkID
deferAttrs attrs = do
  attrs' <- (traverse . traverse) (deferVal . fmap absurd) attrs
  deferVal $ VAttr $ M.fromList attrs'

deepEval :: MonadEval m => ThunkID -> m Value
deepEval tid = Fix <$> (force tid >>= traverse deepEval)

freshId :: MonadEval m => m Int
freshId = liftEval $ state (\(EvalState ts n) -> (n, EvalState ts (n + 1)))

freshThunkId :: MonadEval m => m ThunkID
freshThunkId = ThunkID <$> freshId

freshVarId :: MonadEval m => m VarID
freshVarId = VarID <$> freshId

freshLblId :: MonadEval m => m LabelID
freshLblId = LabelID <$> freshId

freshFuncId :: MonadEval m => m TempFuncID
freshFuncId = TempFuncID <$> freshId

type RTEval = WriterT Dependencies Eval -- TODO rename this

{-# ANN resolveToRuntimeVar "hlint: ignore" #-}
resolveToRuntimeVar :: Name -> RTEval (VarID, TypeVar)
resolveToRuntimeVar name =
  lift $
    lookupVar name $ \thunk ->
      liftEval (force thunk) >>= \case
        VRTVar tpid ->
          view (envRTVar tpid) >>= \case
            Just tv -> pure (tpid, tv)
            Nothing -> err
        _ -> err
  where
    err = throwError $ "Variable " <> show name <> " did not resolve to runtime variable"

{-# ANN resolveToBlockLabel "hlint: ignore" #-}
resolveToBlockLabel :: Name -> RTEval (LabelID, TypeVar)
resolveToBlockLabel name = lift $
  lookupVar name $ \thunk ->
    liftEval (force thunk) >>= \case
      VBlockLabel tpid ->
        view (envRTLabel tpid) >>= \case
          Just tv -> pure (tpid, tv)
          Nothing -> err
      _ -> err
  where
    err = throwError $ "Variable " <> show name <> " did not resolve to block label"

mkThunk :: Thunk -> EvalControl ThunkID
mkThunk thunk = state $ \(EvalState ts n) -> (ThunkID n, EvalState (M.insert (ThunkID n) thunk ts) (n + 1))

setThunk :: MonadEval m => ThunkID -> Thunk -> m ()
setThunk tid thunk = liftEval $ thunks . at tid ?= thunk

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

deferVal :: MonadEval m => LazyValue -> m ThunkID
deferVal = liftEval . mkThunk . Computed

close :: Eval a -> Eval (EvalControl a)
close (Eval (ReaderT m)) = Eval $ ReaderT $ pure . m

force :: MonadEval m => ThunkID -> m LazyValue
force tid =
  liftEval $
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
