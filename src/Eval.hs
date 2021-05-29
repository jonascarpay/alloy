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
  | VRTExpr (DynEval (RTExpr VarID LabelID PreCall TypeVar TypeVar))
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
describeValue VRTExpr {} = "runtime expression"
describeValue VFunc {} = "runtime function"
describeValue VList {} = "list"

-- | The evaluation monad, minus the environment
-- It is used in two places, first with an Environment reader to create the Eval monad proper.
-- Second, when we defer a value, in which case we never want make sure we don't accidentally
-- inherit the environment of the evaluation site.
newtype Eval a = Eval
  {unEval :: Coroutine (StateT EvalState (ExceptT String IO)) a}
  deriving (Functor, MonadIO, Applicative, Monad, MonadError String, MonadState EvalState, MonadCoroutine)

newtype ThunkID = ThunkID Int deriving (Eq, Show, Ord)

class MonadError String m => MonadEval m where
  liftEval :: Eval a -> m a

instance MonadEval Eval where
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
  = Deferred (Eval LazyValue)
  | Computed LazyValue

newtype StaticEval a = StaticEval {unStaticEval :: ReaderT StaticEnv Eval a}
  deriving (Functor, MonadIO, Applicative, Monad, MonadReader StaticEnv, MonadError String, MonadEval, MonadCoroutine)

newtype TypeVar = TypeVar (UF.Point (Set Type))

newtype DynEval a = DynEval {unDynEval :: RWST DynamicEnv Dependencies () Eval a}
  deriving (Functor, MonadIO, Applicative, Monad, MonadError String, MonadEval, MonadCoroutine)

type Env = Map Name ThunkID

data StaticEnv = StaticEnv
  { _statBinds :: Map Name ThunkID,
    _statName :: Maybe Name
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

makeLenses ''StaticEnv
makeLenses ''DynamicEnv
makeLenses ''EvalState

withName :: Name -> StaticEval a -> StaticEval a
withName name = local (statName ?~ name)

lookupVar ::
  Name ->
  (ThunkID -> StaticEval r) ->
  StaticEval r
lookupVar name kct = do
  view (statBinds . at name) >>= \case
    Nothing -> throwError $ "undefined variable " <> show name
    Just tid -> kct tid

bindThunk :: Name -> ThunkID -> StaticEnv -> StaticEnv
bindThunk name tid = statBinds . at name ?~ tid

bindThunks :: [(Name, ThunkID)] -> StaticEnv -> StaticEnv
bindThunks = appEndo . mconcat . fmap (Endo . uncurry bindThunk)

bindRtvar ::
  Name ->
  TypeVar ->
  RTEval a ->
  RTEval (VarID, a)
bindRtvar name tv k = error "bindRTVar"

-- tmpid <- freshVarId
-- thunk <- lift $ deferVal (VRTVar tmpid)
-- a <- flip local k $ \env ->
--   env & dynamicEnv . dynVars . at tmpid ?~ tv
--     & staticEnv . statBinds . at name ?~ thunk
-- pure (tmpid, a)

bindLabel ::
  Name ->
  TypeVar ->
  (LabelID -> RTEval a) ->
  RTEval a
bindLabel name tv k = error "bindLabel"

-- tmpid <- freshLblId
-- thunk <- lift $ deferVal (VBlockLabel tmpid)
-- flip local (k tmpid) $ \env ->
--   env & dynamicEnv . dynLabels . at tmpid ?~ tv
--     & staticEnv . statBinds . at name ?~ thunk

runEvalControl :: Eval a -> IO (Either String a)
runEvalControl (Eval m) =
  runExceptT $
    flip evalStateT st0 $
      runCoroutine m
  where
    st0 = EvalState mempty 0

runStatic :: StaticEnv -> StaticEval a -> Eval a
runStatic env (StaticEval m) = runReaderT m env

-- TODO rename this and runEvalControl
runEval :: StaticEval a -> IO (Either String a)
runEval (StaticEval m) = runEvalControl $ runReaderT m env0
  where
    env0 = StaticEnv mempty Nothing

deferAttrs :: [(Name, ValueF Void)] -> StaticEval ThunkID
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

type RTEval = WriterT Dependencies StaticEval -- TODO rename this

{-# ANN resolveToRuntimeVar "hlint: ignore" #-}
resolveToRuntimeVar :: Name -> RTEval (VarID, TypeVar)
resolveToRuntimeVar name = error "resolveToRuntimeVar"

-- lift $
--   lookupVar name $ \thunk ->
--     liftEval (force thunk) >>= \case
--       VRTVar tpid ->
--         view (envRTVar tpid) >>= \case
--           Just tv -> pure (tpid, tv)
--           Nothing -> err
--       _ -> err
-- where
--   err = throwError $ "Variable " <> show name <> " did not resolve to runtime variable"

{-# ANN resolveToBlockLabel "hlint: ignore" #-}
resolveToBlockLabel :: Name -> RTEval (LabelID, TypeVar)
resolveToBlockLabel name = error "resolveToBlockLabel" -- lift $
-- lookupVar name $ \thunk ->
--   liftEval (force thunk) >>= \case
--     VBlockLabel tpid ->
--       view (envRTLabel tpid) >>= \case
--         Just tv -> pure (tpid, tv)
--         Nothing -> err
--     _ -> err
-- where
--   err = throwError $ "Variable " <> show name <> " did not resolve to block label"

mkThunk :: Thunk -> Eval ThunkID
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

unify :: TypeVar -> TypeVar -> StaticEval TypeVar
unify a b = a <$ unify_ a b

unify_ :: MonadIO m => TypeVar -> TypeVar -> m ()
unify_ (TypeVar a) (TypeVar b) = liftIO $ UF.union' a b (\sa sb -> pure (sa <> sb))

getType :: StaticEval Type -> TypeVar -> StaticEval Type
getType def (TypeVar tv) = do
  tys <- liftIO $ UF.descriptor tv
  case S.toList tys of
    [a] -> pure a
    [] -> def
    l -> throwError $ "Overdetermined: " <> show l

deferVal :: MonadEval m => LazyValue -> m ThunkID
deferVal = liftEval . mkThunk . Computed

close :: StaticEval a -> StaticEval (Eval a)
close (StaticEval (ReaderT m)) = StaticEval $ ReaderT $ pure . m

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

getTypeSuspend :: TypeVar -> StaticEval Type
getTypeSuspend tv = go retries
  where
    retries = 10
    go :: Int -> StaticEval Type
    go 0 = throwError "Underdetermined type variable"
    go n = getType (suspend $ go (n -1)) tv
