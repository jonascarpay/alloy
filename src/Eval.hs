{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Eval where

import Control.Monad.Except
import Control.Monad.RWS as RWS
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

type ThunkID = Int

data ValueF val
  = VPrim Prim
  | VClosure Name Expr Context -- TODO benchmark if we can safely remove this
  | VClosure' (ThunkID -> Eval (ValueF ThunkID)) -- TODO can this be Eval ThunkID, not force evaluation?
  | VType Type
  | VAttr (Map Name val)
  | VRTVar Name TypeVar -- TODO proper binding
  | VBlockLabel Name TypeVar -- TODO proper binding
  | VSelf Int
  | VBlock Dependencies (RTBlock PreCall TypeVar)
  | VFunc Dependencies (Either TempID GUID)
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

-- TODO Eval is no longer a transformer, remove the m argument
newtype Eval a = Eval
  { _unLazyT ::
      Coroutine
        ( RWST
            EvalEnv
            ()
            EvalState
            (ExceptT String IO)
        )
        a
  }
  deriving (Functor, MonadIO, Applicative, Monad, MonadReader EvalEnv, MonadError String, MonadState EvalState, MonadCoroutine)

data EvalState = EvalState
  { _thunkSource :: Int,
    _thunks :: Map ThunkID Thunk,
    _tempSource :: Int
  }

newtype TypeVar = TypeVar (UF.Point (Set Type))

data Binding
  = BThunk ThunkID
  | BRTVar TypeVar
  | BBlockLabel TypeVar
  | BSelf Int

isThunk :: Binding -> Bool
isThunk (BThunk _) = True
isThunk _ = False

type Env = Map Name Binding

data Context = Context
  { _ctxBinds :: Map Name Binding,
    _ctxName :: Maybe Name,
    _ctxFile :: FilePath
  }

-- | The Context is definition-local, the rest of the environmant is local to the evaluation site.
data EvalEnv = EvalEnv
  { _ctx :: Context,
    _envFnDepth :: Int,
    _envFnStack :: [([TypeVar], TypeVar)],
    _envBlockVar :: Maybe TypeVar,
    _envExprVar :: Maybe TypeVar
  }

makeLenses ''Context
makeLenses ''EvalEnv
makeLenses ''EvalState

withName :: Name -> Eval a -> Eval a
withName name = local (ctx . ctxName ?~ name)

lookupVar ::
  (MonadReader EvalEnv m, MonadError String m) =>
  Name ->
  (ThunkID -> m r) ->
  (TypeVar -> m r) ->
  (TypeVar -> m r) ->
  (Int -> m r) ->
  m r
lookupVar name kct krt klbl kself = do
  view (ctx . ctxBinds . at name) >>= \case
    Nothing -> throwError $ "undefined variable " <> show name
    Just (BThunk tid) -> kct tid
    Just (BRTVar tv) -> krt tv
    Just (BBlockLabel tv) -> klbl tv
    Just (BSelf n) -> kself n

bindThunk :: Name -> ThunkID -> Context -> Context
bindThunk name tid = ctxBinds . at name ?~ BThunk tid

bindThunks :: [(Name, ThunkID)] -> Context -> Context
bindThunks = appEndo . mconcat . fmap (Endo . uncurry bindThunk)

bindRtvar :: Name -> TypeVar -> Env -> Env
bindRtvar n tv = M.insert n (BRTVar tv)

runEval :: FilePath -> Eval a -> IO (Either String a)
runEval fp (Eval m) =
  (fmap . fmap) fst $
    runExceptT $
      evalRWST (runCoroutine m) env0 st0
  where
    env0 = EvalEnv (Context mempty Nothing fp) 0 mempty Nothing Nothing
    st0 = EvalState 0 mempty 0

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

freshTempId :: Eval TempID
freshTempId = state (\(EvalState us un ts) -> (ts, EvalState us un (ts + 1)))

type RTEval = WriterT Dependencies Eval -- TODO rename this

resolveToRuntimeVar :: Name -> RTEval (Name, TypeVar)
resolveToRuntimeVar name = lift $ lookupVar name kComp (\tv -> pure (name, tv)) (const err) (const err)
  where
    err = throwError $ "Variable " <> show name <> " did not resolve to runtime variable"
    kComp tid =
      force tid >>= \case
        VRTVar v tv -> pure (v, tv)
        _ -> err

resolveToBlockLabel :: Name -> RTEval (Name, TypeVar)
resolveToBlockLabel name = lift $ lookupVar name kComp (const err) (\tv -> pure (name, tv)) (const err)
  where
    err = throwError $ "Variable " <> show name <> " did not resolve to block label"
    kComp tid =
      force tid >>= \case
        VBlockLabel v tv -> pure (v, tv)
        _ -> err

mkThunk :: Thunk -> Eval ThunkID
mkThunk thunk = state $ \(EvalState n m t) -> (n, EvalState (n + 1) (M.insert n thunk m) t)

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
