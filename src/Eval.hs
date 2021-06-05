{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Eval where

import Control.Monad.Except
import Control.Monad.State
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

-- | The evaluation monad, minus the environment
-- It is used in two places, first with an Environment reader to create the Eval monad proper.
-- Second, when we defer a value, in which case we never want make sure we don't accidentally
-- inherit the environment of the evaluation site.
newtype Eval a = Eval
  {unEval :: Coroutine (StateT EvalState (ExceptT String IO)) a}
  deriving (Functor, MonadIO, Applicative, Monad, MonadError String, MonadState EvalState, MonadCoroutine)

newtype ThunkID = ThunkID Int deriving (Eq, Show, Ord)

data EvalState = EvalState
  { _thunks :: Map ThunkID Thunk,
    _idSource :: Int
  }

data ValueF val
  = VPrim Prim
  | VClosure Name Expr StaticEnv -- TODO benchmark if we can safely remove this
  | VClosure' (DynamicEnv -> ThunkID -> Eval LazyValue) -- TODO can this be Eval ThunkID, not force evaluation?
  | VType Type
  | VAttr (Map Name val)
  | VList (Seq val)
  | VRT RTExprV
  | VFunc Dependencies (Either TempFuncID GUID)
  | VBlock Int
  | VVar Int
  | VRecCall Int
  deriving (Functor, Foldable, Traversable)

type RTExprV = ExpressionEnv () -> (Dependencies, RTExpr Int Int PreCall () ())

type Value = Fix ValueF

type LazyValue = ValueF ThunkID

data Thunk
  = Deferred (Eval LazyValue)
  | Computed LazyValue

newtype TypeVar = TypeVar (UF.Point (Set Type))

type Bindings = Map Name ThunkID

data StaticEnv = StaticEnv
  { _statBinds :: Map Name ThunkID,
    _statName :: Maybe Name,
    _statFile :: FilePath
  }

type DynamicEnv = [FunctionSig]

type FunctionSig = ([(Name, TypeVar)], TypeVar)

data ExpressionEnv tv = ExpressionEnv
  { _eeVarStack :: [tv],
    _eeBlockStack :: [tv],
    _eeExpr :: tv,
    _eeFn :: tv
  }

makeLenses ''StaticEnv
makeLenses ''ExpressionEnv
makeLenses ''EvalState

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

describeValue :: ValueF f -> String
describeValue (VPrim (PInt _)) = "integer"
describeValue (VPrim (PDouble _)) = "double"
describeValue (VPrim (PBool _)) = "boolean"
describeValue (VPrim (PString _)) = "string"
describeValue VClosure {} = "closure"
describeValue VClosure' {} = "closure"
describeValue VType {} = "type"
describeValue VAttr {} = "attribute set"
describeValue VVar {} = "runtime variable"
describeValue VBlock {} = "block label"
describeValue VRecCall {} = "recursive function reference"
describeValue VRT {} = "code block"
describeValue VFunc {} = "runtime function"
describeValue VList {} = "list"

runEval :: Eval a -> IO (Either String a)
runEval (Eval m) =
  runExceptT $
    flip evalStateT st0 $
      runCoroutine m
  where
    st0 = EvalState mempty 0

deferAttrs :: [(Name, ValueF Void)] -> Eval ThunkID
deferAttrs attrs = do
  attrs' <- (traverse . traverse) (deferVal . fmap absurd) attrs
  deferVal $ VAttr $ M.fromList attrs'

deepEval :: ThunkID -> Eval Value
deepEval tid = Fix <$> (force tid >>= traverse deepEval)

freshId :: Eval Int
freshId = state (\(EvalState ts n) -> (n, EvalState ts (n + 1)))

freshThunkId :: Eval ThunkID
freshThunkId = ThunkID <$> freshId

freshVarId :: Eval VarID
freshVarId = VarID <$> freshId

freshLblId :: Eval LabelID
freshLblId = LabelID <$> freshId

freshFuncId :: Eval TempFuncID
freshFuncId = TempFuncID <$> freshId

mkThunk :: Thunk -> Eval ThunkID
mkThunk thunk = state $ \(EvalState ts n) -> (ThunkID n, EvalState (M.insert (ThunkID n) thunk ts) (n + 1))

setThunk :: ThunkID -> Thunk -> Eval ()
setThunk tid thunk = thunks . at tid ?= thunk

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

deferVal :: LazyValue -> Eval ThunkID
deferVal = mkThunk . Computed

force :: ThunkID -> Eval LazyValue
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
