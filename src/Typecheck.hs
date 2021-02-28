{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Typecheck (typecheckBlock, typecheckFunction) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.ST
import Data.Bifunctor
import Data.Map (Map)
import Data.Map qualified as M
import Data.UnionFind.ST qualified as UF
import Expr
import Lens.Micro.Platform
import Program

newtype TypeVar s info = TypeVar (UF.Point s (Map Type info))

data TypecheckContext s info = TypecheckContext
  { _tcBinds :: Map Name (TypeVar s info),
    _tcBlock :: TypeVar s info,
    _tcSelf :: Maybe ([TypeVar s info], TypeVar s info),
    _tcEnv :: RuntimeEnv
  }

makeLenses ''TypecheckContext

type Typecheck s info = ReaderT (TypecheckContext s info) (ST s)

typecheckBlock ::
  RuntimeEnv ->
  RTBlock (Maybe Type) ->
  Either String (Type, RTBlock Type)
typecheckBlock env (Block stmts) = runTypecheckT env setup run
  where
    setup = (,Nothing) <$> fresh'
    run = (fmap . fmap . fmap) Block (typecheck stmts)

typecheckFunction ::
  RuntimeEnv ->
  Name ->
  [(Name, Type)] ->
  Type ->
  RTBlock (Maybe Type) ->
  Either String FunDef
typecheckFunction env name args ret (Block stmts) = runTypecheckT env setup run
  where
    setup = do
      vRet <- fresh'
      setType' vRet ret ()
      args' <- (traverse . traverse) (flip freshFrom' ()) args
      pure (vRet, Just (args', vRet))
    run =
      (fmap . fmap)
        (\(_, stmts') -> FunDef args ret name (Block stmts'))
        (typecheck stmts)

typecheck ::
  [Stmt (Maybe Type) (RTExpr (Maybe Type) (Maybe Type))] ->
  Typecheck s () (Either String (Type, [Stmt Type (RTExpr Type Type)]))
typecheck stmts = do
  stmts' <- checkStmts stmts
  runExceptT $ do
    tRet <- view tcBlock >>= getTypeInExceptT
    tStmts <- (traverse . traverseStmtTypes) getTypeInExceptT stmts'
    pure (tRet, tStmts)

checkStmts ::
  [Stmt (Maybe Type) (RTExpr (Maybe Type) (Maybe Type))] ->
  Typecheck s () [Stmt (TypeVar s ()) (RTExpr (TypeVar s ()) (TypeVar s ()))]
checkStmts [] = do
  ret <- view tcBlock
  [] <$ setType ret TVoid ()
checkStmts [Return expr] = do
  ret <- view tcBlock
  expr' <- checkRTExpr expr
  unify ret (rtInfo expr')
  pure [Return expr']
checkStmts (Return expr : r) = do
  ret <- view tcBlock
  expr' <- checkRTExpr expr
  unify ret (rtInfo expr')
  r' <- checkStmts r
  pure (Return expr' : r')
checkStmts (Decl name mtyp expr : r) = do
  var <- freshMaybe mtyp ()
  expr' <- checkRTExpr expr
  unify var (rtInfo expr')
  r' <- bind name var $ checkStmts r
  pure (Decl name var expr' : r')
checkStmts (Assign name expr : r) = do
  expr' <- checkRTExpr expr
  view (tcBinds . at name) >>= \case
    Nothing -> error "you're referincing a function that doesn't exit in the environment, this should be impossible"
    Just var -> unify var (rtInfo expr')
  r' <- checkStmts r
  pure (Assign name expr' : r')
checkStmts (ExprStmt expr : r) = do
  expr' <- checkRTExpr expr
  r' <- checkStmts r
  pure (ExprStmt expr' : r')

checkRTExpr ::
  RTExpr (Maybe Type) (Maybe Type) ->
  Typecheck s () (RTExpr (TypeVar s ()) (TypeVar s ()))
checkRTExpr (RTVar name mtyp) = do
  var <- freshMaybe mtyp ()
  view (tcBinds . at name) >>= \case
    Nothing -> pure (RTVar name var)
    Just var' -> RTVar name var <$ unify var var'
checkRTExpr (RTLiteral lit mtyp) = RTLiteral lit <$> freshMaybe mtyp ()
checkRTExpr (RTArith op a b mtyp) = do
  var <- freshMaybe mtyp ()
  a' <- checkRTExpr a
  b' <- checkRTExpr b
  unify var (rtInfo a')
  unify (rtInfo a') (rtInfo b')
  pure $ RTArith op a' b' var
checkRTExpr (RTBlock (Block stmts) mtyp) = do
  var <- freshMaybe mtyp ()
  stmts' <- local (tcBlock .~ var) $ checkStmts stmts
  pure (RTBlock (Block stmts') var)
checkRTExpr (RTCall fn args mtyp) = do
  (vargs, vret) <- callSig fn
  var <- freshMaybe mtyp ()
  unify var vret
  let f expr typ = do
        expr' <- checkRTExpr expr
        unify (rtInfo expr') typ
        pure expr'
  args' <- zipWithM f args vargs
  pure $ RTCall (Left Self) args' var

callSig :: Either Self GUID -> Typecheck s () ([TypeVar s ()], TypeVar s ())
callSig efn = do
  RuntimeEnv env <- view tcEnv
  case efn of
    Left Self ->
      view tcSelf >>= \case
        Nothing -> error "calling self in a non-function"
        Just r -> pure r
    Right guid -> case M.lookup guid env of
      Nothing -> error "calling nonexisting guid"
      Just (FunDef args ret _ _) -> do
        vret <- freshFrom ret ()
        vargs <- forM args $ \(_, arg) -> freshFrom arg ()
        pure (vargs, vret)

freshFrom' :: Type -> info -> ST s (TypeVar s info)
freshFrom' typ info = do
  var <- fresh'
  setType' var typ info
  pure var

freshFrom :: Type -> info -> Typecheck s info (TypeVar s info)
freshFrom typ info = lift $ freshFrom' typ info

freshMaybe :: Maybe Type -> info -> Typecheck s info (TypeVar s info)
freshMaybe mtyp info = do
  var <- fresh
  forM_ mtyp $ \typ -> setType var typ info
  pure var

getTypeInExceptT :: TypeVar s info -> ExceptT String (Typecheck s info) Type
getTypeInExceptT var = do
  lift (checkType var) >>= \case
    [(a, _)] -> pure a
    [] -> throwError "Unknown type for whatever"
    l -> throwError $ "Overdetermined: " <> show (fst <$> l)

traverseStmtTypes :: Applicative m => (typ -> m typ') -> Stmt typ (RTExpr typ typ) -> m (Stmt typ' (RTExpr typ' typ'))
traverseStmtTypes f (Return expr) = Return <$> traverseRTExprTypes f expr
traverseStmtTypes f (Decl name typ expr) = liftA2 (Decl name) (f typ) (traverseRTExprTypes f expr)
traverseStmtTypes f (Assign name expr) = Assign name <$> traverseRTExprTypes f expr
traverseStmtTypes f (ExprStmt expr) = ExprStmt <$> traverseRTExprTypes f expr

traverseRTExprTypes :: Applicative m => (typ -> m typ') -> RTExpr typ typ -> m (RTExpr typ' typ')
traverseRTExprTypes f = go
  where
    go (RTVar name typ) = RTVar name <$> f typ
    go (RTLiteral lit typ) = RTLiteral lit <$> f typ
    go (RTArith op a b typ) = liftA3 (RTArith op) (go a) (go b) (f typ)
    go (RTBlock (Block stmts) typ) =
      liftA2
        RTBlock
        (Block <$> (traverse . traverseStmtTypes) f stmts)
        (f typ)
    go (RTCall name args typ) = liftA2 (RTCall name) (traverse go args) (f typ)

bind :: Name -> TypeVar s info -> Typecheck s info a -> Typecheck s info a
bind n t = local (over tcBinds $ M.insert n t)

fresh :: Typecheck s info (TypeVar s info)
fresh = lift fresh'

unify :: Monoid info => TypeVar s info -> TypeVar s info -> Typecheck s info ()
unify ta tb = lift $ unify' ta tb

setType :: TypeVar s info -> Type -> info -> Typecheck s info ()
setType var typ info = lift $ setType' var typ info

fresh' :: ST s (TypeVar s info)
fresh' = TypeVar <$> UF.fresh mempty

unify' :: Monoid info => TypeVar s info -> TypeVar s info -> ST s ()
unify' (TypeVar ta) (TypeVar tb) = UF.union' ta tb (\sa sb -> pure (M.unionWith mappend sa sb))

setType' :: TypeVar s info -> Type -> info -> ST s ()
setType' (TypeVar t) typ info = UF.modifyDescriptor t (M.insert typ info)

runTypecheckT ::
  RuntimeEnv ->
  (forall s. ST s (TypeVar s info, Maybe ([(Name, TypeVar s info)], TypeVar s info))) ->
  (forall s. Typecheck s info a) ->
  a
runTypecheckT env setup run = runST $ do
  (blk, mfun) <- setup
  let args = maybe [] fst mfun
  let ctx = TypecheckContext mempty blk (fmap (first (fmap snd)) mfun) env
  flip runReaderT ctx $ foldr (uncurry bind) run args

checkType :: TypeVar s info -> Typecheck s info [(Type, info)]
checkType var = M.toList <$> tlookup var
  where
    tlookup :: TypeVar s info -> Typecheck s info (Map Type info)
    tlookup (TypeVar p) = lift $ UF.repr p >>= UF.descriptor
