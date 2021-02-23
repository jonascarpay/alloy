{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Typecheck
  ( typecheckBlock,
    typecheckFunction,
  )
where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.ST
import Data.Map (Map)
import Data.Map qualified as M
import Data.UnionFind.ST qualified as UF
import Expr
import Program

typecheckFunction ::
  RuntimeEnv ->
  [(Name, Type)] ->
  Type ->
  RTBlock (Maybe Type) ->
  Either String Function
typecheckFunction env args ret (Block stmts) =
  (\(_, stmts') -> Function args ret (Block stmts')) <$> typecheckStmts env (Just ret) args stmts

typecheckBlock ::
  RuntimeEnv ->
  Maybe Type ->
  RTBlock (Maybe Type) ->
  Either String (Type, RTBlock Type)
typecheckBlock env ret (Block blk) = fmap Block <$> typecheckStmts env ret [] blk

typecheckStmts ::
  RuntimeEnv ->
  Maybe Type ->
  [(Name, Type)] ->
  [Stmt (Maybe Type) (RTExpr (Maybe Type) (Maybe Type))] ->
  Either String (Type, [Stmt Type (RTExpr Type Type)])
typecheckStmts env ret args stmts = runTypecheckT $ do
  vRet <- fresh
  forM_ ret (\t -> setType vRet t ())
  let f :: (Name, Type) -> Typecheck ctx () a -> Typecheck ctx () a
      f (name, typ) cont = do
        var <- fresh
        setType var typ ()
        bind name var cont
  stmts' <- flip (foldr f) args $ checkStmts env vRet stmts
  runExceptT $ do
    tRet <- getTypeInExceptT vRet
    tStmts <- (traverse . traverseStmtTypes) getTypeInExceptT stmts'
    pure (tRet, tStmts)

checkStmts ::
  RuntimeEnv ->
  TypeVar ctx () ->
  [Stmt (Maybe Type) (RTExpr (Maybe Type) (Maybe Type))] ->
  Typecheck ctx () [Stmt (TypeVar ctx ()) (RTExpr (TypeVar ctx ()) (TypeVar ctx ()))]
checkStmts _ ret [] = [] <$ setType ret TVoid ()
checkStmts env ret [Return expr] = do
  expr' <- checkRTExpr env expr
  unify ret (rtInfo expr')
  pure [Return expr']
checkStmts env ret (Return expr : r) = do
  expr' <- checkRTExpr env expr
  unify ret (rtInfo expr')
  r' <- checkStmts env ret r
  pure (Return expr' : r')
checkStmts env ret (Decl name mtyp expr : r) = do
  var <- freshMaybe mtyp ()
  expr' <- checkRTExpr env expr
  unify var (rtInfo expr')
  r' <- bind name var $ checkStmts env ret r
  pure (Decl name var expr' : r')
checkStmts env ret (Assign name expr : r) = do
  expr' <- checkRTExpr env expr
  asks (M.lookup name) >>= \case
    Nothing -> undefined
    Just var -> unify var (rtInfo expr')
  r' <- checkStmts env ret r
  pure (Assign name expr' : r')
checkStmts env ret (ExprStmt expr : r) = do
  expr' <- checkRTExpr env expr
  r' <- checkStmts env ret r
  pure (ExprStmt expr' : r')

checkRTExpr ::
  RuntimeEnv ->
  RTExpr (Maybe Type) (Maybe Type) ->
  Typecheck ctx () (RTExpr (TypeVar ctx ()) (TypeVar ctx ()))
checkRTExpr _ (RTVar name mtyp) = do
  var <- freshMaybe mtyp ()
  asks (M.lookup name) >>= \case
    Nothing -> pure (RTVar name var)
    Just var' -> RTVar name var <$ unify var var'
checkRTExpr _ (RTLiteral lit mtyp) = RTLiteral lit <$> freshMaybe mtyp ()
checkRTExpr env (RTArith op a b mtyp) = do
  var <- freshMaybe mtyp ()
  a' <- checkRTExpr env a
  b' <- checkRTExpr env b
  unify var (rtInfo a')
  unify (rtInfo a') (rtInfo b')
  pure $ RTArith op a' b' var
checkRTExpr env (RTBlock (Block stmts) mtyp) = do
  var <- freshMaybe mtyp ()
  stmts' <- checkStmts env var stmts
  pure (RTBlock (Block stmts') var)
checkRTExpr (RuntimeEnv env) (RTCall name args mtyp) =
  case M.lookup name env of
    Just (Function fargs fret _) -> do
      var <- freshMaybe mtyp ()
      setType var fret ()
      let f expr typ = do
            expr' <- checkRTExpr (RuntimeEnv env) expr
            setType (rtInfo expr') typ ()
            pure expr'
      args' <- zipWithM f args (snd <$> fargs)
      pure $ RTCall name args' var
    Nothing -> error "report this immediately"

freshMaybe :: Maybe Type -> info -> Typecheck ctx info (TypeVar ctx info)
freshMaybe mtyp info = do
  var <- fresh
  forM_ mtyp $ \typ -> setType var typ info
  pure var

getTypeInExceptT :: TypeVar ctx info -> ExceptT String (Typecheck ctx info) Type
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

newtype TypeVar ctx info = TypeVar (UF.Point ctx (Map Type info))

type Typecheck ctx info = ReaderT (Context ctx info) (ST ctx)

type Context ctx info = Map Name (TypeVar ctx info)

bind :: Name -> TypeVar ctx info -> Typecheck ctx info a -> Typecheck ctx info a
bind n t = local (M.insert n t)

fresh :: Typecheck ctx info (TypeVar ctx info)
fresh = lift $ TypeVar <$> UF.fresh mempty

unify :: Monoid info => TypeVar ctx info -> TypeVar ctx info -> Typecheck ctx info ()
unify (TypeVar ta) (TypeVar tb) =
  lift $
    UF.union' ta tb (\sa sb -> pure (M.unionWith mappend sa sb))

setType :: TypeVar ctx info -> Type -> info -> Typecheck ctx info ()
setType (TypeVar t) typ info =
  lift $
    UF.modifyDescriptor t (M.insert typ info)

tlookup :: TypeVar ctx info -> Typecheck ctx info (Map Type info)
tlookup (TypeVar p) = lift $ UF.repr p >>= UF.descriptor

runTypecheckT :: (forall ctx. Typecheck ctx info a) -> a
runTypecheckT m = runST (runReaderT m mempty)

checkType :: TypeVar ctx info -> Typecheck ctx info [(Type, info)]
checkType var = M.toList <$> tlookup var
