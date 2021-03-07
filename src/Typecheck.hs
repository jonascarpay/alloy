{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Typecheck (typecheckBlock, typecheckFunction) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.ST
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
    _tcSelf :: SelfCtx s info,
    _tcDeps :: Dependencies
  }

type Typecheck s info = ReaderT (TypecheckContext s info) (ST s)

type SelfCtx s info = [([TypeVar s info], TypeVar s info)]

makeLenses ''TypecheckContext

typecheckBlock ::
  Dependencies ->
  RTBlock PreCall (Maybe Type) ->
  Either String (Type, RTBlock PreCall Type)
typecheckBlock env (Block stmts) = undefined --runTypecheckT env setup run
  where
    setup = (,Nothing) <$> fresh'
    run = (fmap . fmap . fmap) Block (typecheck stmts)

typecheckFunction ::
  Dependencies ->
  Name ->
  [(Name, Type)] ->
  Type ->
  RTBlock PreCall (Maybe Type) ->
  Either String (RTBlock PreCall Type)
typecheckFunction env name args ret (Block stmts) = undefined -- runTypecheckT env setup run
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

rtStmtTypes ::
  Traversal
    (Stmt typ (RTExpr PreCall typ typ))
    (Stmt typ' (RTExpr PreCall typ' typ'))
    typ
    typ'
rtStmtTypes f = stmtMasterTraversal (rtExprMasterTraversal pure f f pure pure) f pure

typecheck ::
  [Stmt (Maybe Type) (RTExpr PreCall (Maybe Type) (Maybe Type))] ->
  Typecheck s () (Either String (Type, [Stmt Type (RTExpr PreCall Type Type)]))
typecheck stmts = do
  stmts' <- checkStmts stmts
  runExceptT $ do
    tRet <- view tcBlock >>= getTypeInExceptT
    tStmts <- (traverse . rtStmtTypes) getTypeInExceptT stmts'
    pure (tRet, tStmts)

checkStmts ::
  [Stmt (Maybe Type) (RTExpr PreCall (Maybe Type) (Maybe Type))] ->
  Typecheck s () [Stmt (TypeVar s ()) (RTExpr PreCall (TypeVar s ()) (TypeVar s ()))]
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
  RTExpr PreCall (Maybe Type) (Maybe Type) ->
  Typecheck s () (RTExpr PreCall (TypeVar s ()) (TypeVar s ()))
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
  when (length args /= length vargs) $ error "argument length mismatch" -- TODO throw an error
  args' <- zipWithM f args vargs
  pure $ RTCall fn args' var

callSig :: PreCall -> Typecheck s () ([TypeVar s ()], TypeVar s ())
callSig efn = do
  Dependencies env tenv <- view tcDeps
  case efn of
    CallRec n ->
      view (tcSelf . to (safeLookup n)) >>= \case
        Nothing -> error "PreCall to self escapes scope"
        Just r -> pure r
    CallKnown guid -> case M.lookup guid env of
      Nothing -> error "calling nonexisting guid"
      Just (FunDef args ret _ _) -> do
        vret <- freshFrom ret ()
        vargs <- forM args $ \(_, arg) -> freshFrom arg ()
        pure (vargs, vret)

safeLookup :: Int -> [a] -> Maybe a
safeLookup n l = case splitAt n l of
  (_, a : _) -> Just a
  _ -> Nothing

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
  Dependencies ->
  (forall s. ST s (TypeVar s info, [([(Name, TypeVar s info)], TypeVar s info)])) ->
  (forall s. Typecheck s info a) ->
  a
runTypecheckT _ _ _ = undefined

-- runTypecheckT env setup run = runST $ do
--   (blk, mfun) <- setup
--   let args = maybe [] fst mfun
--   let ctx = TypecheckContext mempty blk (fmap (first (fmap snd)) undefined) env
--   flip runReaderT ctx $ foldr (uncurry bind) run args

checkType :: TypeVar s info -> Typecheck s info [(Type, info)]
checkType var = M.toList <$> tlookup var
  where
    tlookup :: TypeVar s info -> Typecheck s info (Map Type info)
    tlookup (TypeVar p) = lift $ UF.repr p >>= UF.descriptor
