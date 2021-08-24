{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO
-- vars have the type of the underlying variable. Maybe this should be a Ref
-- type, and automatically deref?  Also relates to linearity

-- TODO
-- Once there are proper function types, we can treat `fun` the same as `var`
-- and `blk`

module Eval.Typecheck (typeCheck) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.ST
import Data.HashMap.Strict (HashMap)
import Data.Map (Map)
import Data.Sequence qualified as Seq
import Data.UnionFind.ST qualified as UF
import Data.Void
import Eval.Lib (labels, structuralZip, types, vars)
import Eval.Types
import Expr (Prim)
import Lens.Micro.Platform

typeCheck ::
  RTValue (Bind Int Void) Void (Either FuncIX Hash) Prim () ->
  [Type] ->
  Type ->
  HashMap Hash Sig ->
  Map FuncIX Sig ->
  Either String (RTValue (Bind Int Void) Void (Either FuncIX Hash) Prim Type)
typeCheck body args ret closedSigs openSigs =
  runCheck (closedSigs, openSigs) $ do
    ctx <- freshT ret
    -- TODO somehow check _once_ that the length of argument list is correct, and then reuse proof
    let instantiateArg :: Bind Int Void -> Check s (Typed s (Bind Int Void))
        instantiateArg (Free v) = absurd v
        instantiateArg (Bound argIx) = case args ^? ix argIx of
          Just typ -> Typed (Bound argIx) <$> freshT typ
          Nothing -> throwError "Impossible"
        body' = over labels absurd body
    -- TODO
    -- After type checking is done, make sure that primitives have the correct type
    -- Currently, ints might still be doubles
    vars instantiateArg body' >>= checkValue ctx >>= types (resolve . getVar)

type Check s = ReaderT Sigs (ExceptT String (ST s))

type Sigs = (HashMap Hash Sig, Map FuncIX Sig)

runCheck :: Sigs -> (forall s. Check s a) -> Either String a
runCheck deps m = runST $ runExceptT $ runReaderT m deps

newtype TypeVar s = TypeVar (UF.Point s (Maybe (TypeF (TypeVar s))))

joinMaybes :: Applicative m => (a -> a -> m a) -> Maybe a -> Maybe a -> m (Maybe a)
joinMaybes f (Just a) (Just b) = Just <$> f a b
joinMaybes _ Nothing b = pure b
joinMaybes _ a Nothing = pure a

unify :: TypeVar s -> TypeVar s -> Check s ()
unify (TypeVar a) (TypeVar b) = do
  mta <- liftST $ UF.descriptor a
  mtb <- liftST $ UF.descriptor b
  mt <- joinMaybes unifyTypes mta mtb
  liftST $ UF.union a b >> UF.setDescriptor a mt
  where
    unifyTypes :: TypeF (TypeVar s) -> TypeF (TypeVar s) -> Check s (TypeF (TypeVar s))
    unifyTypes a b = structuralZip (\a b -> a <$ unify a b) err a b
      where
        err = throwError $ "Couldn't unify " <> show (() <$ a) <> " with " <> show (() <$ b)

liftST :: ST s a -> Check s a
liftST = lift . lift

fresh :: Maybe (TypeF (TypeVar s)) -> Check s (TypeVar s)
fresh j = liftST $ TypeVar <$> UF.fresh j

freshT :: Type -> Check s (TypeVar s)
freshT (Type t) = traverse freshT t >>= fresh . Just

resolve :: TypeVar s -> Check s Type
resolve var = fmap Type $ resolve1 var >>= traverse resolve

resolve1 :: TypeVar s -> Check s (TypeF (TypeVar s))
resolve1 (TypeVar v) = do
  liftST (UF.descriptor v) >>= \case
    Just t -> pure t
    Nothing -> pure TVoid

lookupSig :: Either FuncIX Hash -> Check s Sig
lookupSig func = view (lens func) >>= maybe err pure
  where
    err = do
      env <- ask
      throwError $ "Impossible, could not find " <> show func <> " in env " <> show env
    lens :: Either FuncIX Hash -> Lens' Sigs (Maybe Sig)
    lens = either (\i -> _2 . at i) (\h -> _1 . at h)

data Typed s a = Typed
  { getAnn :: a,
    getVar :: TypeVar s
  }
  deriving (Functor)

-- checkLit (RTTuple t) = traverse checkLit t >>= fresh . Exactly . TTuple

-- TODO like other placees, this should be renamed to something more expr-y
checkValue ::
  forall a s var blk lit.
  TypeVar s ->
  RTValue (Typed s var) (Typed s blk) (Either FuncIX Hash) lit a ->
  Check s (RTValue var blk (Either FuncIX Hash) lit (Typed s a))
checkValue ctx (RTArith op l r a) = do
  l' <- checkValue ctx l
  r' <- checkValue ctx r
  pure $ RTArith op l' r' (Typed a ctx)
checkValue ctx (RTComp op l r a) = do
  freshT (Type TBool) >>= unify ctx
  var <- fresh Nothing
  l' <- checkValue var l
  r' <- checkValue var r
  pure $ RTComp op l' r' (Typed a ctx)
checkValue ctx (RTLit prim a) = pure $ RTLit prim (Typed a ctx)
checkValue ctx (RTTuple tup a) = do
  (types, exprs) <- fmap Seq.unzip $
    forM tup $ \expr -> do
      var <- fresh Nothing
      expr' <- checkValue var expr
      pure (var, expr')
  fresh (Just $ TTuple types) >>= unify ctx
  pure $ RTTuple exprs (Typed a ctx)
checkValue ctx (RTCond c t f a) = do
  vc <- freshT (Type TBool)
  c' <- checkValue vc c
  t' <- checkValue ctx t
  f' <- checkValue ctx f
  pure $ RTCond c' t' f' (Typed a ctx)
checkValue ctx (ValueSel haystack needle a) = do
  var <- fresh Nothing
  haystack' <- checkValue var haystack
  resolve1 var >>= \case
    (TTuple ts) -> case Seq.lookup needle ts of
      Nothing -> throwError "Tuple index out of bounds"
      Just t -> unify t ctx
    _ -> throwError "Indexing into something that's not a tuple"
  pure $ ValueSel haystack' needle (Typed a ctx)
checkValue ctx (Call fn args a) = do
  (argTypes, ret) <- lookupSig fn
  freshT ret >>= unify ctx
  -- TODO again, prove arg lengths _once_, somehow
  unless (length args == length argTypes) $ throwError "Argument length mismatch"
  args' <- zipWithM (\val typ -> freshT typ >>= \var -> checkValue var val) args argTypes
  pure $ Call fn args' (Typed a ctx)
checkValue ctx (PlaceVal plc a) = do
  plc' <- checkPlace ctx plc
  pure $ PlaceVal plc' (Typed a ctx)
checkValue ctx (Block blk a) = do
  let instantiateLabel :: Bind () (Typed s blk) -> Typed s (Bind () blk)
      instantiateLabel (Bound ()) = Typed (Bound ()) ctx
      instantiateLabel (Free t) = Free <$> t
  blk' <- checkProg ctx (over labels instantiateLabel blk)
  pure (Block blk' (Typed a ctx))
checkValue ctx (RTRef plc a) = do
  vInner <- fresh Nothing
  plc' <- checkPlace vInner plc
  fresh (Just $ TPtr vInner) >>= unify ctx
  pure $ RTRef plc' (Typed a ctx)

checkPlace ::
  TypeVar s ->
  RTPlace (Typed s var) (Typed s blk) (Either FuncIX Hash) lit a ->
  Check s (RTPlace var blk (Either FuncIX Hash) lit (Typed s a))
checkPlace ctx (Place (Typed var t) a) = do
  unify ctx t
  pure $ Place var (Typed a ctx)
checkPlace ctx (PlaceSel haystack needle a) = do
  -- TODO combine with ValueSel
  var <- fresh Nothing
  haystack' <- checkPlace var haystack
  resolve1 var >>= \case
    (TTuple ts) -> case Seq.lookup needle ts of
      Nothing -> throwError "Tuple index out of bounds"
      Just t -> unify t ctx
    _ -> throwError "Indexing into something that's not a tuple"
  pure $ PlaceSel haystack' needle (Typed a ctx)
checkPlace ctx (RTDeref val a) = do
  var <- fresh Nothing
  val' <- checkValue var val
  resolve1 var >>= \case
    TPtr t -> unify t ctx
    _ -> throwError "Dereferencing something that's not a pointer"
  pure $ RTDeref val' (Typed a ctx)

-- TODO
-- Break always takes a label but ExprStmt still allows breaking to an implicit
-- labe.  A terminating ExprStmt should be replaced by a (labeled) break for
-- consistency, which would remove the need for the block context type variable to be
-- passed around.
checkProg ::
  forall a s var blk lit.
  TypeVar s ->
  RTProg (Typed s var) (Typed s blk) (Either FuncIX Hash) lit a ->
  Check s (RTProg var blk (Either FuncIX Hash) lit (Typed s a))
checkProg blk (Decl mtyp val k) = do
  ctx <- fresh Nothing
  forM_ mtyp $ freshT >=> unify ctx
  val' <- checkValue ctx val
  let instantiateVar :: Bind () (Typed s var) -> Typed s (Bind () var)
      instantiateVar (Bound ()) = Typed (Bound ()) ctx
      instantiateVar (Free t) = Free <$> t
  k' <- checkProg blk $ over vars instantiateVar k
  pure $ Decl mtyp val' k'
checkProg blk (Assign lhs rhs k) = do
  var <- fresh Nothing
  lhs' <- checkPlace var lhs
  rhs' <- checkValue var rhs
  k' <- checkProg blk k
  pure $ Assign lhs' rhs' k'
checkProg _ (Break (Typed lbl var) val) = Break lbl <$> checkValue var val
checkProg _ (Continue (Typed lbl _)) = pure $ Continue lbl
checkProg blk (ExprStmt val Nothing) = do
  val' <- checkValue blk val
  pure $ ExprStmt val' Nothing
checkProg blk (ExprStmt val (Just k)) = do
  var <- fresh Nothing
  val' <- checkValue var val
  ExprStmt val' . Just <$> checkProg blk k
