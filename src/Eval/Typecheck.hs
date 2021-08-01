{-# LANGUAGE DeriveTraversable #-}
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
import Data.Set (Set)
import Data.Set qualified as S
import Data.UnionFind.ST qualified as UF
import Data.Void
import Eval.Lib (labels, types, vars)
import Eval.Types
import Expr
import Lens.Micro.Platform

valueType :: RTValue typ var lbl fun -> typ
valueType (RTArith _ _ _ t) = t
valueType (RTComp _ _ _ t) = t
valueType (RTCond _ _ _ t) = t
valueType (PlaceVal _ t) = t
valueType (Block _ t) = t
valueType (RTPrim _ t) = t
valueType (Call _ _ t) = t

placeType :: RTPlace typ var lbl fun -> typ
placeType (Place _ t) = t
placeType (Deref _ t) = t

-- TypeVar s changes a lot, but Deps never changes, so we put them at different points in the stack
type Check s = ReaderT Deps (ExceptT String (ST s))

runCheck :: Deps -> (forall s. Check s a) -> Either String a
runCheck deps m = runST $ runExceptT $ runReaderT m deps

newtype TypeVar s = TypeVar (UF.Point s (Set Type))

liftST :: ST s a -> Check s a
liftST = lift . lift

fresh :: Check s (TypeVar s)
fresh = liftST $ TypeVar <$> UF.fresh mempty

freshAt :: Type -> Check s (TypeVar s)
freshAt typ = do
  var <- fresh
  judge var typ
  pure var

unify :: TypeVar s -> TypeVar s -> Check s ()
unify (TypeVar a) (TypeVar b) = liftST $ UF.union' a b (\sa sb -> pure (sa <> sb))

judge :: TypeVar s -> Type -> Check s ()
judge (TypeVar a) typ = liftST $ UF.modifyDescriptor a (S.insert typ)

resolve :: Typed s a -> Check s (Type, a)
resolve (Typed a (TypeVar v)) = do
  typ <- liftST $ UF.descriptor v
  case S.toList typ of
    [] -> throwError "Ambiguous"
    [t] -> pure (t, a)
    ts -> throwError $ "Mismatch: " <> show ts

type Sig = ([Type], Type)

functionSig :: RTFunc a -> Sig
functionSig (RTFunc args ret _) = (args, ret)

lookupSig :: Either FuncIX Hash -> Check s Sig
lookupSig (Right hash) = view (closedFuncs . at hash) >>= maybe (throwError "Impossible") (pure . functionSig)
lookupSig (Left ix) = view openFuncs >>= either pure (const $ throwError "Impossible") . find
  where
    find :: Set CallGraph -> Either Sig ()
    find s = forM_ (S.toList s) $ \(CallGraph this body _ binds deps) -> do
      when (this == ix) $ Left (functionSig body)
      when (S.member ix binds) $ find deps

data Typed s a = Typed a (TypeVar s)
  deriving (Functor)

typeCheck ::
  RTValue a (Bind Int Void) Void (Either FuncIX Hash) ->
  [Type] ->
  Type ->
  Deps ->
  Either String (RTValue (Type, a) (Bind Int Void) Void (Either FuncIX Hash))
typeCheck body args ret deps =
  runCheck deps $ do
    ctx <- freshAt ret
    -- TODO somehow check _once_ that the length of argument list is correct, and then reuse proof
    let instantiateArg :: Bind Int Void -> Check s (Typed s (Bind Int Void))
        instantiateArg (Free v) = absurd v
        instantiateArg (Bound argIx) = case args ^? ix argIx of
          Just typ -> Typed (Bound argIx) <$> freshAt typ
          Nothing -> throwError "Impossible"
    body' <- vars instantiateArg $ over labels absurd body
    body'' <- checkValue ctx body'
    types resolve body''

-- TODO
-- like other placees, this should be renamed to something more expr-y
checkValue ::
  forall a s var blk.
  TypeVar s ->
  RTValue a (Typed s var) (Typed s blk) (Either FuncIX Hash) ->
  Check s (RTValue (Typed s a) var blk (Either FuncIX Hash))
checkValue ctx (RTArith op l r a) = do
  l' <- checkValue ctx l
  r' <- checkValue ctx r
  pure $ RTArith op l' r' (Typed a ctx)
checkValue ctx (RTComp op l r a) = do
  judge ctx TBool
  var <- fresh
  l' <- checkValue var l
  r' <- checkValue var r
  pure $ RTComp op l' r' (Typed a ctx)
checkValue ctx (RTPrim prim a) = do
  case prim of
    PInt _ -> pure () -- TODO restrict to int/double
    PDouble _ -> judge ctx TDouble
    PBool _ -> judge ctx TBool
    PVoid -> judge ctx TVoid
  pure $ RTPrim prim (Typed a ctx)
checkValue ctx (RTCond c t f a) = do
  vc <- freshAt TBool
  c' <- checkValue vc c
  t' <- checkValue ctx t
  f' <- checkValue ctx f
  pure $ RTCond c' t' f' (Typed a ctx)
checkValue ctx (Call fn args a) = do
  (argTypes, ret) <- lookupSig fn
  judge ctx ret
  -- TODO again, prove arg lengths _once_, somehow
  unless (length args == length argTypes) $ throwError "Argument lenght mismatch"
  args' <- zipWithM (\val typ -> freshAt typ >>= \var -> checkValue var val) args argTypes
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

checkPlace ::
  TypeVar s ->
  RTPlace a (Typed s var) (Typed s blk) (Either FuncIX Hash) ->
  Check s (RTPlace (Typed s a) var blk (Either FuncIX Hash))
checkPlace ctx (Place (Typed var t) a) = do
  unify ctx t
  pure $ Place var (Typed a ctx)
checkPlace _ _ = throwError "Deref type checkingnot yet implemented"

-- TODO
-- Break always takes a label but ExprStmt still allows breaking to an implicit
-- labe.  A terminating ExprStmt should be replaced by a (labeled) break for
-- consistency, which would remove the need for the block context type variable to be
-- passed around.
checkProg ::
  forall a s var blk.
  TypeVar s ->
  RTProg a (Typed s var) (Typed s blk) (Either FuncIX Hash) ->
  Check s (RTProg (Typed s a) var blk (Either FuncIX Hash))
checkProg blk (Decl typ val k) = do
  ctx <- freshAt typ
  val' <- checkValue ctx val
  let instantiateVar :: Bind () (Typed s var) -> Typed s (Bind () var)
      instantiateVar (Bound ()) = Typed (Bound ()) ctx
      instantiateVar (Free t) = Free <$> t
  k' <- checkProg blk $ over vars instantiateVar k
  pure $ Decl typ val' k'
checkProg blk (Assign lhs rhs k) = do
  var <- fresh
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
  val' <- checkValue blk val
  ExprStmt val' . Just <$> checkProg blk k
