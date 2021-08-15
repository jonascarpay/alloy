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
import Data.Set (Set)
import Data.Set qualified as S
import Data.UnionFind.ST qualified as UF
import Data.Void
import Eval.Lib (labels, types, vars)
import Eval.Types
import Expr
import Lens.Micro.Platform

-- TypeVar s changes a lot, but Deps never changes, so we put them at different points in the stack
type Check s = ReaderT Sigs (ExceptT String (ST s))

type Sigs = (HashMap Hash Sig, Map FuncIX Sig)

runCheck :: Sigs -> (forall s. Check s a) -> Either String a
runCheck deps m = runST $ runExceptT $ runReaderT m deps

newtype TypeVar s = TypeVar (UF.Point s (Judgement Type))

-- TODO there is probably some sort of better, lattice-y name (class?) for this
-- TODO is this where we should collect errors? Not just throw them?
data Judgement a
  = Any
  | OneOf (Set2 a)
  | Exactly a
  | None
  | Mismatch (Set2 a)

newtype Set2 a = Set2 (Set a)

instance Ord a => Semigroup (Set2 a) where
  Set2 a <> Set2 b = Set2 (a <> b)

set2 :: Ord a => a -> a -> Set2 a
set2 a b = Set2 $ S.fromList [a, b]

insert2 :: Ord a => a -> Set2 a -> Set2 a
insert2 a (Set2 as) = Set2 (S.insert a as)

intersection2 :: Ord a => (Set2 a -> r) -> (a -> r) -> r -> (Set2 a -> Set2 a -> r)
intersection2 fn f1 f0 (Set2 a) (Set2 b) = case S.toList s of
  [] -> f0
  [e] -> f1 e
  _ -> fn (Set2 s)
  where
    s = S.intersection a b

member2 :: Ord a => a -> Set2 a -> Bool
member2 a (Set2 as) = S.member a as

toList2 :: Set2 a -> [a]
toList2 (Set2 as) = S.toList as

minimum2 :: Set2 a -> a
minimum2 (Set2 as) = S.findMin as

instance Ord a => Semigroup (Judgement a) where
  Any <> t = t
  OneOf t <> Any = OneOf t
  OneOf a <> OneOf b = intersection2 OneOf Exactly None a b
  OneOf ts <> Exactly t = if member2 t ts then Exactly t else None
  OneOf _ <> None = None
  OneOf _ <> Mismatch ts = Mismatch ts
  Exactly t <> Any = Exactly t
  Exactly t <> OneOf ts = if member2 t ts then Exactly t else None
  Exactly a <> Exactly b = if a == b then Exactly a else Mismatch (set2 a b)
  Exactly _ <> None = None
  Exactly a <> Mismatch as = Mismatch (insert2 a as)
  None <> _ = None
  Mismatch as <> Mismatch bs = Mismatch (as <> bs)
  Mismatch ts <> _ = Mismatch ts

instance Ord a => Monoid (Judgement a) where
  mempty = Any

liftST :: ST s a -> Check s a
liftST = lift . lift

fresh :: Check s (TypeVar s)
fresh = liftST $ TypeVar <$> UF.fresh mempty

freshAt :: Type -> Check s (TypeVar s)
freshAt typ = do
  var <- fresh
  judge var $ Exactly typ
  pure var

unify :: TypeVar s -> TypeVar s -> Check s ()
unify (TypeVar a) (TypeVar b) = liftST $ UF.union' a b (\sa sb -> pure (sa <> sb))

judge :: TypeVar s -> Judgement Type -> Check s ()
judge (TypeVar a) typ = liftST $ UF.modifyDescriptor a (mappend typ)

resolve :: Typed s a -> Check s (Type, a)
resolve (Typed a (TypeVar v)) = do
  liftST (UF.descriptor v) >>= \case
    Any -> pure (TVoid, a)
    OneOf ts -> pure (minimum2 ts, a)
    Exactly t -> pure (t, a)
    None -> throwError "No valid types"
    Mismatch ts -> throwError $ "Mismatch: " <> show (toList2 ts)

lookupSig :: Either FuncIX Hash -> Check s Sig
lookupSig func = view (lens func) >>= maybe err pure
  where
    err = do
      env <- ask
      throwError $ "Impossible, could not find " <> show func <> " in env " <> show env
    lens :: Either FuncIX Hash -> Lens' Sigs (Maybe Sig)
    lens = either (\i -> _2 . at i) (\h -> _1 . at h)

data Typed s a = Typed a (TypeVar s)
  deriving (Functor)

typeCheck ::
  RTValue a RTLit (Bind Int Void) Void (Either FuncIX Hash) ->
  [Type] ->
  Type ->
  HashMap Hash Sig ->
  Map FuncIX Sig ->
  Either String (RTValue (Type, a) RTLit (Bind Int Void) Void (Either FuncIX Hash))
typeCheck body args ret closedSigs openSigs =
  runCheck (closedSigs, openSigs) $ do
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
-- monomorphize where possible, roll up in a synonym
checkValue ::
  forall a s var blk.
  TypeVar s ->
  RTValue a RTLit (Typed s var) (Typed s blk) (Either FuncIX Hash) ->
  Check s (RTValue (Typed s a) RTLit var blk (Either FuncIX Hash))
checkValue ctx (RTArith op l r a) = do
  l' <- checkValue ctx l
  r' <- checkValue ctx r
  pure $ RTArith op l' r' (Typed a ctx)
checkValue ctx (RTComp op l r a) = do
  judge ctx $ Exactly TBool
  var <- fresh
  l' <- checkValue var l
  r' <- checkValue var r
  pure $ RTComp op l' r' (Typed a ctx)
checkValue ctx (RTLit lit a) = do
  case lit of
    RTPrim prim -> case prim of
      PInt _ -> judge ctx $ OneOf (set2 TInt TDouble)
      PDouble _ -> judge ctx $ Exactly TDouble
      PBool _ -> judge ctx $ Exactly TBool
      PVoid -> judge ctx $ Exactly TVoid
    RTTuple t -> undefined
  pure $ RTLit lit (Typed a ctx)
checkValue ctx (RTCond c t f a) = do
  vc <- freshAt TBool

  c' <- checkValue vc c
  t' <- checkValue ctx t
  f' <- checkValue ctx f
  pure $ RTCond c' t' f' (Typed a ctx)
checkValue ctx (Call fn args a) = do
  (argTypes, ret) <- lookupSig fn
  judge ctx $ Exactly ret
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
  RTPlace a RTLit (Typed s var) (Typed s blk) (Either FuncIX Hash) ->
  Check s (RTPlace (Typed s a) RTLit var blk (Either FuncIX Hash))
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
  RTProg a RTLit (Typed s var) (Typed s blk) (Either FuncIX Hash) ->
  Check s (RTProg (Typed s a) RTLit var blk (Either FuncIX Hash))
checkProg blk (Decl mtyp val k) = do
  ctx <- fresh
  forM_ mtyp $ judge ctx . Exactly
  val' <- checkValue ctx val
  let instantiateVar :: Bind () (Typed s var) -> Typed s (Bind () var)
      instantiateVar (Bound ()) = Typed (Bound ()) ctx
      instantiateVar (Free t) = Free <$> t
  k' <- checkProg blk $ over vars instantiateVar k
  pure $ Decl mtyp val' k'
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
  var <- fresh
  val' <- checkValue var val
  ExprStmt val' . Just <$> checkProg blk k
