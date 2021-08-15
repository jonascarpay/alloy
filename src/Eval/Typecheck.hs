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
import Data.Set (Set)
import Data.Set qualified as S
import Data.UnionFind.ST qualified as UF
import Data.Void
import Eval.Lib (closedOver, labels, types, vars)
import Eval.Types
import Expr
import Lens.Micro.Platform

-- TypeVar s changes a lot, but Deps never changes, so we put them at different points in the stack
type Check s = ReaderT Sigs (ExceptT String (ST s))

type Sigs = (HashMap Hash Sig, Map FuncIX Sig)

runCheck :: Sigs -> (forall s. Check s a) -> Either String a
runCheck deps m = runST $ runExceptT $ runReaderT m deps

newtype TypeVar s = TypeVar (UF.Point s (Judgement s))

-- TODO there is probably some sort of better, lattice-y name (class?) for this
data Judgement s
  = Any
  | OneOf (Set2 (TypeF Void)) -- TODO ambiguities are kind of primitive atm
  | Exactly (TypeF (TypeVar s))

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

unify :: TypeVar s -> TypeVar s -> Check s ()
unify (TypeVar a) (TypeVar b) = do
  ja <- liftST $ UF.descriptor a
  jb <- liftST $ UF.descriptor b
  jc <- ja <=> jb
  liftST $ UF.union a b >> UF.setDescriptor a jc
  where
    describeType :: TypeF a -> String
    describeType TVoid = "void"
    describeType TBool = "bool"
    describeType TInt = "int"
    describeType TDouble = "double"
    describeType (TTuple _) = "tuple"

    (<+>) :: TypeF (TypeVar s) -> TypeF (TypeVar s) -> Check s (TypeF (TypeVar s))
    TVoid <+> TVoid = pure TVoid
    TBool <+> TBool = pure TBool
    TInt <+> TInt = pure TInt
    TDouble <+> TDouble = pure TDouble
    TTuple as <+> TTuple bs = do
      when (Seq.length as /= Seq.length bs) $ throwError "tuple length mismatch"
      TTuple <$> sequence (Seq.zipWith (\a b -> a <$ unify a b) as bs)
    a <+> b = throwError $ "Type mismatch between " <> describeType a <> " and " <> describeType b

    (<=>) :: Judgement s -> Judgement s -> Check s (Judgement s)
    Any <=> t = pure t
    OneOf t <=> Any = pure (OneOf t)
    OneOf a <=> OneOf b = intersection2 (pure . OneOf) (pure . Exactly . fmap absurd) (throwError "Nil intersection") a b
    OneOf ts <=> Exactly t
      | Just t' <- closedOver traverse t, member2 t' ts = pure (Exactly t)
      | otherwise = throwError "Nil intersection"
    Exactly t <=> Any = pure (Exactly t)
    Exactly t <=> OneOf ts
      | Just t' <- closedOver traverse t, member2 t' ts = pure (Exactly t)
      | otherwise = throwError "Nil intersection"
    Exactly a <=> Exactly b = Exactly <$> (a <+> b)

liftST :: ST s a -> Check s a
liftST = lift . lift

fresh :: Judgement s -> Check s (TypeVar s)
fresh j = liftST $ TypeVar <$> UF.fresh j

fromType :: Type -> Check s (TypeVar s)
fromType (Type t) = traverse fromType t >>= fresh . Exactly

fromTypeF :: TypeF Type -> Check s (TypeVar s)
fromTypeF = fromType . Type

resolve :: TypeVar s -> Check s Type
resolve (TypeVar v) =
  liftST (UF.descriptor v) >>= \case
    Any -> pure (Type TVoid)
    (OneOf ts) -> Type . minimum <$> (traverse . traverse) resolve (fmap absurd <$> toList2 ts)
    (Exactly t) -> Type <$> traverse resolve t

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

typeCheck ::
  RTValue () RTLit (Bind Int Void) Void (Either FuncIX Hash) ->
  [Type] ->
  Type ->
  HashMap Hash Sig ->
  Map FuncIX Sig ->
  Either String (RTValue Type RTLit (Bind Int Void) Void (Either FuncIX Hash))
typeCheck body args ret closedSigs openSigs =
  runCheck (closedSigs, openSigs) $ do
    ctx <- fromType ret
    -- TODO somehow check _once_ that the length of argument list is correct, and then reuse proof
    let instantiateArg :: Bind Int Void -> Check s (Typed s (Bind Int Void))
        instantiateArg (Free v) = absurd v
        instantiateArg (Bound argIx) = case args ^? ix argIx of
          Just typ -> Typed (Bound argIx) <$> fromType typ
          Nothing -> throwError "Impossible"
        body' = over labels absurd body
    vars instantiateArg body' >>= checkValue ctx >>= types (resolve . getVar)

checkLit :: RTLit -> Check s (TypeVar s)
checkLit (RTPrim prim) = case prim of
  PInt _ -> fresh (OneOf (set2 TInt TDouble))
  PDouble _ -> fromTypeF TDouble
  PBool _ -> fromTypeF TBool
  PVoid -> fromTypeF TVoid
checkLit (RTTuple t) = traverse checkLit t >>= fresh . Exactly . TTuple

-- TODO like other placees, this should be renamed to something more expr-y
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
  fromType (Type TBool) >>= unify ctx
  var <- fresh Any
  l' <- checkValue var l
  r' <- checkValue var r
  pure $ RTComp op l' r' (Typed a ctx)
checkValue ctx (RTLit lit a) = do
  checkLit lit >>= unify ctx
  pure $ RTLit lit (Typed a ctx)
checkValue ctx (RTCond c t f a) = do
  vc <- fromTypeF TBool
  c' <- checkValue vc c
  t' <- checkValue ctx t
  f' <- checkValue ctx f
  pure $ RTCond c' t' f' (Typed a ctx)
checkValue ctx (Call fn args a) = do
  (argTypes, ret) <- lookupSig fn
  fromType ret >>= unify ctx
  -- TODO again, prove arg lengths _once_, somehow
  unless (length args == length argTypes) $ throwError "Argument length mismatch"
  args' <- zipWithM (\val typ -> fromType typ >>= \var -> checkValue var val) args argTypes
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
  ctx <- fresh Any
  forM_ mtyp $ fromType >=> unify ctx
  val' <- checkValue ctx val
  let instantiateVar :: Bind () (Typed s var) -> Typed s (Bind () var)
      instantiateVar (Bound ()) = Typed (Bound ()) ctx
      instantiateVar (Free t) = Free <$> t
  k' <- checkProg blk $ over vars instantiateVar k
  pure $ Decl mtyp val' k'
checkProg blk (Assign lhs rhs k) = do
  var <- fresh Any
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
  var <- fresh Any
  val' <- checkValue var val
  ExprStmt val' . Just <$> checkProg blk k
