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
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as S
import Data.UnionFind.ST qualified as UF
import Data.Void
import Eval.Lib (closedOver, labels, types, vars)
import Eval.Types
import Expr
import Lens.Micro.Platform

typeCheck ::
  RTValue () (Bind Int Void) Void (Either FuncIX Hash) ->
  [Type] ->
  Type ->
  HashMap Hash Sig ->
  Map FuncIX Sig ->
  Either String (RTValue Type (Bind Int Void) Void (Either FuncIX Hash))
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
    -- TODO
    -- After type checking is done, make sure that primitives have the correct type
    -- Currently, ints might still be doubles
    vars instantiateArg body' >>= checkValue ctx >>= types (resolve . getVar)

type Check s = ReaderT Sigs (ExceptT String (ST s))

type Sigs = (HashMap Hash Sig, Map FuncIX Sig)

runCheck :: Sigs -> (forall s. Check s a) -> Either String a
runCheck deps m = runST $ runExceptT $ runReaderT m deps

newtype TypeVar s = TypeVar (UF.Point s (Judgement s))

data Type' f
  = TVoid'
  | TBool'
  | TInt'
  | TDouble'
  | TKnownTuple' (Seq f)
  | TTuple' (IntMap f) -- TODO there probably is a better way to represent constraints, maybe somehow combine with OneOf
  deriving (Eq, Ord, Functor, Foldable, Traversable)

-- TODO there is probably some sort of better, lattice-y name (class?) for this
data Judgement s
  = Any
  | OneOf (Set2 (Type' Void)) -- TODO ambiguities are kind of primitive atm
  | Exactly (Type' (TypeVar s))

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

toList2 :: Set2 a -> NonEmpty a
toList2 (Set2 as) = NE.fromList $ S.toList as

unify :: TypeVar s -> TypeVar s -> Check s ()
unify (TypeVar a) (TypeVar b) = do
  ja <- liftST $ UF.descriptor a
  jb <- liftST $ UF.descriptor b
  jc <- ja <=> jb
  liftST $ UF.union a b >> UF.setDescriptor a jc
  where
    (<+>) :: Type' (TypeVar s) -> Type' (TypeVar s) -> Check s (Type' (TypeVar s))
    TVoid' <+> TVoid' = pure TVoid'
    TBool' <+> TBool' = pure TBool'
    TInt' <+> TInt' = pure TInt'
    TDouble' <+> TDouble' = pure TDouble'
    TKnownTuple' as <+> TKnownTuple' bs = do
      when (Seq.length as /= Seq.length bs) $ throwError "tuple length mismatch"
      TKnownTuple' <$> sequence (Seq.zipWith (\a b -> a <$ unify a b) as bs)
    TKnownTuple' as <+> TTuple' bs = TKnownTuple' as <$ checkTuple as bs
    TTuple' as <+> TKnownTuple' bs = TKnownTuple' bs <$ checkTuple bs as
    TTuple' as <+> TTuple' bs = TTuple' <$> unionWithM (\a b -> a <$ unify a b) as bs
    a <+> b = unificationErr (pure a) (pure b)

    unionWithM :: Monad m => (a -> a -> m a) -> IntMap a -> IntMap a -> m (IntMap a)
    unionWithM f a b = sequence $ IM.unionWith (\ma mb -> join $ liftM2 f ma mb) (pure <$> a) (pure <$> b)

    checkTuple :: Seq (TypeVar s) -> IntMap (TypeVar s) -> Check s ()
    checkTuple sq im = void $
      flip IM.traverseWithKey im $ \ix var -> case Seq.lookup ix sq of
        Nothing -> throwError $ "Index " <> show ix <> " out of bounds for tuple"
        Just var' -> unify var var'

    unificationErr :: NonEmpty (Type' ta) -> NonEmpty (Type' tb) -> Check s a
    unificationErr as bs = throwError $ "Cannot match " <> describeTypes as <> " with " <> describeTypes bs
      where
        describeTypes (a :| []) = describeType a
        describeTypes as = "one of (" <> unwords (describeType <$> toList as) <> ")"
        describeType :: Type' a -> String
        describeType TVoid' = "void"
        describeType TBool' = "bool"
        describeType TInt' = "int"
        describeType TDouble' = "double"
        describeType (TTuple' _) = "tuple"
        describeType (TKnownTuple' _) = "tuple"

    (<=>) :: Judgement s -> Judgement s -> Check s (Judgement s)
    Any <=> t = pure t
    OneOf t <=> Any = pure (OneOf t)
    OneOf a <=> OneOf b = intersection2 (pure . OneOf) (pure . Exactly . fmap absurd) (unificationErr (toList2 a) (toList2 b)) a b
    OneOf ts <=> Exactly t
      | Just t' <- closedOver traverse t, member2 t' ts = pure (Exactly t)
      | otherwise = unificationErr (toList2 ts) (pure t)
    Exactly t <=> Any = pure (Exactly t)
    Exactly t <=> OneOf ts
      | Just t' <- closedOver traverse t, member2 t' ts = pure (Exactly t)
      | otherwise = unificationErr (pure t) (toList2 ts)
    Exactly a <=> Exactly b = Exactly <$> (a <+> b)

liftST :: ST s a -> Check s a
liftST = lift . lift

fresh :: Judgement s -> Check s (TypeVar s)
fresh j = liftST $ TypeVar <$> UF.fresh j

fromType :: Type -> Check s (TypeVar s)
fromType TInt = fresh $ Exactly TInt'
fromType TDouble = fresh $ Exactly TDouble'
fromType TBool = fresh $ Exactly TBool'
fromType TVoid = fresh $ Exactly TVoid'
fromType (TTuple ts) = traverse fromType ts >>= fresh . Exactly . TKnownTuple'

resolve :: TypeVar s -> Check s Type
resolve (TypeVar v) =
  liftST (UF.descriptor v) >>= \case
    Any -> pure TVoid
    (OneOf ts) -> minimum <$> traverse resolveType (fmap absurd <$> toList2 ts)
    (Exactly t) -> resolveType t
  where
    resolveType :: Type' (TypeVar s) -> Check s Type
    resolveType TVoid' = pure TVoid
    resolveType TDouble' = pure TDouble
    resolveType TInt' = pure TInt
    resolveType TBool' = pure TBool
    resolveType (TKnownTuple' s) = TTuple <$> traverse resolve s
    resolveType (TTuple' _) = throwError "Ambiguous tuple type"

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

checkPrim :: Prim -> Check s (TypeVar s)
checkPrim (PInt _) = fresh (OneOf (set2 TInt' TDouble'))
checkPrim (PDouble _) = fromType TDouble
checkPrim (PBool _) = fromType TBool
checkPrim PVoid = fromType TVoid

-- checkLit (RTTuple t) = traverse checkLit t >>= fresh . Exactly . TTuple

-- TODO like other placees, this should be renamed to something more expr-y
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
  fromType TBool >>= unify ctx
  var <- fresh Any
  l' <- checkValue var l
  r' <- checkValue var r
  pure $ RTComp op l' r' (Typed a ctx)
checkValue ctx (RTPrim prim a) = do
  checkPrim prim >>= unify ctx
  pure $ RTPrim prim (Typed a ctx)
checkValue ctx (RTTuple tup a) = do
  (types, exprs) <- fmap Seq.unzip $
    forM tup $ \expr -> do
      var <- fresh Any
      expr' <- checkValue var expr
      pure (var, expr')
  fresh (Exactly (TKnownTuple' types)) >>= unify ctx
  pure $ RTTuple exprs (Typed a ctx)
checkValue ctx (RTCond c t f a) = do
  vc <- fromType TBool
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
  RTPlace a (Typed s var) (Typed s blk) (Either FuncIX Hash) ->
  Check s (RTPlace (Typed s a) var blk (Either FuncIX Hash))
checkPlace ctx (Place (Typed var t) a) = do
  unify ctx t
  pure $ Place var (Typed a ctx)
checkPlace ctx (RTSel haystack needle a) = do
  var <- fresh (Exactly $ TTuple' (IM.singleton needle ctx))
  haystack' <- checkPlace var haystack
  pure $ RTSel haystack' needle (Typed a ctx)
checkPlace _ (Deref _ _) = throwError "Deref type checking not yet implemented"

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
