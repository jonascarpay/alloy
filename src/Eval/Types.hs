{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Eval.Types where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import Data.Hashable
import Data.Hashable.Lifted
import Data.IORef
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Text (Text)
import Data.Void
import Expr
import GHC.Generics
import Lens.Micro.Platform (makeLenses)
import Rebound

-- TODO
-- Compile-time numbers should probably be represented as Scientific
-- Func should really be called procedure

-- VRTPlace and VLbl can never escape the scope in which they're defined, because they can only occur after a definer in a runtime expression, and so the surrounding context must evaluate to a runtime expression.
-- However, this definition makes it seem like we can have free-floating vars/lbls, similar to how a normal value can escape its lexical scope through closures.
-- Maybe there is some type-level way to make clearer that that is not possible in this case.
-- Maybe they exist as a different binding in a different transformer, it's hard to really call them thunks in the first place
-- In any case, it needs to be stressed that for these, their dynamc scope is their lexical scope.
-- Another clue is that variables should never be printable as values, which makes any handling outside the evaluator somewhat awkwars.
--
-- TODO
-- Our Strings shouldn't be Text.
-- Indexing, length, concat is all O(n).
-- Maybe just use Seq Char?
data Value f
  = VClosure (Thunk -> EvalBase WHNF) -- TODO params
  | VRTValue Deps (EvalPhase RTValue)
  | VRTPlace Deps (EvalPhase RTPlace)
  | VFunc Deps (Either FuncIX Hash)
  | VType Type
  | VPrim Prim
  | VString Text
  | VBlk BlockIX
  | VAttr (Map Symbol f)
  | VList (Seq f)
  deriving (Functor, Foldable, Traversable)

type Sig = ([Type], Type)

type WHNF = Value Thunk

newtype NF = NF {unNF :: Value NF}

newtype Hash = Hash {unHash :: Int}
  deriving newtype (Eq, Show, Ord, Hashable)

newtype Name = Name Symbol
  deriving newtype (Show)

instance Eq Name where _ == _ = True

instance Ord Name where compare _ _ = EQ

instance Hashable Name where hashWithSalt s _ = s

-- Function calls form a sort of directed bigraph in which
--   - there always is an edge between a parent and its direct children
--   - edges can only go to direct ancestors/descendants (not cousins)
-- The tree structure represents the static scope of functions, the edges are calls.
-- TODO
-- At some point a nameless representation might be nice
data CallGraph = CallGraph
  { cgFunc :: FuncIX,
    cgBody :: RTFunc (Either FuncIX Hash),
    cgOpen :: Set FuncIX,
    cgBind :: Set FuncIX,
    cgDeps :: Set CallGraph
  }
  deriving stock (Show)

instance Eq CallGraph where a == b = cgFunc a == cgFunc b

instance Ord CallGraph where a `compare` b = cgFunc a `compare` cgFunc b

data Deps = Deps
  { _closedFuncs :: HashMap Hash (RTFunc Hash),
    _openFuncs :: Set CallGraph
  }

instance Semigroup Deps where
  Deps ca oa <> Deps cb ob = Deps (ca <> cb) (oa <> ob)

instance Monoid Deps where mempty = Deps mempty mempty

newtype Thunk = Thunk (IORef (Either (EvalBase WHNF) WHNF))

-- These are not allowed to be hashable.
-- This way, we make sure that only completely nameless terms get hashed
-- TODO These are no longer IX's, but rather IDs
newtype VarIX = VarIX Int
  deriving newtype (Eq, Show, Enum)

newtype BlockIX = BlockIX Int
  deriving newtype (Eq, Show, Enum)

newtype FuncIX = FuncIX {unFuncIX :: Int}
  deriving newtype (Eq, Ord, Show, Enum)

-- TODO
-- The ord instance determines the order of defaulting in the case of ambigous types. Not ideal.
data TypeF f
  = TVoid -- TODO Can this be the empty tuple now?
  | TBool
  | TInt
  | TDouble
  | TPtr f
  | TTuple (Seq f)
  | TArray Int f
  deriving stock (Eq, Show, Ord, Generic, Functor, Foldable, Traversable)

-- TODO unify fixpoints? Newtype deriving is nice tho
newtype Type = Type (TypeF Type) deriving newtype (Show, Eq, Ord, Hashable)

instance Hashable f => Hashable (TypeF f) where
  hashWithSalt s TInt = hashWithSalt s (0 :: Int)
  hashWithSalt s TDouble = hashWithSalt s (1 :: Int)
  hashWithSalt s TBool = hashWithSalt s (2 :: Int)
  hashWithSalt s TVoid = hashWithSalt s (3 :: Int)
  hashWithSalt s (TTuple m) = hashWithSalt s (4 :: Int, toList m)
  hashWithSalt s (TPtr t) = hashWithSalt s (5 :: Int, t)
  hashWithSalt s (TArray n t) = hashWithSalt s (6 :: Int, n, t)

instance Hashable a => Hashable (Seq a) where hashWithSalt salt as = hashWithSalt salt (toList as)

instance Hashable1 Seq where liftHashWithSalt f salt as = liftHashWithSalt f salt (toList as)

-- TODO The RT* data types don't gain anything by being traversable

-- TODO The (Maybe Type) field in Decl doesn't make sense after type checking, maybe make it
-- variable that becomes () after TC. This should also help with printing
data RTProg var blk fun lit info
  = Decl Name (Maybe Type) (RTValue var blk fun lit info) (RTProg (Bind () var) blk fun lit info)
  | Assign (RTPlace var blk fun lit info) (RTValue var blk fun lit info) (RTProg var blk fun lit info)
  | Break blk (RTValue var blk fun lit info)
  | Continue blk
  | ExprStmt (RTValue var blk fun lit info) (Maybe (RTProg var blk fun lit info))
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving anyclass (Hashable, Hashable1)

-- TODO Name is misleading. Makes sense in contrast to place, but is an epxression more than a value
data RTValue var blk fun lit info
  = RTArith ArithOp (RTValue var blk fun lit info) (RTValue var blk fun lit info) info
  | RTComp CompOp (RTValue var blk fun lit info) (RTValue var blk fun lit info) info
  | RTLit lit info
  | ValueSel (RTValue var blk fun lit info) Int info
  | RTTuple (Seq (RTValue var blk fun lit info)) info
  | RTCond (RTValue var blk fun lit info) (RTValue var blk fun lit info) (RTValue var blk fun lit info) info
  | Call fun [RTValue var blk fun lit info] info
  | PlaceVal (RTPlace var blk fun lit info) info
  | Block (RTProg var (Bind () blk) fun lit info) info
  | RTRef (RTPlace var blk fun lit info) info
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving anyclass (Hashable, Hashable1)

-- TODO
-- DynamicSel, where the index is not statically known, for uniform tuple types?
-- TODO
-- Sel also exists for non-lvalues, in which case it is not assignable.
-- Weirdly, C doesn't care, this is valid:
--   ((struct { int x; }){.x = 1}).x = 3;
data RTPlace var blk fun lit info
  = Place var info -- TODO Rename
  | PlaceSel (RTPlace var blk fun lit info) Int info
  | RTDeref (RTValue var blk fun lit info) info
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving anyclass (Hashable, Hashable1)

type EvalPhase ast = ast VarIX BlockIX (Either FuncIX Hash) Prim ()

data RTFunc fun = RTFunc
  { fnArgs :: [(Name, Type)],
    fnRet :: Type,
    fnBody :: RTValue (Bind Int Void) Void fun Prim Type
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

newtype EvalBase a = EvalBase {unEvalBase :: ReaderT (Map FuncIX Sig) (StateT Int (ExceptT String IO)) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFix, MonadError String, MonadFresh, MonadReader (Map FuncIX Sig))

type Eval = ReaderT EvalEnv EvalBase

type MonadFresh = MonadState Int -- TODO proper class

unEval :: Eval a -> EvalBase a
unEval = flip runReaderT env0
  where
    env0 = EvalEnv mempty

type Comp = WriterT Deps Eval

newtype EvalEnv = EvalEnv {_binds :: Map Symbol Thunk} -- TODO Just `type`?

-- TODO Maybe all things get an `inf` argument
class RTAST f where
  traverseAst ::
    Applicative m =>
    (var -> m var') ->
    (blk -> m blk') ->
    (fun -> m fun') ->
    (inf -> lit -> m lit') ->
    (inf -> m inf') ->
    (f var blk fun lit inf -> m (f var' blk' fun' lit' inf'))

instance RTAST RTValue where
  {-# INLINE traverseAst #-}
  traverseAst fv fb ff fl fi = go
    where
      go (RTArith op l r t) = RTArith op <$> go l <*> go r <*> fi t
      go (RTComp op l r t) = RTComp op <$> go l <*> go r <*> fi t
      go (Call fun args t) = Call <$> ff fun <*> traverse go args <*> fi t
      go (PlaceVal plc t) = PlaceVal <$> traverseAst fv fb ff fl fi plc <*> fi t
      go (ValueSel h n t) = ValueSel <$> go h <*> pure n <*> fi t
      go (Block blk t) = Block <$> traverseAst fv (traverse fb) ff fl fi blk <*> fi t
      go (RTCond cond true false t) = RTCond <$> go cond <*> go true <*> go false <*> fi t
      go (RTTuple tup t) = RTTuple <$> traverse go tup <*> fi t
      go (RTLit lit t) = RTLit <$> fl t lit <*> fi t
      go (RTRef r i) = RTRef <$> traverseAst fv fb ff fl fi r <*> fi i

instance RTAST RTPlace where
  {-# INLINE traverseAst #-}
  traverseAst fv fb ff fl fi = go
    where
      go (Place var t) = Place <$> fv var <*> fi t
      go (PlaceSel h n t) = PlaceSel <$> go h <*> pure n <*> fi t
      go (RTDeref val t) = RTDeref <$> traverseAst fv fb ff fl fi val <*> fi t

instance RTAST RTProg where
  {-# INLINE traverseAst #-}
  traverseAst fv fb ff fl fi (Decl name typ val k) = Decl name typ <$> traverseAst fv fb ff fl fi val <*> traverseAst (traverse fv) fb ff fl fi k
  traverseAst fv fb ff fl fi (Assign lhs rhs k) = Assign <$> traverseAst fv fb ff fl fi lhs <*> traverseAst fv fb ff fl fi rhs <*> traverseAst fv fb ff fl fi k
  traverseAst fv fb ff fl fi (Break blk val) = Break <$> fb blk <*> traverseAst fv fb ff fl fi val
  traverseAst fv fb ff fl fi (ExprStmt val k) = ExprStmt <$> traverseAst fv fb ff fl fi val <*> traverse (traverseAst fv fb ff fl fi) k
  traverseAst _ fb _ _ _ (Continue blk) = Continue <$> fb blk

makeLenses ''EvalEnv
makeLenses ''Deps
