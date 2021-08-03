{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Eval.Types where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Hashable
import Data.Hashable.Lifted
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as M
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Void
import Expr
import GHC.Generics
import Lens.Micro.Platform (makeLenses)

-- VRTPlace and VLbl can never escape the scope in which they're defined, because they can only occur after a definer in a runtime expression, and so the surrounding context must evaluate to a runtime expression.
-- However, this definition makes it seem like we can have free-floating vars/lbls, similar to how a normal value can escape its lexical scope through closures.
-- Maybe there is some type-level way to make clearer that that is not possible in this case.
-- Maybe they exist as a different binding in a different transformer, it's hard to really call them thunks in the first place
-- In any case, it needs to be stressed that for these, their dynamc scope is their lexical scope.
-- Another clue is that variables should never be printable as values, which makes any handling outside the evaluator somewhat awkwars.
data Value f
  = VClosure (Thunk -> EvalBase WHNF) -- TODO params
  | VRTValue Deps (RTValue () VarIX BlockIX (Either FuncIX Hash))
  | VRTPlace Deps (RTPlace () VarIX BlockIX (Either FuncIX Hash))
  | VFunc Deps (Either FuncIX Hash)
  | VType Type
  | VPrim Prim
  | VString ByteString
  | VBlk BlockIX
  | VAttr (Map Name f)
  | VList (Seq f)
  deriving (Functor, Foldable, Traversable)

type Sig = ([Type], Type)

type WHNF = Value Thunk

newtype NF = NF {unNF :: Value NF}

newtype Hash = Hash {unHash :: Int}
  deriving newtype (Eq, Show, Ord, Hashable)

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

data NamelessCG = NamelessCG

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
-- Since this _only_ describes the implementation, structs with the same
-- types of fields but different names shouldn't count as different values.
-- Ideally we'd find some way to do name-based access, but erase the names at
-- runtime
data Type
  = TInt
  | TDouble
  | TBool
  | TVoid
  | TStruct (Map Name Type)
  deriving stock (Eq, Show, Ord, Generic)

instance Hashable Type where
  hashWithSalt s TInt = hashWithSalt s (0 :: Int)
  hashWithSalt s TDouble = hashWithSalt s (1 :: Int)
  hashWithSalt s TBool = hashWithSalt s (2 :: Int)
  hashWithSalt s TVoid = hashWithSalt s (3 :: Int)
  hashWithSalt s (TStruct m) = hashWithSalt s (4 :: Int, M.toList m)

data Bind b a
  = Bound b
  | Free a
  deriving stock (Eq, Show, Ord, Functor, Foldable, Traversable, Generic, Generic1)
  deriving anyclass (Hashable, Hashable1)

-- TODO The (Maybe Type) field in Decl doesn't make sense after type checking, maybe make it
-- variable that becomes () after TC
data RTProg typ var blk fun
  = Decl (Maybe Type) (RTValue typ var blk fun) (RTProg typ (Bind () var) blk fun)
  | Assign (RTPlace typ var blk fun) (RTValue typ var blk fun) (RTProg typ var blk fun)
  | Break blk (RTValue typ var blk fun)
  | Continue blk
  | ExprStmt (RTValue typ var blk fun) (Maybe (RTProg typ var blk fun))
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving anyclass (Hashable, Hashable1)

-- TODO Name is misleading. Makes sense in contrast to place, but is an epxression more than a value
data RTValue typ var blk fun
  = RTArith ArithOp (RTValue typ var blk fun) (RTValue typ var blk fun) typ
  | RTComp CompOp (RTValue typ var blk fun) (RTValue typ var blk fun) typ
  | RTPrim Prim typ
  | RTCond (RTValue typ var blk fun) (RTValue typ var blk fun) (RTValue typ var blk fun) typ
  | Call fun [RTValue typ var blk fun] typ
  | PlaceVal (RTPlace typ var blk fun) typ
  | Block (RTProg typ var (Bind () blk) fun) typ
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving anyclass (Hashable, Hashable1)

data RTPlace typ var blk fun
  = Place var typ
  | Deref (RTValue typ var blk fun) typ
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving anyclass (Hashable, Hashable1)

data RTFunc fun = RTFunc
  { fnArgs :: [Type],
    fnRet :: Type,
    fnBody :: RTValue Type (Bind Int Void) Void fun
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving anyclass (Hashable, Hashable1)

newtype EvalBase a = EvalBase {unEvalBase :: ReaderT (Map FuncIX Sig) (StateT Int (ExceptT String IO)) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFix, MonadError String, MonadFresh, MonadReader (Map FuncIX Sig))

type Eval = ReaderT EvalEnv EvalBase

type MonadFresh = MonadState Int -- TODO proper class

unEval :: Eval a -> EvalBase a
unEval = flip runReaderT env0
  where
    env0 = EvalEnv mempty

type Comp = WriterT Deps Eval

newtype EvalEnv = EvalEnv {_binds :: Map Name Thunk} -- TODO Just `type`?

makeLenses ''EvalEnv
makeLenses ''Deps

class RTAST f where
  traverseAst ::
    Applicative m =>
    (typ -> m typ') ->
    (var -> m var') ->
    (lbl -> m lbl') ->
    (fun -> m fun') ->
    (f typ var lbl fun -> m (f typ' var' lbl' fun'))

instance RTAST RTValue where
  traverseAst ft fv fl ff = go
    where
      go (RTArith op l r t) = RTArith op <$> go l <*> go r <*> ft t
      go (RTComp op l r t) = RTComp op <$> go l <*> go r <*> ft t
      go (Call fun args t) = Call <$> ff fun <*> traverse go args <*> ft t
      go (PlaceVal plc t) = PlaceVal <$> traverseAst ft fv fl ff plc <*> ft t
      go (Block blk t) = Block <$> traverseAst ft fv (traverse fl) ff blk <*> ft t
      go (RTCond cond true false t) = RTCond <$> go cond <*> go true <*> go false <*> ft t
      go (RTPrim p t) = RTPrim p <$> ft t

instance RTAST RTPlace where
  traverseAst ft fv _ _ (Place var t) = Place <$> fv var <*> ft t
  traverseAst ft fv fl ff (Deref val t) = Deref <$> traverseAst ft fv fl ff val <*> ft t

instance RTAST RTProg where
  traverseAst ft fv fl ff (Decl typ val k) = Decl typ <$> traverseAst ft fv fl ff val <*> traverseAst ft (traverse fv) fl ff k
  traverseAst ft fv fl ff (Assign lhs rhs k) = Assign <$> traverseAst ft fv fl ff lhs <*> traverseAst ft fv fl ff rhs <*> traverseAst ft fv fl ff k
  traverseAst ft fv fl ff (Break lbl val) = Break <$> fl lbl <*> traverseAst ft fv fl ff val
  traverseAst ft fv fl ff (ExprStmt val k) = ExprStmt <$> traverseAst ft fv fl ff val <*> traverse (traverseAst ft fv fl ff) k
  traverseAst _ _ fl _ (Continue lbl) = Continue <$> fl lbl
