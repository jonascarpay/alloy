{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Program where

import Data.Hashable
import Data.Map (Map)
import Expr
import GHC.Generics
import Lens.Micro.Platform
import Numeric (showHex)

type RTBlock var lbl call typ = Block var lbl typ (RTExpr var lbl call typ typ)

newtype VarID = VarID Int deriving (Eq, Show, Ord, Hashable)

newtype LabelID = LabelID Int deriving (Eq, Show, Ord, Hashable)

newtype TempFuncID = TempFuncID Int deriving (Eq, Show, Ord, Hashable)

data RTLiteral
  = RTInt Int
  | RTDouble Double
  | RTBool Bool
  deriving (Eq, Show, Generic)

instance Hashable RTLiteral

data RTExpr var lbl call typ a
  = RTVar var a
  | RTAccessor (RTExpr var lbl call typ a) Name a
  | RTStruct (Map Name (RTExpr var lbl call typ a)) a
  | RTLiteral RTLiteral a
  | RTBin BinOp (RTExpr var lbl call typ a) (RTExpr var lbl call typ a) a
  | RTBlock (Block var lbl typ (RTExpr var lbl call typ a)) a
  | RTCall call [RTExpr var lbl call typ a] a
  | RTCond (RTExpr var lbl call typ a) (RTExpr var lbl call typ a) (RTExpr var lbl call typ a) a
  deriving (Eq, Show, Generic)

rtExprCalls :: Traversal (RTExpr var lbl call typ a) (RTExpr var lbl call' typ a) call call'
rtExprCalls f = rtExprMasterTraversal pure pure f pure pure pure

rtExprTypes :: Traversal (RTExpr var lbl call typ a) (RTExpr var lbl call typ' a) typ typ'
rtExprTypes f = rtExprMasterTraversal pure pure pure f pure pure

stmtExpr :: Traversal (Stmt var lbl typ expr) (Stmt var lbl typ expr') expr expr'
stmtExpr = stmtMasterTraversal pure pure pure

blkVars :: Traversal (RTBlock var1 lbl call typ) (RTBlock var2 lbl call typ) var1 var2
blkVars f = blkStmts . traverse $ rtStmtMasterTraversal f pure pure pure pure pure

{-# INLINE rtExprMasterTraversal #-}
rtExprMasterTraversal ::
  Applicative f =>
  (var -> f var') ->
  (lbl -> f lbl') ->
  (call -> f call') ->
  (typ -> f typ') ->
  (info -> f info') ->
  (RTLiteral -> f RTLiteral) ->
  (RTExpr var lbl call typ info -> f (RTExpr var' lbl' call' typ' info'))
rtExprMasterTraversal fVar fLbl fCall fTyp fInfo fLit = go
  where
    go (RTVar nm i) = RTVar <$> fVar nm <*> fInfo i
    go (RTAccessor str field i) = RTAccessor <$> go str <*> pure field <*> fInfo i
    go (RTStruct str i) = RTStruct <$> traverse go str <*> fInfo i
    go (RTLiteral lit i) = RTLiteral <$> fLit lit <*> fInfo i
    go (RTBin op l r i) = RTBin op <$> go l <*> go r <*> fInfo i
    go (RTBlock (Block lbl blk typ) i) =
      RTBlock <$> (Block <$> traverse fLbl lbl <*> traverse (stmtMasterTraversal fVar fLbl fTyp go) blk <*> fTyp typ) <*> fInfo i
    go (RTCall cl args i) = RTCall <$> fCall cl <*> traverse go args <*> fInfo i
    go (RTCond cond tr fl i) = RTCond <$> go cond <*> go tr <*> go fl <*> fInfo i

{-# INLINE stmtMasterTraversal #-}
stmtMasterTraversal ::
  Applicative f =>
  (var -> f var') ->
  (lbl -> f lbl') ->
  (typ -> f typ') ->
  (expr -> f expr') ->
  (Stmt var lbl typ expr -> f (Stmt var' lbl' typ' expr'))
stmtMasterTraversal fVar fLbl fTyp fExpr = go
  where
    go (Return expr) = Return <$> fExpr expr
    go (Break mname mexpr) = Break <$> traverse fLbl mname <*> traverse fExpr mexpr
    go (Continue mname) = Continue <$> traverse fLbl mname
    go (Decl nm typ expr) = Decl <$> fVar nm <*> fTyp typ <*> fExpr expr
    go (Assign nm expr) = Assign <$> fVar nm <*> fExpr expr
    go (ExprStmt expr) = ExprStmt <$> fExpr expr

-- TODO combine with previous, there's probably some gains to be made and we only need this one I think
{-# INLINE rtStmtMasterTraversal #-}
rtStmtMasterTraversal ::
  Applicative f =>
  (var -> f var') ->
  (lbl -> f lbl') ->
  (call -> f call') ->
  (typ -> f typ') ->
  (info -> f info') ->
  (RTLiteral -> f RTLiteral) ->
  (Stmt var lbl typ (RTExpr var lbl call typ info) -> f (Stmt var' lbl' typ' (RTExpr var' lbl' call' typ' info')))
rtStmtMasterTraversal fVar fLbl fCall fTyp fInfo fLit = stmtMasterTraversal fVar fLbl fTyp (rtExprMasterTraversal fVar fLbl fCall fTyp fInfo fLit)

instance
  (Hashable typ, Hashable lbl, Hashable info, Hashable var, Hashable call) =>
  Hashable (RTExpr var lbl call typ info)

rtInfo :: RTExpr var lbl call typ a -> a
rtInfo (RTVar _ a) = a
rtInfo (RTAccessor _ _ a) = a
rtInfo (RTBin _ _ _ a) = a
rtInfo (RTBlock _ a) = a
rtInfo (RTCall _ _ a) = a
rtInfo (RTLiteral _ a) = a
rtInfo (RTCond _ _ _ a) = a
rtInfo (RTStruct _ a) = a

newtype GUID = GUID {unGUID :: Int}
  deriving (Eq, Ord, Hashable)

instance Show GUID where
  show (GUID hash) = showHex (fromIntegral hash :: Word) ""

data Slot
  = Argument Int
  | Local Int
  deriving (Eq, Show, Generic)

instance Hashable Slot

data Dependencies = Dependencies
  { _depKnownFuncs :: Map GUID (FunDef Slot LabelID GUID),
    _depTempFuncs :: Map TempFuncID TempFunc
  }
  deriving (Eq, Show, Generic)

instance Hashable Dependencies

data TempFunc = TempFunc
  { _tempFunc :: FunDef VarID LabelID PreCall,
    _tempFuncDeps :: Map TempFuncID TempFunc
  }
  deriving (Eq, Show, Generic)

tempFuncs :: Traversal' TempFunc (FunDef VarID LabelID PreCall)
tempFuncs f = go
  where
    go (TempFunc fn deps) = TempFunc <$> f fn <*> traverse go deps

instance Hashable TempFunc

type RecIndex = Int

data PreCall
  = CallRec RecIndex
  | CallKnown GUID
  | CallTemp TempFuncID
  deriving (Eq, Show, Generic)

instance Hashable PreCall

precallRec :: Traversal' PreCall RecIndex
precallRec f (CallRec n) = CallRec <$> f n
precallRec _ c = pure c

precallTemp :: Traversal' PreCall TempFuncID
precallTemp f (CallTemp t) = CallTemp <$> f t
precallTemp _ c = pure c

-- TODO make FunType datatype
data FunDef var lbl call = FunDef
  { _fnArgs :: [(var, Type)],
    _fnRet :: Type,
    _fnName :: Name,
    _fnBody :: RTBlock var lbl call Type
  }
  deriving (Eq, Show, Generic)

instance (Hashable var, Hashable lbl, Hashable call) => Hashable (FunDef var lbl call) where
  hashWithSalt s (FunDef args ret _ body) = hashWithSalt s (args, ret, body)

instance Semigroup Dependencies where
  Dependencies a b <> Dependencies a' b' = Dependencies (a <> a') (b <> b')

instance Monoid Dependencies where
  mempty = Dependencies mempty mempty

makeLenses ''FunDef
makeLenses ''Dependencies
makeLenses ''TempFunc

funCalls :: Traversal (FunDef var lbl call) (FunDef var lbl call') call call'
funCalls = fnBody . blkStmts . traverse . stmtExpr . rtExprCalls
