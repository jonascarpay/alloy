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

type RTBlock call typ = Block typ (RTExpr call typ typ)

data RTLiteral
  = RTInt Int
  | RTDouble Double
  | RTStruct (Map Name RTLiteral)
  deriving (Eq, Show, Generic)

instance Hashable RTLiteral

data RTExpr call typ a
  = RTVar Name a
  | RTLiteral RTLiteral a
  | RTArith ArithOp (RTExpr call typ a) (RTExpr call typ a) a
  | RTBlock (Block typ (RTExpr call typ a)) a
  | RTCall call [RTExpr call typ a] a
  deriving (Eq, Show, Generic)

rtExprCalls :: Traversal (RTExpr call typ a) (RTExpr call' typ a) call call'
rtExprCalls f = rtExprMasterTraversal f pure pure pure pure

rtExprTypes :: Traversal (RTExpr call typ a) (RTExpr call typ' a) typ typ'
rtExprTypes f = rtExprMasterTraversal pure f pure pure pure

stmtExpr :: Traversal (Stmt typ expr) (Stmt typ expr') expr expr'
stmtExpr f = stmtMasterTraversal f pure pure

{-# INLINE rtExprMasterTraversal #-}
rtExprMasterTraversal ::
  Applicative f =>
  (call -> f call') ->
  (typ -> f typ') ->
  (info -> f info') ->
  (Name -> f Name) ->
  (RTLiteral -> f RTLiteral) ->
  (RTExpr call typ info -> f (RTExpr call' typ' info'))
rtExprMasterTraversal fCall fTyp fInfo fName fLit = go
  where
    go (RTVar nm i) = RTVar <$> fName nm <*> fInfo i
    go (RTLiteral lit i) = RTLiteral <$> fLit lit <*> fInfo i
    go (RTArith op l r i) = RTArith op <$> go l <*> go r <*> fInfo i
    go (RTBlock (Block lbl blk) i) = RTBlock <$> (Block <$> traverse fName lbl <*> traverse (stmtMasterTraversal go fTyp fName) blk) <*> fInfo i
    go (RTCall cl args i) = RTCall <$> fCall cl <*> traverse go args <*> fInfo i

{-# INLINE stmtMasterTraversal #-}
stmtMasterTraversal ::
  Applicative f =>
  (expr -> f expr') ->
  (typ -> f typ') ->
  (Name -> f Name) ->
  (Stmt typ expr -> f (Stmt typ' expr'))
stmtMasterTraversal fExpr fTyp fName = go
  where
    go (Return expr) = Return <$> fExpr expr
    go (Break mname mexpr) = Break <$> traverse fName mname <*> traverse fExpr mexpr
    go (Continue mname) = Continue <$> traverse fName mname
    go (Decl nm typ expr) = Decl <$> fName nm <*> fTyp typ <*> fExpr expr
    go (Assign nm expr) = Assign <$> fName nm <*> fExpr expr
    go (ExprStmt expr) = ExprStmt <$> fExpr expr

instance (Hashable typ, Hashable info, Hashable call) => Hashable (RTExpr call typ info)

rtInfo :: RTExpr call typ a -> a
rtInfo (RTVar _ a) = a
rtInfo (RTArith _ _ _ a) = a
rtInfo (RTBlock _ a) = a
rtInfo (RTCall _ _ a) = a
rtInfo (RTLiteral _ a) = a

newtype GUID = GUID {unGUID :: Int}
  deriving (Eq, Show, Ord, Hashable)

type TempID = Int

data Dependencies = Dependencies
  { _depKnownFuncs :: Map GUID (FunDef GUID),
    _depTempFuncs :: Map TempID TempFunc
  }
  deriving (Eq, Show, Generic)

instance Hashable Dependencies

data TempFunc = TempFunc
  { _tempFunc :: FunDef PreCall,
    _tempFuncDeps :: Map TempID TempFunc
  }
  deriving (Eq, Show, Generic)

tempFuncs :: Traversal' TempFunc (FunDef PreCall)
tempFuncs f = go
  where
    go (TempFunc fn deps) = TempFunc <$> f fn <*> traverse go deps

instance Hashable TempFunc

type RecIndex = Int

data PreCall
  = CallRec RecIndex
  | CallKnown GUID
  | CallTemp TempID
  deriving (Eq, Show, Generic)

instance Hashable PreCall

precallRec :: Traversal' PreCall RecIndex
precallRec f (CallRec n) = CallRec <$> f n
precallRec _ c = pure c

precallTemp :: Traversal' PreCall TempID
precallTemp f (CallTemp t) = CallTemp <$> f t
precallTemp _ c = pure c

-- TODO make FunType datatype
data FunDef call = FunDef
  { _fnArgs :: [(Name, Type)],
    _fnRet :: Type,
    _fnName :: Name,
    _fnBody :: RTBlock call Type
  }
  deriving (Eq, Show, Generic)

instance Hashable call => Hashable (FunDef call) where
  hashWithSalt s (FunDef args ret _ body) = hashWithSalt s (args, ret, body)

instance Semigroup Dependencies where
  Dependencies a b <> Dependencies a' b' = Dependencies (a <> a') (b <> b')

instance Monoid Dependencies where
  mempty = Dependencies mempty mempty

makeLenses ''FunDef
makeLenses ''Dependencies
makeLenses ''TempFunc
