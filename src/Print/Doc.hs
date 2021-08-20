{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Print.Doc where

import Control.Applicative
import Control.Monad.State
import Data.Bifoldable
import Data.Bifunctor
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BSB
import Data.Foldable
import Data.Functor.Identity
import Eval.Lib (extractVal, instantiate1Over, labels, vars)
import Eval.Types
import Expr hiding (Expr (..))
import Print.Bifree

newtype FreshT m a = FreshT {unNameT :: StateT Int m a}
  deriving newtype (Functor, Applicative, Monad)

type Fresh = FreshT Identity

type Symbol = Builder

runNameT :: Monad m => FreshT m a -> m a
runNameT (FreshT m) = evalStateT m 0

data DocF stm doc
  = Parens doc
  | Symbol Symbol
  | Prim Prim
  | List [doc]
  | Deref' doc
  | Operator Symbol doc doc
  | Sel doc doc
  | Call' Symbol [doc]
  | Prog Symbol stm
  | Cond doc doc doc
  deriving (Functor, Foldable, Traversable)

instance Bifunctor DocF where
  bimap _ r (Parens doc) = Parens (r doc)
  bimap _ _ (Symbol sym) = Symbol sym
  bimap _ _ (Prim prim) = Prim prim
  bimap _ r (List docs) = List (r <$> docs)
  bimap _ r (Deref' doc) = Deref' (r doc)
  bimap _ r (Operator sym lhs rhs) = Operator sym (r lhs) (r rhs)
  bimap _ r (Sel h n) = Sel (r h) (r n)
  bimap _ r (Call' sym args) = Call' sym (r <$> args)
  bimap _ r (Cond c t f) = Cond (r c) (r t) (r f)
  bimap l _ (Prog sym blk) = Prog sym (l blk)

instance Bifoldable DocF where
  bifoldr _ r a (Parens doc) = r doc a
  bifoldr _ _ a (Symbol _) = a
  bifoldr _ _ a (Prim _) = a
  bifoldr _ r a (List docs) = foldr r a docs
  bifoldr _ r a (Deref' doc) = r doc a
  bifoldr _ r a (Operator _ lhs rhs) = r lhs (r rhs a)
  bifoldr _ r a (Sel h n) = r h (r n a)
  bifoldr _ r a (Call' _ args) = foldr r a args
  bifoldr _ r a (Cond c t f) = r c (r t (r f a))
  bifoldr l _ a (Prog _ blk) = l blk a

-- TODO StatementDocF? SDocF? ProgF? PDoc?
data StatementF doc stm
  = SDecl Symbol doc doc stm
  | SAssign doc doc stm
  | SBreak Symbol doc
  | SContinue Symbol
  | SExpr doc (Maybe stm)
  deriving (Functor, Foldable, Traversable)

instance Bifunctor StatementF where
  bimap l r (SDecl sym lhs rhs k) = SDecl sym (l lhs) (l rhs) (r k)
  bimap l r (SAssign lhs rhs k) = SAssign (l lhs) (l rhs) (r k)
  bimap l _ (SBreak sym expr) = SBreak sym (l expr)
  bimap _ _ (SContinue sym) = SContinue sym
  bimap l r (SExpr expr mk) = SExpr (l expr) (r <$> mk)

instance Bifoldable StatementF where
  bifoldr l r a (SDecl _ lhs rhs k) = l lhs (l rhs (r k a))
  bifoldr l r a (SAssign lhs rhs k) = l lhs (l rhs (r k a))
  bifoldr l _ a (SBreak _ expr) = l expr a
  bifoldr _ _ a (SContinue _) = a
  bifoldr l r a (SExpr expr mk) = l expr (foldr r a mk)

type Statement = Bifix DocF StatementF

type Doc = Bifix StatementF DocF

parens :: Bool -> Doc -> Doc
parens False d = d
parens True d = Bifix $ Parens d

arithPrec :: ArithOp -> Int
arithPrec Add = 3
arithPrec Sub = 3
arithPrec Mul = 4
arithPrec Div = 4

pArithOp :: ArithOp -> Symbol
pArithOp Add = "+"
pArithOp Div = "/"
pArithOp Mul = "*"
pArithOp Sub = "-"

compPrec :: CompOp -> Int
compPrec Eq = 1
compPrec Neq = 1
compPrec _ = 2

fresh :: Monad m => FreshT m Int
fresh = FreshT $ state (\n -> (n, n + 1))

freshBlk :: Monad m => FreshT m Symbol
freshBlk = (\i -> "lbl_" <> BSB.string7 (show i)) <$> fresh

freshVar :: Monad m => FreshT m Symbol
freshVar = (\i -> "var_" <> BSB.string7 (show i)) <$> fresh

pCompOp :: CompOp -> Builder
pCompOp Eq = "=="
pCompOp Neq = "!="
pCompOp Lt = "<"
pCompOp Gt = ">"
pCompOp Leq = "<="
pCompOp Geq = ">="

operator :: Applicative m => (op -> Builder) -> (op -> Int) -> (Int -> a -> m Doc) -> op -> a -> a -> Int -> m Doc
operator pOp opPrec pRec op l r precCtx =
  liftA2
    (\l' r' -> parens (prec < precCtx) $ Bifix $ Operator (pOp op) l' r')
    (pRec prec l)
    (pRec prec r)
  where
    prec = opPrec op

pType :: Type -> Doc
pType TVoid = Bifix $ Symbol "void"
pType TBool = Bifix $ Symbol "bool"
pType TInt = Bifix $ Symbol "int"
pType TDouble = Bifix $ Symbol "double"
pType (TTuple ts) = Bifix $ List $ pType <$> toList ts

pProg :: RTProg Symbol Symbol Symbol Type -> Fresh Statement
pProg (Decl _ val k) = do
  val' <- pValue 0 val
  var <- freshVar
  k' <- pProg $ instantiate1Over vars var k
  let typ = pType $ extractVal val
  pure $ Bifix $ SDecl var typ val' k'
pProg (Assign plc val k) = Bifix <$> liftA3 SAssign (pPlace 0 plc) (pValue 0 val) (pProg k)
pProg (Break lbl val) = Bifix . SBreak lbl <$> pValue 0 val
pProg (Continue lbl) = pure $ Bifix $ SContinue lbl
pProg (ExprStmt expr mk) = Bifix <$> liftA2 SExpr (pValue 0 expr) (traverse pProg mk)

pValue :: Int -> RTValue Symbol Symbol Symbol Type -> Fresh Doc
pValue prec (RTArith op a b _) = operator pArithOp arithPrec pValue op a b prec
pValue prec (RTComp op a b _) = operator pCompOp compPrec pValue op a b prec
pValue prec (ValueSel h n _) = parens (prec > 8) . (\h' -> Bifix . Sel h' . Bifix . Prim $ PInt n) <$> pValue 8 h
pValue prec (RTCond c t f _) = parens (prec > 0) . Bifix <$> liftA3 Cond (pValue 0 c) (pValue 0 t) (pValue 0 f)
pValue _ (RTTuple tup _) = Bifix . List <$> traverse (pValue 0) (toList tup)
pValue _ (RTPrim prim _) = pure $ Bifix (Prim prim)
pValue _ (Block blk _) = do
  lbl <- freshBlk
  blk' <- pProg $ instantiate1Over labels lbl blk
  pure $ Bifix $ Prog lbl blk'
pValue _ (Call f args _) = Bifix . Call' f <$> traverse (pValue 0) args
pValue prec (PlaceVal pl _) = pPlace prec pl

pPlace :: Int -> RTPlace Symbol Symbol Symbol Type -> Fresh Doc
pPlace _ (Place sym _) = pure . Bifix $ Symbol sym
pPlace _ (PlaceSel h n _) = (\h' -> Bifix $ Sel h' $ Bifix $ Prim $ PInt n) <$> pPlace 9 h
pPlace prec (Deref val _) = parens (prec > 9) . Bifix . Deref' <$> pValue 9 val
