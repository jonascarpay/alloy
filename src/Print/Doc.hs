{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Print.Doc where

import Control.Applicative
import Control.Monad.State
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BSB
import Data.Foldable
import Data.Functor.Identity
import Data.Map (Map)
import Eval.Lib (extractVal, instantiate1Over, labels, vars)
import Eval.Types
import Expr hiding (Expr (..))

newtype FreshT m a = FreshT {unNameT :: StateT Int m a}
  deriving newtype (Functor, Applicative, Monad)

type Fresh = FreshT Identity

type Symbol = Builder

runNameT :: Monad m => FreshT m a -> m a
runNameT (FreshT m) = evalStateT m 0

data DocF f g
  = Parens f
  | Symbol Symbol
  | Prim Prim
  | List [f]
  | Attr (Map Name f)
  | Brackets f
  | Deref' f
  | Operator Symbol f f
  | Sel f f
  | Call' Symbol [f]
  | Prog Symbol g
  | Cond f f f

data StatementF f g
  = SDecl Symbol f f g
  | SAssign f f g
  | SBreak Symbol f
  | SContinue Symbol
  | SExpr f (Maybe g)

newtype Statement = Statement (StatementF Doc Statement)

newtype Doc = Doc (DocF Doc Statement)

parens :: Bool -> Doc -> Doc
parens False d = d
parens True d = Doc $ Parens d

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
    (\l' r' -> parens (prec < precCtx) $ Doc $ Operator (pOp op) l' r')
    (pRec prec l)
    (pRec prec r)
  where
    prec = opPrec op

pType :: Type -> Doc
pType TVoid = Doc $ Symbol "void"
pType TBool = Doc $ Symbol "bool"
pType TInt = Doc $ Symbol "int"
pType TDouble = Doc $ Symbol "double"
pType (TTuple ts) = Doc $ List $ pType <$> toList ts

pProg :: RTProg Symbol Symbol Symbol Type -> Fresh Statement
pProg (Decl _ val k) = do
  val' <- pValue 0 val
  var <- freshVar
  k' <- pProg $ instantiate1Over vars var k
  let typ = pType $ extractVal val
  pure $ Statement $ SDecl var typ val' k'
pProg (Assign plc val k) = Statement <$> liftA3 SAssign (pPlace 0 plc) (pValue 0 val) (pProg k)
pProg (Break lbl val) = Statement . SBreak lbl <$> pValue 0 val
pProg (Continue lbl) = pure $ Statement $ SContinue lbl
pProg (ExprStmt expr mk) = Statement <$> liftA2 SExpr (pValue 0 expr) (traverse pProg mk)

pValue :: Int -> RTValue Symbol Symbol Symbol Type -> Fresh Doc
pValue prec (RTArith op a b _) = operator pArithOp arithPrec pValue op a b prec
pValue prec (RTComp op a b _) = operator pCompOp compPrec pValue op a b prec
pValue prec (ValueSel h n _) = parens (prec > 8) . (\h' -> Doc . Sel h' . Doc . Prim $ PInt n) <$> pValue 8 h
pValue prec (RTCond c t f _) = parens (prec > 0) . Doc <$> liftA3 Cond (pValue 0 c) (pValue 0 t) (pValue 0 f)
pValue _ (RTTuple tup _) = Doc . List <$> traverse (pValue 0) (toList tup)
pValue _ (RTPrim prim _) = pure $ Doc (Prim prim)
pValue _ (Block blk _) = do
  lbl <- freshBlk
  blk' <- pProg $ instantiate1Over labels lbl blk
  pure $ Doc $ Prog lbl blk'
pValue _ (Call f args _) = Doc . Call' f <$> traverse (pValue 0) args
pValue prec (PlaceVal pl _) = pPlace prec pl

pPlace :: Int -> RTPlace Symbol Symbol Symbol Type -> Fresh Doc
pPlace _ (Place sym _) = pure . Doc $ Symbol sym
pPlace _ (PlaceSel h n _) = (\h' -> Doc $ Sel h' $ Doc $ Prim $ PInt n) <$> pPlace 9 h
pPlace prec (Deref val _) = parens (prec > 9) . Doc . Deref' <$> pValue 9 val
