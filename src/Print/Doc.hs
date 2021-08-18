{-# LANGUAGE DeriveTraversable #-}
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

data DocF stm doc
  = Parens doc
  | Symbol Symbol
  | Prim Prim
  | List [doc]
  | Attr (Map Name doc)
  | Brackets doc
  | Deref' doc
  | Operator Symbol doc doc
  | Sel doc doc
  | Call' Symbol [doc]
  | Prog Symbol stm
  | Cond doc doc doc
  deriving (Functor, Foldable, Traversable)

data StatementF doc stm
  = SDecl Symbol doc doc stm
  | SAssign doc doc stm
  | SBreak Symbol doc
  | SContinue Symbol
  | SExpr doc (Maybe stm)
  deriving (Functor, Foldable, Traversable)

newtype Fix2 flip self = Fix2 (self (Fix2 self flip) (Fix2 flip self))

type Statement = Fix2 DocF StatementF

type Doc = Fix2 StatementF DocF

parens :: Bool -> Doc -> Doc
parens False d = d
parens True d = Fix2 $ Parens d

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
    (\l' r' -> parens (prec < precCtx) $ Fix2 $ Operator (pOp op) l' r')
    (pRec prec l)
    (pRec prec r)
  where
    prec = opPrec op

pType :: Type -> Doc
pType TVoid = Fix2 $ Symbol "void"
pType TBool = Fix2 $ Symbol "bool"
pType TInt = Fix2 $ Symbol "int"
pType TDouble = Fix2 $ Symbol "double"
pType (TTuple ts) = Fix2 $ List $ pType <$> toList ts

pProg :: RTProg Symbol Symbol Symbol Type -> Fresh Statement
pProg (Decl _ val k) = do
  val' <- pValue 0 val
  var <- freshVar
  k' <- pProg $ instantiate1Over vars var k
  let typ = pType $ extractVal val
  pure $ Fix2 $ SDecl var typ val' k'
pProg (Assign plc val k) = Fix2 <$> liftA3 SAssign (pPlace 0 plc) (pValue 0 val) (pProg k)
pProg (Break lbl val) = Fix2 . SBreak lbl <$> pValue 0 val
pProg (Continue lbl) = pure $ Fix2 $ SContinue lbl
pProg (ExprStmt expr mk) = Fix2 <$> liftA2 SExpr (pValue 0 expr) (traverse pProg mk)

pValue :: Int -> RTValue Symbol Symbol Symbol Type -> Fresh Doc
pValue prec (RTArith op a b _) = operator pArithOp arithPrec pValue op a b prec
pValue prec (RTComp op a b _) = operator pCompOp compPrec pValue op a b prec
pValue prec (ValueSel h n _) = parens (prec > 8) . (\h' -> Fix2 . Sel h' . Fix2 . Prim $ PInt n) <$> pValue 8 h
pValue prec (RTCond c t f _) = parens (prec > 0) . Fix2 <$> liftA3 Cond (pValue 0 c) (pValue 0 t) (pValue 0 f)
pValue _ (RTTuple tup _) = Fix2 . List <$> traverse (pValue 0) (toList tup)
pValue _ (RTPrim prim _) = pure $ Fix2 (Prim prim)
pValue _ (Block blk _) = do
  lbl <- freshBlk
  blk' <- pProg $ instantiate1Over labels lbl blk
  pure $ Fix2 $ Prog lbl blk'
pValue _ (Call f args _) = Fix2 . Call' f <$> traverse (pValue 0) args
pValue prec (PlaceVal pl _) = pPlace prec pl

pPlace :: Int -> RTPlace Symbol Symbol Symbol Type -> Fresh Doc
pPlace _ (Place sym _) = pure . Fix2 $ Symbol sym
pPlace _ (PlaceSel h n _) = (\h' -> Fix2 $ Sel h' $ Fix2 $ Prim $ PInt n) <$> pPlace 9 h
pPlace prec (Deref val _) = parens (prec > 9) . Fix2 . Deref' <$> pValue 9 val
