{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Print.Doc
  ( DocF (..),
    Doc,
    StatementF (..),
    toDoc,
  )
where

import Control.Applicative
import Control.Monad.State
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Bool
import Data.Foldable
import Data.HashMap.Strict qualified as HM
import Data.Map (Map)
import Data.Map qualified as M
import Data.String
import Data.Text qualified as T
import Data.Void
import Eval.Lib (extractVal, foldMNF, foldType, labels, rtFuncCalls, vars)
import Eval.Types
import Expr hiding (Expr (..))
import Lens.Micro.Platform (over)
import Lib.Fresh
import Numeric (showHex)
import Print.Bifree
import Rebound

toDoc :: NF -> Doc
toDoc = runFresh . pNF

data DocF stm doc
  = Parens doc
  | Symbol Symbol
  | List [doc]
  | Operator doc doc doc
  | Sel doc doc -- Combine with Operator?
  | Unary doc doc
  | Call' doc [doc]
  | Func [(doc, doc)] doc doc
  | Prog (Maybe doc) stm
  | Attrs' (Map Symbol doc)
  | Module (Map Symbol doc) doc
  | Cond doc doc doc
  deriving (Functor, Foldable, Traversable)

instance IsString (DocF g f) where
  fromString = Symbol . fromString

instance Bifunctor DocF where
  bimap _ r (Parens doc) = Parens (r doc)
  bimap _ _ (Symbol sym) = Symbol sym
  bimap _ r (List docs) = List (r <$> docs)
  bimap _ r (Operator sym lhs rhs) = Operator (r sym) (r lhs) (r rhs)
  bimap _ r (Unary op d) = Unary (r op) (r d)
  bimap _ r (Sel h n) = Sel (r h) (r n)
  bimap _ r (Call' sym args) = Call' (r sym) (r <$> args)
  bimap _ r (Func args ret body) = Func (bimap r r <$> args) (r ret) (r body)
  bimap _ r (Attrs' m) = Attrs' (r <$> m)
  bimap _ r (Module m d) = Module (r <$> m) (r d)
  bimap _ r (Cond c t f) = Cond (r c) (r t) (r f)
  bimap l r (Prog sym blk) = Prog (r <$> sym) (l blk)

instance Bifoldable DocF where
  bifoldr _ r a (Parens doc) = r doc a
  bifoldr _ _ a (Symbol _) = a
  bifoldr _ r a (List docs) = foldr r a docs
  bifoldr _ r a (Operator _ lhs rhs) = r lhs (r rhs a)
  bifoldr _ r a (Unary op d) = r op (r d a)
  bifoldr _ r a (Sel h n) = r h (r n a)
  bifoldr _ r a (Call' _ args) = foldr r a args
  bifoldr _ r a (Func args ret body) = foldr (flip $ bifoldr r r) (r ret (r body a)) args
  bifoldr _ r a (Attrs' m) = foldr r a m
  bifoldr _ r a (Module m d) = foldr r (r d a) m
  bifoldr _ r a (Cond c t f) = r c (r t (r f a))
  bifoldr l _ a (Prog _ blk) = l blk a

-- TODO StatementDocF? SDocF? ProgF? PDoc?
data StatementF doc stm
  = SDecl Symbol doc doc stm
  | SAssign doc doc stm
  | SBreak doc doc
  | SContinue doc
  | SExpr doc (Maybe stm)
  deriving (Functor, Foldable, Traversable)

instance Bifunctor StatementF where
  bimap l r (SDecl sym lhs rhs k) = SDecl sym (l lhs) (l rhs) (r k)
  bimap l r (SAssign lhs rhs k) = SAssign (l lhs) (l rhs) (r k)
  bimap l _ (SBreak lbl expr) = SBreak (l lbl) (l expr)
  bimap l _ (SContinue lbl) = SContinue (l lbl)
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

pCompOp :: CompOp -> Symbol
pCompOp Eq = "=="
pCompOp Neq = "!="
pCompOp Lt = "<"
pCompOp Gt = ">"
pCompOp Leq = "<="
pCompOp Geq = ">="

operator :: Applicative m => (op -> Symbol) -> (op -> Int) -> (Int -> a -> m Doc) -> op -> a -> a -> Int -> m Doc
operator pOp opPrec pRec op l r precCtx =
  liftA2
    (\l' r' -> parens (prec < precCtx) $ Bifix $ Operator (Bifix $ Symbol $ pOp op) l' r')
    (pRec prec l)
    (pRec prec r)
  where
    prec = opPrec op

pDeps :: Deps -> Fresh (Map Symbol Doc)
pDeps (Deps closed _) = M.fromList <$> traverse (bitraverse (pure . pHash) toDoc) (HM.toList closed)
  where
    toDoc :: RTFunc Hash -> Fresh Doc
    toDoc func = pFunc $ over rtFuncCalls (Bifix . Symbol . pHash) func

pFunc :: RTFunc Doc -> Fresh Doc
pFunc (RTFunc args ret body) = do
  args' <- forM args $ \(Name nm, typ) -> do
    arg <- fresh nm
    pure (Bifix $ Symbol arg, pType typ)
  let ret' = pType ret
  body' <-
    traverseAst
      (pure . unbind (\ix -> fst (args' !! ix)) absurd)
      absurd
      pure
      (const $ pure . pPrim)
      (pure . pType)
      body
  Bifix . Func args' ret' <$> pValue 0 body'

pHash :: Hash -> Symbol
pHash (Hash h) = mappend "fn_" $ T.pack $ take 7 (showHex (fromIntegral h :: Word) "")

pNF :: NF -> Fresh Doc
pNF = foldMNF go
  where
    go :: Value Doc -> Fresh Doc
    go (VClosure _) = pure . Bifix $ Symbol "<closure>"
    go (VRTValue deps val) = do
      deps' <- pDeps deps
      val' <-
        traverseAst
          (error "impossible - variable escaped scope")
          (error "impossible - block escaped scope")
          (either (error "impossible - open function call escaped scope") (pure . Bifix . Symbol . pHash))
          (const $ pure . pPrim)
          (const . pure . Bifix $ "?")
          val
      Bifix . Module deps' <$> pValue 0 val'
    go (VType typ) = pure $ pType typ
    go (VPrim prim) = pure $ pPrim prim
    go (VFunc deps (Right hash)) = do
      deps' <- pDeps deps
      pure . Bifix $ Module deps' (Bifix . Symbol $ pHash hash)
    go (VFunc _ _) = pure $ Bifix "<open function index, how did you do this this is a bug>"
    go (VString str) = pure . showDoc $ str
    go (VAttr m) = pure . Bifix $ Attrs' m
    go (VList l) = pure . Bifix . List $ toList l
    go (VBlk _) = pure $ Bifix "<block index, how did you do this this is a bug>"
    go (VRTPlace _ _) = pure $ Bifix "<lvalue, how did you do this this is a bug>"

showDoc :: Show a => a -> Doc
showDoc = Bifix . Symbol . T.pack . show

pType :: Type -> Doc
pType = foldType go
  where
    go :: TypeF Doc -> Doc
    go TVoid = Bifix $ Symbol "void"
    go TBool = Bifix $ Symbol "bool"
    go TInt = Bifix $ Symbol "int"
    go TDouble = Bifix $ Symbol "double"
    go (TTuple ts) = Bifix $ List $ toList ts
    go (TPtr t) = Bifix $ Unary (Bifix $ Symbol "*") t

pProg :: RTProg Doc Doc Doc Doc Doc -> Fresh Statement
pProg (Decl (Name n) _ val k) = do
  val' <- pValue 0 val
  var <- fresh n
  k' <- pProg $ instantiate1Over vars (Bifix $ Symbol var) k
  pure $ Bifix $ SDecl var (extractVal val) val' k'
pProg (Assign plc val k) = Bifix <$> liftA3 SAssign (pPlace 0 plc) (pValue 0 val) (pProg k)
pProg (Break lbl val) = Bifix . SBreak lbl <$> pValue 0 val
pProg (Continue lbl) = pure $ Bifix $ SContinue lbl
pProg (ExprStmt expr mk) = Bifix <$> liftA2 SExpr (pValue 0 expr) (traverse pProg mk)

pPrim :: Prim -> Doc
pPrim = go
  where
    go (PInt n) = showDoc n
    go (PDouble n) = showDoc n
    go (PBool n) = Bifix $ bool "false" "true" n
    go PVoid = Bifix "void"

pValue :: Int -> RTValue Doc Doc Doc Doc Doc -> Fresh Doc
pValue prec (RTArith op a b _) = operator pArithOp arithPrec pValue op a b prec
pValue prec (RTComp op a b _) = operator pCompOp compPrec pValue op a b prec
pValue prec (ValueSel h n _) = parens (prec > 8) . (\h' -> Bifix . Sel h' . pPrim $ PInt n) <$> pValue 8 h
pValue prec (RTCond c t f _) = parens (prec > 0) . Bifix <$> liftA3 Cond (pValue 0 c) (pValue 0 t) (pValue 0 f)
pValue _ (RTTuple tup _) = Bifix . List <$> traverse (pValue 0) (toList tup)
pValue _ (RTLit lit _) = pure lit
pValue _ (Block blk _) =
  case maybeUnusedOver labels blk of
    Just blk' -> Bifix . Prog Nothing <$> pProg blk'
    Nothing -> do
      lbl <- Bifix . Symbol <$> fresh "lbl"
      Bifix . Prog (Just lbl) <$> pProg (instantiate1Over labels lbl blk)
pValue _ (Call f args _) = Bifix . Call' f <$> traverse (pValue 0) args
pValue prec (PlaceVal pl _) = pPlace prec pl
pValue prec (RTRef r _) = parens (prec > 9) . Bifix . Unary (Bifix $ Symbol "&") <$> pPlace 9 r

pPlace :: Int -> RTPlace Doc Doc Doc Doc Doc -> Fresh Doc
pPlace _ (Place sym _) = pure sym
pPlace _ (PlaceSel h n _) = (\h' -> Bifix $ Sel h' $ pPrim $ PInt n) <$> pPlace 9 h
pPlace prec (RTDeref val _) = parens (prec > 9) . Bifix . Unary (Bifix $ Symbol "^") <$> pValue 9 val
