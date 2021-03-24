{-# LANGUAGE OverloadedStrings #-}

module Print (ppExpr, ppVal, ppTypedBlock) where

import Data.Bool (bool)
import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Eval
import Expr
import Lens.Micro.Platform
import Numeric
import Prettyprinter
import Program

ppAttrs :: (k -> Doc ann) -> (v -> Doc ann) -> Map k v -> Doc ann
ppAttrs fk fv attrs = braces' $ align . vcat $ (\(k, v) -> fk k <> ":" <+> fv v <> ",") <$> M.toList attrs

braces' :: Doc ann -> Doc ann
braces' d = vcat [lbrace, indent 2 d, rbrace]

ppPrim :: Prim -> Doc ann
ppPrim (PInt x) = pretty x
ppPrim (PDouble x) = pretty x
ppPrim (PBool x) = ppBool x

ppBool :: Bool -> Doc ann
ppBool True = "true"
ppBool False = "false"

ppType :: Type -> Doc ann
ppType TInt = "<int>"
ppType TDouble = "<double>"
ppType TVoid = "<void>"
ppType TBool = "<bool>"
ppType (TStruct m) = angles $ "struct" <> ppAttrs pretty ppType m

ppExpr :: Expr -> Doc ann
ppExpr (Var x) = pretty x
ppExpr (App a b) = ppExpr a <+> ppExpr b
ppExpr (Lam a b) = parens $ pretty a <> ":" <+> ppExpr b
ppExpr (Let args body) =
  vcat
    [ "let",
      indent 2 $ vcat ((\(name, bbody) -> pretty name <+> "=" <+> ppExpr bbody <> ";") <$> args),
      "in",
      indent 2 $ ppExpr body
    ]
ppExpr (Prim n) = ppPrim n
ppExpr (BinExpr op a b) = ppExpr a <+> opSymbol op <+> ppExpr b
ppExpr (Attr m) = ppAttrs pretty ppExpr m
ppExpr (Acc a m) = ppExpr m <> "." <> pretty a
ppExpr (BlockExpr b) = ppBlock (maybe mempty (ppAnn ppExpr)) ppExpr b
ppExpr (List l) = list (ppExpr <$> toList l)
ppExpr (With bind body) = "with" <+> ppExpr bind <> ";" <+> ppExpr body
ppExpr (Cond cond tr fl) = "if" <+> ppExpr cond <+> "then" <+> ppExpr tr <+> "else" <+> ppExpr fl
ppExpr (Func args ret body) =
  list (uncurry (ppTyped pretty ppExpr) <$> args)
    <+> "->"
    <+> ppExpr ret
    <+> ppExpr body

ppLabel :: Maybe Name -> Doc ann
ppLabel (Just lbl) = pretty lbl <> "@"
ppLabel Nothing = mempty

-- TODO just take ppStatement
ppBlock :: (typ -> Doc ann) -> (expr -> Doc ann) -> Block typ expr -> Doc ann
ppBlock _ _ (Block mlbl []) = ppLabel mlbl <> "{}"
ppBlock ft fe (Block lbl [stmt]) = ppLabel lbl <> braces (ppStatement ft fe stmt)
ppBlock ft fe (Block lbl stmts) = ppLabel lbl <> braces' (align $ vcat (ppStatement ft fe <$> stmts))

ppStatement :: (typ -> Doc ann) -> (expr -> Doc ann) -> Stmt typ expr -> Doc ann
ppStatement _ fe (Return expr) = "return" <+> fe expr <> ";"
ppStatement ft fe (Decl name typ expr) = pretty name <+> ft typ <+> "=" <+> fe expr <> ";"
ppStatement _ fe (Assign name expr) = pretty name <+> "=" <+> fe expr <> ";"
ppStatement _ fe (ExprStmt expr) = fe expr <> ";"
ppStatement _ fe (Break mlbl mexpr) = "break" <> mspace (("@" <>) . pretty) mlbl <> mspace fe mexpr <> ";"
ppStatement _ _ (Continue mlbl) = "continue" <> mspace (("@" <>) . pretty) mlbl <> ";"

-- insert a space if Just, otherwise empty
mspace :: (a -> Doc ann) -> Maybe a -> Doc ann
mspace f (Just a) = " " <> f a
mspace _ Nothing = mempty

ppRTExpr :: Dependencies -> (call -> Doc ann) -> (typ -> Doc ann) -> (info -> Doc ann) -> RTExpr call typ info -> Doc ann
ppRTExpr deps ppCall pptyp ppinfo = go
  where
    go (RTVar x _) = pretty x
    go (RTLiteral n _) = ppRTLit n
    go (RTBin op a b _) = go a <+> opSymbol op <+> go b
    go (RTBlock b _) = ppBlock pptyp go b
    go (RTCall call args _) = ppCall call <> list (go <$> args)
    go (RTCond cond tr fl _) = "if" <+> go cond <+> "then" <+> go tr <+> "else" <+> go fl

lookupFun :: Dependencies -> GUID -> FunDef GUID
lookupFun deps guid = fromMaybe err $ deps ^. depKnownFuncs . at guid
  where
    err = error "Referencing non-existing GUID, should be impossible"

ppGuid :: GUID -> Doc ann
ppGuid g = pretty $ take 5 $ show g

ppRTLit :: RTLiteral -> Doc ann
ppRTLit (RTInt n) = pretty n
ppRTLit (RTDouble n) = pretty n
ppRTLit (RTStruct m) = ppAttrs pretty ppRTLit m
ppRTLit (RTBool b) = bool "false" "true" b

opSymbol :: BinOp -> Doc ann
opSymbol (ArithOp op) = arit op
  where
    arit Add = "+"
    arit Mul = "*"
    arit Sub = "-"
opSymbol (CompOp op) = comp op
  where
    comp Lt = "<"
    comp Gt = ">"
    comp Eq = "=="
    comp Neq = "/="
    comp Leq = "<="
    comp Geq = ">="

ppFunctionName :: Name -> GUID -> Doc ann
ppFunctionName name guid = pretty name <> "_" <> ppGuid guid

ppWithDeps :: Dependencies -> Maybe GUID -> Doc ann -> Doc ann
ppWithDeps deps@(Dependencies fns temp) censor doc
  | not (null temp) = "<<impossible>>"
  | null fns = doc
  | otherwise =
    align $
      vcat
        [ "Functions:",
          indent 2 . vcat $ ppFunDef deps <$> guidsByNames fns',
          "Body:",
          indent 2 doc
        ]
  where
    fns' = maybe id M.delete censor fns

guidsByNames :: Map GUID (FunDef GUID) -> [GUID]
guidsByNames m = snd <$> M.keys namedMap
  where
    namedMap =
      M.foldMapWithKey (\guid fn -> M.singleton (fn ^. fnName, guid) fn) m

ppTypedBlock :: Dependencies -> Type -> Block Type (RTExpr GUID Type Type) -> Doc ann
ppTypedBlock deps typ block =
  ppWithDeps deps Nothing $
    ppType typ
      <> ppBlock
        (ppAnn ppType)
        (ppRTExpr deps ppGuid (ppAnn ppType) (ppAnn ppType))
        block

ppAnn :: (typ -> Doc ann) -> (typ -> Doc ann)
ppAnn f t = ":" <+> f t

ppFunDef :: Dependencies -> GUID -> Doc ann
ppFunDef deps guid =
  case deps ^. depKnownFuncs . at guid of
    Nothing -> error "referencing non-existing GUID, should be impossible"
    Just (FunDef args ret name body) ->
      hsep
        [ ppFunctionName name guid,
          list (uncurry (ppTyped pretty ppType) <$> args),
          "->",
          ppType ret,
          -- TODO combine with ppTypedBlock
          ppBlock
            (ppAnn ppType)
            ( ppRTExpr
                deps
                (ppKnownGuid deps . CallKnown)
                (ppAnn ppType)
                (ppAnn ppType)
            )
            body
        ]

ppTyped :: (name -> Doc ann) -> (typ -> Doc ann) -> name -> typ -> Doc ann
ppTyped fname ftype name typ = fname name <> ":" <+> ftype typ

ppMaybeType :: Maybe Type -> Doc ann
ppMaybeType Nothing = "_"
ppMaybeType (Just typ) = ppType typ

ppKnownGuid :: Dependencies -> PreCall -> Doc ann
ppKnownGuid deps (CallKnown guid) =
  case deps ^. depKnownFuncs . at guid of
    Nothing -> angles $ "impossible guid " <> ppGuid guid
    Just fd -> ppFunctionName (fd ^. fnName) guid

ppVal :: Value -> Doc ann
ppVal (Fix (VPrim n)) = ppPrim n
ppVal (Fix (VAttr attrs)) = ppAttrs pretty ppVal attrs
ppVal (Fix VClosure {}) = "<<closure>>"
ppVal (Fix VClosure' {}) = "<<closure'>>"
ppVal (Fix VRTVar {}) = "I'm not sure, is this even possible?" -- TODO
ppVal (Fix (VBlock deps b)) =
  ppWithDeps
    deps
    Nothing
    $ ppBlock
      (const "<var>")
      (ppRTExpr deps (ppKnownGuid deps) (const "<var>") (const "<var>"))
      b
ppVal (Fix (VList l)) = list (ppVal <$> toList l)
ppVal (Fix (VFunc deps (Right guid))) =
  ppWithDeps deps (Just guid) $ ppFunDef deps guid
ppVal (Fix (VType t)) = ppType t
