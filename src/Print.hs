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
ppPrim (PString str) = dquotes $ pretty str

ppBool :: Bool -> Doc ann
ppBool True = "true"
ppBool False = "false"

ppRepr :: Repr -> Doc ann
ppRepr RInt = "<int>"
ppRepr RDouble = "<double>"
ppRepr RVoid = "<void>"
ppRepr RBool = "<bool>"
ppRepr (RStruct m) = angles $ "struct" <> ppAttrs pretty ppType m

ppType :: Type -> Doc ann
ppType (TRepr rep) = ppRepr rep
ppType (TUser tid t) = angles $ "user_" <> pretty tid <> ppType t

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
ppExpr (Attr m) = ppAttrs ppExpr ppExpr m
ppExpr (Acc a m) = ppExpr m <> "." <> brackets (ppExpr a)
ppExpr (BlockExpr b) = ppBlock pretty pretty (maybe mempty (ppAnn ppExpr)) ppExpr b
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
ppBlock :: (var -> Doc ann) -> (lbl -> Doc ann) -> (typ -> Doc ann) -> (expr -> Doc ann) -> Block var lbl typ expr -> Doc ann
ppBlock _ ppLbl _ _ (Block mlbl [] _) = ppMLabel ppLbl mlbl <> "{}"
ppBlock ppVar ppLbl ft fe (Block lbl [stmt] _) = ppMLabel ppLbl lbl <> braces (ppStatement ppVar ppLbl ft fe stmt)
ppBlock ppVar ppLbl ft fe (Block lbl stmts _) = ppMLabel ppLbl lbl <> braces' (align $ vcat (ppStatement ppVar ppLbl ft fe <$> stmts))

ppStatement :: (var -> Doc ann) -> (lbl -> Doc ann) -> (typ -> Doc ann) -> (expr -> Doc ann) -> Stmt var lbl typ expr -> Doc ann
ppStatement ppVar ppLbl _ fe (Return expr) = "return" <+> fe expr <> ";"
ppStatement ppVar ppLbl ft fe (Decl name typ expr) = ppVar name <+> ft typ <+> "=" <+> fe expr <> ";"
ppStatement ppVar ppLbl _ fe (Assign name expr) = ppVar name <+> "=" <+> fe expr <> ";"
ppStatement ppVar ppLbl _ fe (ExprStmt expr) = fe expr <> ";"
ppStatement ppVar ppLbl _ fe (Break mlbl mexpr) = "break" <> mspace (("@" <>) . ppLbl) mlbl <> mspace fe mexpr <> ";"
ppStatement ppVar ppLbl _ _ (Continue mlbl) = "continue" <> mspace (("@" <>) . ppLbl) mlbl <> ";"

ppMLabel :: (lbl -> Doc ann) -> (Maybe lbl -> Doc ann)
ppMLabel f (Just lbl) = f lbl <> "@"
ppMLabel f Nothing = mempty

-- insert a space if Just, otherwise empty
mspace :: (a -> Doc ann) -> Maybe a -> Doc ann
mspace f (Just a) = " " <> f a
mspace _ Nothing = mempty

ppRTExpr :: Dependencies -> (var -> Doc ann) -> (lbl -> Doc ann) -> (call -> Doc ann) -> (typ -> Doc ann) -> (info -> Doc ann) -> RTExpr var lbl call typ info -> Doc ann
ppRTExpr deps ppVar ppLbl ppCall pptyp ppinfo = go
  where
    go (RTVar x _) = ppVar x
    go (RTAccessor expr field _) = go expr <> "." <> pretty field
    go (RTLiteral n _) = ppRTLit n
    go (RTBin op a b _) = go a <+> opSymbol op <+> go b
    go (RTBlock b _) = ppBlock ppVar ppLbl pptyp go b
    go (RTCall call args _) = ppCall call <> list (go <$> args)
    go (RTCond cond tr fl _) = "if" <+> go cond <+> "then" <+> go tr <+> "else" <+> go fl
    go (RTStruct m _) = ppAttrs pretty go m

lookupFun :: Dependencies -> GUID -> FunDef Slot LabelID GUID
lookupFun deps guid = fromMaybe err $ deps ^. depKnownFuncs . at guid
  where
    err = error "Referencing non-existing GUID, should be impossible"

ppGuid :: GUID -> Doc ann
ppGuid g = pretty $ take 5 $ show g

ppRTLit :: RTLiteral -> Doc ann
ppRTLit (RTInt n) = pretty n
ppRTLit (RTDouble n) = pretty n
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
opSymbol Concat = "++"

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

guidsByNames :: Map GUID (FunDef var lbl GUID) -> [GUID]
guidsByNames m = snd <$> M.keys namedMap
  where
    namedMap =
      M.foldMapWithKey (\guid fn -> M.singleton (fn ^. fnName, guid) fn) m

ppTypedBlock :: Dependencies -> Type -> RTBlock Slot LabelID GUID Type -> Doc ann
ppTypedBlock deps typ block =
  ppWithDeps deps Nothing $
    ppType typ
      <> ppBlock
        ppSlot
        ppLabel'
        (ppAnn ppType)
        (ppRTExpr deps ppSlot ppLabel' ppGuid (ppAnn ppType) (ppAnn ppType))
        block

ppLabel' :: LabelID -> Doc ann
ppLabel' (LabelID n) = "lbl_" <> pretty n

ppAnn :: (typ -> Doc ann) -> (typ -> Doc ann)
ppAnn f t = ":" <+> f t

ppFunDef :: Dependencies -> GUID -> Doc ann
ppFunDef deps guid =
  case deps ^. depKnownFuncs . at guid of
    Nothing -> error "referencing non-existing GUID, should be impossible"
    Just (FunDef args ret name body) ->
      hsep
        [ ppFunctionName name guid,
          list (uncurry (ppTyped ppSlot ppType) <$> args),
          "->",
          ppType ret,
          -- TODO combine with ppTypedBlock
          ppBlock
            ppSlot
            ppLabel'
            (ppAnn ppType)
            ( ppRTExpr
                deps
                ppSlot
                ppLabel'
                (ppKnownGuid deps . CallKnown)
                (ppAnn ppType)
                (ppAnn ppType)
            )
            body
        ]

ppSlot :: Slot -> Doc ann
ppSlot (Argument n) = "arg_" <> pretty n
ppSlot (Local n) = "var_" <> pretty n

ppTyped :: (name -> Doc ann) -> (typ -> Doc ann) -> name -> typ -> Doc ann
ppTyped fname ftype name typ = fname name <> ":" <+> ftype typ

ppKnownGuid :: Dependencies -> PreCall -> Doc ann
ppKnownGuid deps (CallKnown guid) =
  case deps ^. depKnownFuncs . at guid of
    Nothing -> angles $ "impossible guid " <> ppGuid guid
    Just fd -> ppFunctionName (fd ^. fnName) guid

ppVal :: Value -> Doc ann
ppVal (Fix (VPrim n)) = ppPrim n
ppVal (Fix (VAttr attrs)) = ppAttrs (ppVal . fromOrdValue) ppVal attrs
ppVal (Fix VClosure {}) = "<<closure>>"
ppVal (Fix VClosure' {}) = "<<closure'>>"
ppVal (Fix VRTVar {}) = "I'm not sure, is this even possible?" -- TODO
ppVal (Fix (VBlock deps b)) = "<<body expression>>"
ppVal (Fix (VList l)) = list (ppVal <$> toList l)
ppVal (Fix (VFunc deps (Right guid))) =
  ppWithDeps deps (Just guid) $ ppFunDef deps guid
ppVal (Fix (VType t)) = ppType t
