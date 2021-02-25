{-# LANGUAGE OverloadedStrings #-}

module Print (ppExpr, ppVal, ppTypedBlock) where

import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as M
import Eval
import Expr
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
ppExpr (Arith op a b) = ppExpr a <+> opSymbol op <+> ppExpr b
ppExpr (Attr m) = ppAttrs pretty ppExpr m
ppExpr (Acc a m) = ppExpr m <> "." <> pretty a
ppExpr (BlockExpr b) = ppBlock (ppMaybeAnn ppExpr) ppExpr b
ppExpr (List l) = list (ppExpr <$> toList l)
ppExpr (With bind body) = "with" <+> ppExpr bind <> ";" <+> ppExpr body
ppExpr (Func args ret body) =
  list (uncurry (ppTyped pretty ppExpr) <$> args)
    <+> "->"
    <+> ppExpr ret
    <+> ppBlock (ppMaybeAnn ppExpr) ppExpr body

ppBlock :: (typ -> Doc ann) -> (expr -> Doc ann) -> Block typ expr -> Doc ann
ppBlock _ _ (Block []) = "{}"
ppBlock ft fe (Block [stmt]) = braces $ ppStatement ft fe stmt
ppBlock ft fe (Block stmts) = braces' $ align $ vcat (ppStatement ft fe <$> stmts)

ppStatement :: (typ -> Doc ann) -> (expr -> Doc ann) -> Stmt typ expr -> Doc ann
ppStatement _ fe (Return expr) = "return" <+> fe expr <> ";"
ppStatement ft fe (Decl name typ expr) = pretty name <> ft typ <+> "=" <+> fe expr <> ";"
ppStatement _ fe (Assign name expr) = pretty name <+> "=" <+> fe expr <> ";"
ppStatement _ fe (ExprStmt expr) = fe expr <> ";"

ppRTExpr :: RuntimeEnv -> (typ -> Doc ann) -> (info -> Doc ann) -> RTExpr typ info -> Doc ann
ppRTExpr _ _ _ (RTVar x _) = pretty x
ppRTExpr _ _ _ (RTLiteral n _) = ppRTLit n
ppRTExpr env pptyp ppinfo (RTArith op a b _) = ppRTExpr env pptyp ppinfo a <+> opSymbol op <+> ppRTExpr env pptyp ppinfo b
ppRTExpr env pptyp ppinfo (RTBlock b _) = ppBlock pptyp (ppRTExpr env pptyp ppinfo) b
ppRTExpr env pptyp ppinfo (RTCall guid args _) =
  case M.lookup guid (rtFunctions env) of
    Just (_, info) -> ppFunctionName guid info <> list (ppRTExpr env pptyp ppinfo <$> args)
    Nothing -> undefined

ppGuid :: GUID -> Doc ann
ppGuid (GUID n) = pretty $ take 5 $ showHex (fromIntegral n :: Word) ""

ppRTLit :: RTLiteral -> Doc ann
ppRTLit (RTInt n) = pretty n
ppRTLit (RTDouble n) = pretty n
ppRTLit (RTStruct m) = ppAttrs pretty ppRTLit m

opSymbol :: ArithOp -> Doc ann
opSymbol Add = "+"
opSymbol Mul = "*"
opSymbol Sub = "-"

ppWithRuntimeEnv :: RuntimeEnv -> Doc ann -> Doc ann
ppWithRuntimeEnv env@(RuntimeEnv fns) doc
  | null (rtFunctions env) = doc
  | otherwise =
    align $
      vcat
        [ "Functions:",
          indent 2 . vcat $ uncurry (ppFunction env) <$> M.elems fns,
          "Body:",
          indent 2 doc
        ]

ppTypedBlock :: RuntimeEnv -> Type -> Block Type (RTExpr Type Type) -> Doc ann
ppTypedBlock env typ block =
  ppWithRuntimeEnv env $
    ppType typ <> ppBlock ppType (ppRTExpr env ppType ppType) block

ppFunctionName :: GUID -> FunctionInfo -> Doc ann
ppFunctionName guid name =
  mconcat
    [maybe "anon_fn" pretty name, "_", ppGuid guid]

ppFunction :: RuntimeEnv -> Function -> FunctionInfo -> Doc ann
ppFunction env (Function args ret body guid) info =
  hsep
    [ ppFunctionName guid info,
      list (uncurry (ppTyped pretty ppType) <$> args),
      "->",
      ppType ret,
      ppBlock ppType (ppRTExpr env ppType ppType) body
    ]

ppTyped :: (name -> Doc ann) -> (typ -> Doc ann) -> name -> typ -> Doc ann
ppTyped fname ftype name typ = fname name <> ":" <+> ftype typ

ppMaybeAnn :: (typ -> Doc ann) -> (Maybe typ -> Doc ann)
ppMaybeAnn _ Nothing = mempty
ppMaybeAnn f (Just typ) = ": " <> f typ

ppMaybeType :: Maybe Type -> Doc ann
ppMaybeType = ppMaybeAnn ppType

ppVal :: Value -> Doc ann
ppVal (Fix (VPrim n)) = ppPrim n
ppVal (Fix (VAttr attrs)) = ppAttrs pretty ppVal attrs
ppVal (Fix VClosure {}) = "<<closure>>"
ppVal (Fix VClosure' {}) = "<<closure'>>"
ppVal (Fix VRTVar {}) = "I'm not sure, is this even possible?" -- TODO
ppVal (Fix (VBlock env b)) =
  ppWithRuntimeEnv
    env
    ( ppBlock
        ppMaybeType
        (ppRTExpr env ppMaybeType ppMaybeType)
        b
    )
ppVal (Fix (VList l)) = list (ppVal <$> toList l)
ppVal (Fix (VFunc env info func)) = ppWithRuntimeEnv env $ ppFunction env func info
ppVal (Fix (VType t)) = ppType t
