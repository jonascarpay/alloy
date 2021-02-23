{-# LANGUAGE OverloadedStrings #-}

module Print (ppExpr, ppVal, ppTypedBlock) where

import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as M
import Eval
import Expr
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
ppExpr (BlockExpr b) = ppBlock ppExpr ppExpr b
ppExpr (List l) = list (ppExpr <$> toList l)
ppExpr (Func args ret body) =
  list (uncurry (ppTyped pretty ppExpr) <$> args)
    <+> "->"
    <+> ppExpr ret
    <+> ppBlock ppExpr ppExpr body

ppBlock :: (typ -> Doc ann) -> (expr -> Doc ann) -> Block typ expr -> Doc ann
ppBlock _ _ (Block []) = "{}"
ppBlock ft fe (Block [stmt]) = braces $ ppStatement ft fe stmt
ppBlock ft fe (Block stmts) = braces' $ align $ vcat (ppStatement ft fe <$> stmts)

ppStatement :: (typ -> Doc ann) -> (expr -> Doc ann) -> Stmt typ expr -> Doc ann
ppStatement _ fe (Return expr) = "return" <+> fe expr <> ";"
ppStatement ft fe (Decl name typ expr) = ppTyped pretty ft name typ <+> "=" <+> fe expr <> ";"
ppStatement _ fe (Assign name expr) = pretty name <+> "=" <+> fe expr <> ";"
ppStatement _ fe (ExprStmt expr) = fe expr <> ";"

ppRTExpr :: (typ -> Doc ann) -> (info -> Doc ann) -> RTExpr typ info -> Doc ann
ppRTExpr _ _ (RTVar x _) = pretty x
ppRTExpr _ _ (RTLiteral n _) = ppRTLit n
ppRTExpr pptyp ppinfo (RTArith op a b _) = ppRTExpr pptyp ppinfo a <+> opSymbol op <+> ppRTExpr pptyp ppinfo b
ppRTExpr pptyp ppinfo (RTBlock b _) = ppBlock pptyp (ppRTExpr pptyp ppinfo) b
ppRTExpr pptyp ppinfo (RTCall name args _) = pretty name <> list (ppRTExpr pptyp ppinfo <$> args)

ppRTLit :: RTLiteral -> Doc ann
ppRTLit (RTInt n) = pretty n
ppRTLit (RTDouble n) = pretty n
ppRTLit (RTStruct m) = ppAttrs pretty ppRTLit m

opSymbol :: ArithOp -> Doc ann
opSymbol Add = "+"
opSymbol Mul = "*"
opSymbol Sub = "-"

ppWithRuntimeEnv :: RuntimeEnv -> Doc ann -> Doc ann
ppWithRuntimeEnv (RuntimeEnv fns) doc
  | null fns = doc
  | otherwise =
    align $
      vcat
        [ "Functions:",
          indent 2 . vcat $ (\(name, func) -> pretty name <> ppFunction func) <$> M.toList fns,
          "Body:",
          indent 2 doc
        ]

ppTypedBlock :: Type -> Block Type (RTExpr Type Type) -> Doc ann
ppTypedBlock typ block = ppType typ <> ppBlock ppType (ppRTExpr ppType ppType) block

ppFunction :: Function -> Doc ann
ppFunction (Function args ret body) = list (uncurry (ppTyped pretty ppType) <$> args) <+> "->" <+> ppType ret <+> ppBlock ppType (ppRTExpr ppType ppType) body

ppTyped :: (name -> Doc ann) -> (typ -> Doc ann) -> name -> typ -> Doc ann
ppTyped fname ftype name typ = fname name <> ":" <+> ftype typ

ppMaybeType :: Maybe Type -> Doc ann
ppMaybeType = maybe mempty ppType

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
        (ppRTExpr ppMaybeType ppMaybeType)
        b
    )
ppVal (Fix (VList l)) = list (ppVal <$> toList l)
ppVal (Fix (VFunc env func)) = ppWithRuntimeEnv env $ ppFunction func
ppVal (Fix (VType t)) = ppType t
