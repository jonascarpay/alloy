{-# LANGUAGE OverloadedStrings #-}

module Print (ppExpr, ppVal) where

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
ppPrim (PType x) = ppType x

ppBool :: Bool -> Doc ann
ppBool True = "true"
ppBool False = "false"

ppType :: Type -> Doc ann
ppType TInt = "int"
ppType TDouble = "double"
ppType TBool = "bool"
ppType TVoid = "void"

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
ppExpr (Func args ret body) = list (uncurry (ppTyped pretty ppExpr) <$> args) <+> "->" <+> ppExpr ret <+> ppExpr body

ppBlock :: (typ -> Doc ann) -> (expr -> Doc ann) -> Block typ expr -> Doc ann
ppBlock _ _ (Block []) = "{}"
ppBlock ft fe (Block [stmt]) = braces $ ppStatement ft fe stmt
ppBlock ft fe (Block stmts) = braces' $ align $ vcat (ppStatement ft fe <$> stmts)

ppStatement :: (typ -> Doc ann) -> (expr -> Doc ann) -> Stmt typ expr -> Doc ann
ppStatement _ fe (Return expr) = "return" <+> fe expr <> ";"
ppStatement ft fe (Decl name typ expr) = ppTyped pretty ft name typ <+> "=" <+> fe expr <> ";"
ppStatement _ fe (Assign name expr) = pretty name <+> "=" <+> fe expr <> ";"
ppStatement _ fe (ExprStmt expr) = fe expr <> ";"

ppRTExpr :: RTExpr -> Doc ann
ppRTExpr (RTVar x) = pretty x
ppRTExpr (RTPrim n) = ppPrim n
ppRTExpr (RTArith op a b) = ppRTExpr a <+> opSymbol op <+> ppRTExpr b
ppRTExpr (RTBlock b) = ppBlock ppType ppRTExpr b
ppRTExpr (RTCall name args) = pretty name <> list (ppRTExpr <$> args)

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

ppFunction :: Function Type RTExpr -> Doc ann
ppFunction (Function args ret body) = list (uncurry (ppTyped pretty ppType) <$> args) <+> "->" <+> ppType ret <+> ppBlock ppType ppRTExpr body

ppTyped :: (name -> Doc ann) -> (typ -> Doc ann) -> name -> typ -> Doc ann
ppTyped fname ftype name typ = fname name <> ":" <+> ftype typ

ppVal :: Value -> Doc ann
ppVal (Fix (VPrim n)) = ppPrim n
ppVal (Fix (VAttr attrs)) = ppAttrs pretty ppVal attrs
ppVal (Fix VClosure {}) = "<<closure>>"
ppVal (Fix VRTVar {}) = "I'm not sure, is this even possible?" -- TODO
ppVal (Fix (VBlock env b)) = ppWithRuntimeEnv env (ppBlock ppType ppRTExpr b)
ppVal (Fix (VList l)) = list (ppVal <$> toList l)
ppVal (Fix (VFunc env func)) = ppWithRuntimeEnv env $ ppFunction func
