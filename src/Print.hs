{-# LANGUAGE OverloadedStrings #-}

module Print (ppExpr, ppVal) where

import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as M
import Eval
import Expr
import Prettyprinter
import Program

ppMap :: (k -> Doc ann) -> (v -> Doc ann) -> Map k v -> Doc ann
ppMap fk fv attrs = braces' $ align . vcat $ (\(k, v) -> fk k <+> ":" <+> fv v <> ",") <$> M.toList attrs

braces' :: Doc ann -> Doc ann
braces' d = vcat [lbrace, indent 2 d, rbrace]

ppExpr :: Expr -> Doc ann
ppExpr (Var x) = pretty x
ppExpr (App a b) = ppExpr a <+> ppExpr b
ppExpr (Lam a b) = parens $ pretty a <> ":" <+> ppExpr b
ppExpr (Lit n) = pretty n
ppExpr (Arith op a b) = ppExpr a <+> opSymbol op <+> ppExpr b
ppExpr (Attr m) = ppMap pretty ppExpr m
ppExpr (Acc a m) = ppExpr m <> "." <> pretty a
ppExpr (BlockExpr b) = ppBlock ppExpr b
ppExpr (List l) = list (ppExpr <$> toList l)
ppExpr (Func args body) = list (pretty <$> args) <> ":" <+> ppExpr body

ppBlock :: (expr -> Doc ann) -> Block expr -> Doc ann
ppBlock _ (Block []) = "{}"
ppBlock f (Block [stmt]) = braces $ ppStatement f stmt
ppBlock f (Block stmts) = braces' $ align $ vcat (ppStatement f <$> stmts)

ppStatement :: (epxr -> Doc ann) -> Stmt epxr -> Doc ann
ppStatement f (Break expr) = "break" <+> f expr <> ";"
ppStatement f (Decl name expr) = "var" <+> pretty name <+> "=" <+> f expr <> ";"
ppStatement f (Assign name expr) = pretty name <+> "=" <+> f expr <> ";"
ppStatement f (ExprStmt expr) = f expr <> ";"

ppRTExpr :: RTExpr -> Doc ann
ppRTExpr (RTVar x) = pretty x
ppRTExpr (RTLit n) = pretty n
ppRTExpr (RTArith op a b) = ppRTExpr a <+> opSymbol op <+> ppRTExpr b
ppRTExpr (RTBlock b) = ppBlock ppRTExpr b
ppRTExpr (RTCall name args) = pretty name <> list (ppRTExpr <$> args)

opSymbol :: ArithOp -> Doc ann
opSymbol Add = "+"
opSymbol Mul = "*"
opSymbol Sub = "-"

ppWithRuntimeEnv :: RuntimeEnv -> Doc ann -> Doc ann
ppWithRuntimeEnv (RuntimeEnv fns) doc
  | null fns = doc
  | otherwise =
    vcat
      [ "let",
        indent 2 . vcat $ (\(name, (args, body)) -> ppFunction name args body) <$> M.toList fns,
        "in",
        indent 2 doc
      ]

ppFunction :: Name -> [Name] -> Block RTExpr -> Doc ann
ppFunction name args body = pretty name <> list (pretty <$> args) <> ":" <+> ppBlock ppRTExpr body

ppVal :: Value -> Doc ann
ppVal (Fix (VInt n)) = pretty n
ppVal (Fix (VAttr attrs)) = ppMap pretty ppVal attrs
ppVal (Fix VClosure {}) = "<<comptime closure>>"
ppVal (Fix VRTVar {}) = error "I'm not sure" -- TODO
ppVal (Fix (VBlock env b)) = ppWithRuntimeEnv env (ppBlock ppRTExpr b)
ppVal (Fix (VList l)) = list (ppVal <$> toList l)
ppVal (Fix (VFunc env args body)) = ppWithRuntimeEnv env $ ppFunction "" args body
