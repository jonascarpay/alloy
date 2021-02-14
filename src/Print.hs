{-# LANGUAGE OverloadedStrings #-}

module Print (ppExpr, ppVal) where

import Data.Map (Map)
import Data.Map qualified as M
import Eval
import Expr
import Prettyprinter
import Program

ppMap :: (k -> Doc ann) -> (v -> Doc ann) -> Map k v -> Doc ann
ppMap fk fv attrs = braces $ align . vcat $ (\(k, v) -> hsep [fk k, "=", fv v <> ";"]) <$> M.toList attrs

ppExpr :: Expr -> Doc ann
ppExpr (Var x) = pretty x
ppExpr (App a b) = ppExpr a <+> ppExpr b
ppExpr (Lam a b) = parens $ pretty a <> ":" <+> ppExpr b
ppExpr (Lit n) = pretty n
ppExpr (Arith _ a b) = parens $ hsep ["_", ppExpr a, ppExpr b]
ppExpr (Attr m) = ppMap pretty ppExpr m
ppExpr (Acc a m) = ppExpr m <> "." <> pretty a
ppExpr (BlockExpr b) = ppBlock ppExpr b

ppBlock :: (expr -> Doc ann) -> Block expr -> Doc ann
ppBlock f (Block stmts) = braces $ align $ vcat (ppStatement f <$> stmts)

ppStatement :: (epxr -> Doc ann) -> Stmt epxr -> Doc ann
ppStatement f (Break expr) = "break" <+> f expr <> ";"
ppStatement f (Decl name expr) = "var" <+> pretty name <+> "=" <+> f expr <> ";"
ppStatement f (Assign name expr) = pretty name <+> "=" <+> f expr <> ";"

ppRTExpr :: RTExpr -> Doc ann
ppRTExpr (RTVar x) = pretty x
ppRTExpr (RTLit n) = pretty n
ppRTExpr (RTArith _ a b) = parens $ "_" <+> ppRTExpr a <+> ppRTExpr b
ppRTExpr (RTBlock b) = ppBlock ppRTExpr b

ppVal :: Value -> Doc ann
ppVal (Fix (VInt n)) = pretty n
ppVal (Fix (VAttr attrs)) = ppMap pretty ppVal attrs
ppVal (Fix VClosure {}) = "<<comptime closure>>"
ppVal (Fix VRTVar {}) = error "I'm not sure" -- TODO
ppVal (Fix (VBlock (Closure env b)))
  | null env = ppBlock ppRTExpr b
  | otherwise = error "closure actually has values" -- TODO
