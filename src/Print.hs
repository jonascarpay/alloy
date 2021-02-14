{-# LANGUAGE OverloadedStrings #-}

module Print (ppExpr, ppVal) where

import Data.Map (Map)
import Data.Map qualified as M
import Eval
import Expr
import Prettyprinter

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
ppExpr (BlockExpr b) = ppBlock b

ppBlock :: Block Expr -> Doc ann
ppBlock (Block stmts) = braces $ align $ vcat (ppStatement <$> stmts)

ppStatement :: Stmt Expr -> Doc ann
ppStatement (Break expr) = "break" <+> ppExpr expr <> ";"
ppStatement (Decl name expr) = "var" <+> pretty name <+> "=" <+> ppExpr expr <> ";"
ppStatement (Assign name expr) = pretty name <+> "=" <+> ppExpr expr <> ";"

ppVal :: Value -> Doc ann
ppVal (Fix (VInt n)) = pretty n
ppVal (Fix (VAttr attrs)) = ppMap pretty ppVal attrs
ppVal (Fix VClosure {}) = "<<comptime closure>>"
ppVal (Fix VBlock {}) = "<<runtime closure>>"
