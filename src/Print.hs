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
ppExpr (Fix e) = ppExprF ppExpr e

ppExprF :: (r -> Doc ann) -> ExprF r -> Doc ann
ppExprF f = go
  where
    go (Var x) = pretty x
    go (App a b) = f a <> f b
    go (Lam a b) = parens $ pretty a <> ":" <> f b
    go (Lit n) = pretty n
    go (Arith _ a b) = parens $ hsep ["_", f a, f b]
    go (Attr m) = ppMap pretty f m
    go (Acc a m) = f m <> "." <> pretty a
    go (ASTLit _) = "<<code>>"

ppVal :: Value -> Doc ann
ppVal (Fix (VInt n)) = pretty n
ppVal (Fix (VAttr attrs)) = ppMap pretty ppVal attrs
ppVal (Fix VClosure {}) = "<<closure>>"
