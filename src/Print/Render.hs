{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module Print.Render (render) where

import Data.Bifoldable
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.Foldable
import Data.List (intersperse)
import Expr (Prim (..))
import Print.Bifree
import Print.Doc
import Print.Printer as P

data Style = Single | Multi
  deriving (Eq, Show, Ord)

type AnnDoc = Bicofree StatementF DocF Style

render :: Doc -> ByteString
render = runPrinter . cataBicofree renderStmt renderDoc . annotate

renderDoc :: Style -> DocF Printer Printer -> Printer
renderDoc Single (Parens doc) = "(" <> doc <> ")"
renderDoc Multi (Parens doc) = newline <> "( " <> align doc <> newline <> ")"
renderDoc _ (Symbol sym) = emit sym
renderDoc _ (Prim prim) = renderPrim prim
renderDoc Single (List doc) = "[" <> fold (intersperse ", " doc) <> "]"
renderDoc Multi (List doc) = newline <> align ("[ " <> fold (intersperse (newline <> ", ") doc)) <> newline <> "]"
renderDoc _ (Deref' d) = "*" <> d
renderDoc Single (Operator op l r) = l <> space <> emit op <> space <> r
renderDoc Multi (Operator op l r) = l <> newline <> emit op <> space <> r
renderDoc _ (Sel h n) = h <> "." <> n
renderDoc sty (Call' fn args) = emit fn <> renderDoc sty (List args)
renderDoc Single (Prog lbl blk) = emit lbl <> "@{" <> blk <> "}"
renderDoc Multi (Prog lbl blk) = emit lbl <> "@{" <> newline <> indent blk <> newline <> "}"
renderDoc Single (Cond c t f) = fold $ intersperse space ["if", c, "then", t, "else", f]
renderDoc Multi (Cond c t f) = "if" <> space <> c <> newline <> "then" <> space <> t <> newline <> "else" <> space <> f

renderStmt :: Style -> StatementF Printer Printer -> Printer
renderStmt _ (SDecl sym typ rhs k) =
  "var" <> space <> emit sym <> ":" <> space <> typ <> space <> "=" <> space <> rhs <> ";" <> newline <> k
renderStmt _ (SAssign lhs rhs k) = lhs <> space <> "=" <> space <> rhs <> ";" <> newline <> k
renderStmt _ (SBreak sym expr) = "break" <> space <> emit sym <> space <> expr <> ";"
renderStmt _ (SContinue sym) = "continue" <> space <> emit sym <> ";"
renderStmt _ (SExpr doc Nothing) = doc
renderStmt _ (SExpr doc (Just k)) = doc <> ";" <> newline <> k

renderPrim :: Prim -> Printer
renderPrim (PInt n) = emits n
renderPrim (PDouble n) = emits n
renderPrim (PBool n) = bool "false" "true" n
renderPrim PVoid = "void"

annotate :: Doc -> AnnDoc
annotate = toBicofree fStmt fExpr
  where
    fExpr :: DocF Style Style -> Style
    fExpr doc = bifoldr max max Single doc
    fStmt :: StatementF Style Style -> Style
    fStmt SDecl {} = Multi
    fStmt SAssign {} = Multi
    fStmt SContinue {} = Single
    fStmt (SBreak _ expr) = expr
    fStmt (SExpr Single Nothing) = Single
    fStmt SExpr {} = Multi
