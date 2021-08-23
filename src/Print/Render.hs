{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module Print.Render (render) where

import Data.Bifoldable
import Data.ByteString (ByteString)
import Data.Foldable
import Data.List (intersperse)
import Data.Map qualified as M
import Print.Bifree
import Print.Doc
import Print.Printer as P

data Style = Single | Multi
  deriving (Eq, Show, Ord)

type AnnDoc = Bicofree StatementF DocF Style

render :: Doc -> ByteString
render = runPrinter . cataBicofree renderStmt renderDoc . annotate

space :: Printer
space = emit " "

renderDoc :: Style -> DocF Printer Printer -> Printer
renderDoc Single (Parens doc) = "(" <> doc <> ")"
renderDoc Multi (Parens doc) = "(" <> indent doc <> newline <> ")"
renderDoc _ (Symbol sym) = emit sym
renderDoc Single (List doc) = "[" <> fold (intersperse ", " doc) <> "]"
renderDoc Multi (List doc) =
  indent $
    mconcat
      [ "[",
        mconcat $ intersperse (newline <> ",") (indent <$> doc),
        newline,
        "]"
      ]
renderDoc _ (Deref' d) = "*" <> d
renderDoc Single (Operator op l r) = l <> space <> op <> space <> r
renderDoc Multi (Operator op l r) = l <> newline <> op <> space <> r
renderDoc _ (Module deps body) | null deps = body
renderDoc _ (Module deps body) =
  indent $
    mconcat
      [ "Module dependencies:",
        indent . mconcat . intersperse newline . flip fmap (M.toList deps) $ \(str, p) ->
          emit str <> " = " <> p <> ";",
        newline,
        "Module body:",
        indent body
      ]
renderDoc _ (Func args ret body) = renderDoc Single (List $ (\(arg, typ) -> arg <> ": " <> typ) <$> args) <> " -> " <> ret <> space <> body
renderDoc _ (Sel h n) = h <> "." <> n
renderDoc sty (Call' fn args) = fn <> renderDoc sty (List args)
renderDoc Single (Prog lbl blk) = lbl <> "@{ " <> blk <> " }"
renderDoc Multi (Prog lbl blk) = lbl <> "@{" <> newline <> indent blk <> newline <> "}"
renderDoc Single (Attrs' m) = "{ " <> foldMap (\(nm, d) -> emitSbs nm <> " = " <> d <> "; ") (M.toList m) <> "}"
renderDoc Multi (Attrs' m) =
  let render (nm, d) = emitSbs nm <> " = " <> d <> ";"
   in mconcat
        [ "{",
          indent $ mconcat $ intersperse newline $ fmap render (M.toList m),
          newline,
          "}"
        ]
renderDoc Single (Cond c t f) = fold $ intersperse space ["if", c, "then", t, "else", f]
renderDoc Multi (Cond c t f) = "if" <> space <> c <> newline <> indent ("then" <> space <> t) <> newline <> indent ("else" <> space <> f)

renderStmt :: Style -> StatementF Printer Printer -> Printer
renderStmt _ (SDecl sym typ rhs k) =
  "var" <> space <> emit sym <> ":" <> space <> typ <> space <> "=" <> space <> rhs <> ";" <> newline <> k
renderStmt _ (SAssign lhs rhs k) = lhs <> space <> "=" <> space <> rhs <> ";" <> newline <> k
renderStmt _ (SBreak sym expr) = "break" <> space <> sym <> space <> expr <> ";"
renderStmt _ (SContinue sym) = "continue" <> space <> sym <> ";"
renderStmt _ (SExpr doc Nothing) = doc
renderStmt _ (SExpr doc (Just k)) = doc <> ";" <> newline <> k

annotate :: Doc -> AnnDoc
annotate = toBicofree fStmt fExpr
  where
    fExpr :: DocF Style Style -> Style
    fExpr (Module deps _) | not (null deps) = Multi
    fExpr doc = bifoldr max max Single doc
    fStmt :: StatementF Style Style -> Style
    fStmt SDecl {} = Multi
    fStmt SAssign {} = Multi
    fStmt SContinue {} = Single
    fStmt (SBreak _ expr) = expr
    fStmt (SExpr Single Nothing) = Single
    fStmt SExpr {} = Multi