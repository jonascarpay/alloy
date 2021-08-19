{-# LANGUAGE DeriveTraversable #-}

module Print.Render where

import Data.Bifoldable
import Print.Bifree
import Print.Doc

data Style = Single | Multi
  deriving (Eq, Show, Ord)

type AnnDoc = Bicofree StatementF DocF Style

type AnnStatement = Bicofree DocF StatementF Style

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
