module Parse where

import Control.Monad
import Data.Functor.Identity
import Data.Void
import Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as Lex

type Parser = ParsecT Void String Identity

pExpr :: Parser Expr
pExpr = foldl1 (\a b -> Fix (App a b)) <$> some pTerm
  where
    pTerm = choice [parens pExpr, pLam, pLit, pVar]
    lexeme :: Parser a -> Parser a
    lexeme p = p <* space
    lchunk = void . lexeme . chunk
    pName :: Parser String
    pName = lexeme $ takeWhile1P (Just "identifier") (`elem` ['a' .. 'z'])
    parens :: Parser p -> Parser p
    parens p = lchunk "(" *> p <* lchunk ")"
    pLam = do
      void $ lchunk "\\"
      xs <- some pName
      lchunk "."
      body <- pExpr
      pure $ foldr (\a b -> Fix (Lam a b)) body xs
    pLit = lexeme $ Fix . Lit <$> Lex.signed (pure ()) Lex.decimal
    pVar = Fix . Var <$> pName
