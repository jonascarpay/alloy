module Parse where

import Control.Monad
import Data.Functor.Identity
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Void
import Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as Lex

type Parser = ParsecT Void String Identity

lexeme :: Parser a -> Parser a
lexeme p = p <* space

lchunk :: String -> Parser ()
lchunk = void . lexeme . chunk

parens :: Parser p -> Parser p
parens p = lchunk "(" *> p <* lchunk ")"

braces :: Parser p -> Parser p
braces p = lchunk "{" *> p <* lchunk "}"

pName :: Parser Name
pName = lexeme $ do
  n <- takeWhile1P (Just "identifier") (`elem` ['a' .. 'z'])
  if S.member n keywords
    then fail "keyword"
    else pure n
  where
    keywords :: Set Name
    keywords = S.fromList ["return"]

pAttrs :: Parser Expr
pAttrs = braces $ Fix . Attr . M.fromList <$> many pField
  where
    pField = do
      n <- pName
      lchunk "="
      x <- pExpr
      lchunk ";"
      pure (n, x)

pExpr :: Parser Expr
pExpr = foldl1 (\a b -> Fix (App a b)) <$> some pTerm
  where
    pTerm = choice [parens pExpr, try pAttrs, pCode, pLam, pLit, pVar]
    pLam = do
      void $ lchunk "\\"
      xs <- some pName
      lchunk "."
      body <- pExpr
      pure $ foldr (\a b -> Fix (Lam a b)) body xs
    pLit = lexeme $ Fix . Lit <$> Lex.signed (pure ()) Lex.decimal
    pVar = Fix . Var <$> pName

pCode :: Parser Expr
pCode = Fix . ASTLit <$> pBlock pExpr

pBlock :: Parser f -> Parser (BlockF f)
pBlock pf =
  braces $
    BlockF <$> sepEndBy1 (pAst pf) (lchunk ";")

pAst :: Parser f -> Parser (ASTF f)
pAst pf = choice [pReturn]
  where
    pReturn = lchunk "return" *> (Return <$> pf)
