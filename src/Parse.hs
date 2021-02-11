module Parse (pToplevel) where

import Control.Monad
import Control.Monad.Combinators.Expr
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

symbol :: String -> Parser ()
symbol = void . Lex.symbol space

parens :: Parser p -> Parser p
parens = between (symbol "(") (symbol ")")

braces :: Parser p -> Parser p
braces = between (symbol "{") (symbol "}")

pToplevel :: Parser Expr
pToplevel = pExpr <* eof

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
      symbol "="
      x <- pExpr
      symbol ";"
      pure (n, x)

-- first try to parse expr as lambda
-- first try to parse term as app
-- TODO is this a hack?
pExpr :: Parser Expr
pExpr = pLam <|> makeExprParser pTerm operatorTable
  where
    pTerm1 = choice [parens pExpr, pAttrs, pVar, pLit]
    pTerm = foldl1 (\a b -> Fix $ App a b) <$> some pTerm1 -- TODO foldl

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [repeatedPostfix pFieldAcc],
    [arith "*" Mul],
    [arith "+" Add, arith "-" Sub]
  ]
  where
    arith :: String -> ArithOp -> Operator Parser Expr
    arith sym op = InfixL ((\l r -> Fix $ Arith op l r) <$ symbol sym)
    repeatedPostfix :: Parser (Expr -> Expr) -> Operator Parser Expr
    repeatedPostfix = Postfix . fmap (foldr1 (.) . reverse) . some

pFieldAcc :: Parser (Expr -> Expr)
pFieldAcc = do
  symbol "."
  field <- pName
  pure $ Fix . Acc field

pLit :: Parser Expr
pLit = lexeme $ Fix . Lit <$> Lex.signed (pure ()) Lex.decimal

pVar :: Parser Expr
pVar = Fix . Var <$> pName

pLam :: Parser Expr
pLam = do
  arg <- try $ pName <* symbol ":"
  Fix . Lam arg <$> pExpr
