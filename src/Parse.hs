module Parse (pToplevel) where

import Control.Monad
import Control.Monad.Combinators.Expr
import Data.Functor.Identity
import Data.Map qualified as M
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as S
import Data.Void
import Expr
import Text.Megaparsec
import Text.Megaparsec.Char hiding (space)
import Text.Megaparsec.Char.Lexer qualified as Lex

type Parser = ParsecT Void String Identity

space :: Parser ()
space = Lex.space space1 (Lex.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme p = p <* space

withPred :: (a -> Maybe String) -> Parser a -> Parser a
withPred hasError p = do
  o <- getOffset
  r <- p
  case hasError r of
    Nothing -> pure r
    Just err -> setOffset o >> fail err

symbol :: String -> Parser ()
symbol = void . Lex.symbol space

parens :: Parser p -> Parser p
parens = between (symbol "(") (symbol ")")

braces :: Parser p -> Parser p
braces = between (symbol "{") (symbol "}")

list :: Parser a -> Parser [a]
list = between (symbol "[") (symbol "]") . flip sepEndBy comma

pList :: Parser Expr
pList = List . Seq.fromList <$> list pExpr

pToplevel :: Parser Expr
pToplevel = space *> pExpr <* eof

pWord :: Parser String
pWord = lexeme . (<?> "identifier") $ do
  h <- letterChar
  t <- many (alphaNumChar <|> char '_')
  pure $ h : t

pName :: Parser Name
pName = withPred hasError pWord
  where
    hasError w
      | S.member w keywords = Just $ "Unexpected keyword \"" <> w <> "\""
      | otherwise = Nothing

keywords :: Set Name
keywords = S.fromList ["return", "true", "int", "bool", "void", "double", "false", "let", "in", "inherit", "break", "var"]

pAttrs :: Parser Expr
pAttrs = braces $ Attr . M.fromList <$> sepEndBy (pInherit <|> pAttrField) comma
  where
    pAttrField = do
      name <- pName
      symbol ":"
      expr <- pExpr
      pure (name, expr)

pInherit :: Parser (Name, Expr)
pInherit = do
  symbol "inherit"
  name <- pName
  pure (name, Var name)

-- first try to parse expr as lambda
-- first try to parse term as app
-- TODO is this a hack?
pExpr :: Parser Expr
pExpr = choice [pLet, pLam, pFunc, makeExprParser pTerm operatorTable]
  where
    -- TODO the try before pAttrs here is so allow it to parse the block expression if it fails

    operatorTable :: [[Operator Parser Expr]]
    operatorTable =
      [ [repeatedPostfix pFieldAcc],
        [InfixL (pure App)], -- function call
        [arith "*" Mul],
        [arith "+" Add, arith "-" Sub]
      ]
      where
        arith :: String -> ArithOp -> Operator Parser Expr
        arith sym op = InfixL (Arith op <$ symbol sym)
        repeatedPostfix :: Parser (Expr -> Expr) -> Operator Parser Expr
        repeatedPostfix = Postfix . fmap (foldr1 (.) . reverse) . some

pTerm :: Parser Expr
pTerm =
  choice
    [ parens pExpr,
      pList,
      try pAttrs,
      pBlock,
      Prim <$> pPrim,
      Var <$> pName
    ]

pFieldAcc :: Parser (Expr -> Expr)
pFieldAcc = symbol "." *> (Acc <$> pName)

pLet :: Parser Expr
pLet = do
  try $ symbol "let"
  fields <- many $ notFollowedBy (symbol "in") *> (pInherit <|> pField) <* semicolon
  symbol "in"
  Let fields <$> pExpr
  where
    pField = do
      n <- pName
      symbol "="
      x <- pExpr
      pure (n, x)

pPrim :: Parser Prim
pPrim = choice [PBool <$> try pBool, PInt <$> pInt, PType <$> pType]
  where
    pBool :: Parser Bool
    pBool = True <$ symbol "true" <|> False <$ symbol "false"
    pInt :: Parser Int
    pInt = lexeme Lex.decimal
    pType :: Parser Type
    pType =
      choice
        [ TInt <$ symbol "int",
          TDouble <$ symbol "double",
          TBool <$ symbol "bool",
          TVoid <$ symbol "void"
        ]

pFunc :: Parser Expr
pFunc = do
  args <- list pTypedName
  symbol "->"
  ret <- pTerm
  Func args ret <$> pExpr

pTypedName :: Parser (Name, Expr)
pTypedName = do
  name <- pName
  symbol ":"
  tp <- pExpr
  pure (name, tp)

pLam :: Parser Expr
pLam = do
  arg <- try $ pName <* symbol ":"
  Lam arg <$> pExpr

semicolon :: Parser ()
semicolon = symbol ";"

comma :: Parser ()
comma = symbol ","

pBlock :: Parser Expr
pBlock = braces $ BlockExpr . Block <$> many pStatement
  where
    pStatement :: Parser (Stmt Expr)
    pStatement = choice [pBreak, pDecl, try pAssign, pExprStmt] -- TODO try to avoid ambiguity with naked statement
    pBreak = Break <$> (symbol "break" *> pExpr <* semicolon)
    pExprStmt = ExprStmt <$> pExpr <* semicolon

pDecl :: Parser (Stmt Expr)
pDecl = do
  symbol "var"
  (name, tp) <- pTypedName
  symbol "="
  body <- pExpr
  semicolon
  pure $ Decl name tp body

pAssign :: Parser (Stmt Expr)
pAssign = do
  name <- pName
  symbol "="
  body <- pExpr
  semicolon
  pure $ Assign name body
