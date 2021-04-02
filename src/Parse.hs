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

keyword :: String -> Parser ()
keyword kw = try $ do
  w <- pWord
  if w == kw
    then pure ()
    else fail $ "expected " <> kw

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
keywords = S.fromList ["return", "break", "continue", "true", "false", "let", "in", "var", "with", "inherit", "if", "then", "else"]

pAttrs :: Parser Expr
pAttrs = braces $ Attr . M.fromList <$> pAttrList
  where
    -- inherit statements can have multiple import, but field declarations can
    -- only have one, so we need to do some marshalling
    pAttrList :: Parser [(Name, Expr)]
    pAttrList = concat <$> sepEndBy (try pInheritFrom <|> pInherit <|> (pure <$> pAttrField)) comma
    pAttrField = do
      name <- pName
      symbol ":"
      expr <- pExpr
      pure (name, expr)

pInheritFrom :: Parser [(Name, Expr)]
pInheritFrom = do
  keyword "inherit"
  attr <- parens pExpr
  names <- some pName
  pure $ (\name -> (name, Acc name attr)) <$> names

pInherit :: Parser [(Name, Expr)]
pInherit = do
  keyword "inherit"
  names <- some pName
  pure $ (\name -> (name, Var name)) <$> names

pExpr :: Parser Expr
pExpr =
  choice
    [ pLet,
      pCond,
      pLam,
      try pFunc,
      uncurry With <$> pWith,
      makeExprParser (try pTerm) operatorTable
      -- TODO The `try` here is to make sure App doesn't try consuming too much, but it is super dangerous since it might lead to blow-up
      -- ideally, we just move accessors and app into a higher-level thing
    ]
  where
    operatorTable :: [[Operator Parser Expr]]
    operatorTable =
      [ [repeatedPostfix pFieldAcc],
        [InfixL (pure App)],
        [arith "*" Mul],
        [arith "+" Add, arith "-" Sub],
        [bool "==" Eq, bool "/=" Neq, bool "<" Lt, bool ">" Gt, bool "<=" Leq, bool ">=" Geq]
      ]
      where
        arith :: String -> ArithOp -> Operator Parser Expr
        arith sym op = InfixL (BinExpr (ArithOp op) <$ symbol sym)
        bool :: String -> CompOp -> Operator Parser Expr
        bool sym op = InfixN (BinExpr (CompOp op) <$ symbol sym)
        repeatedPostfix :: Parser (Expr -> Expr) -> Operator Parser Expr
        repeatedPostfix = Postfix . fmap (foldr1 (.) . reverse) . some

pTerm :: Parser Expr
pTerm =
  choice
    [ parens pExpr,
      pList,
      try pAttrs, -- TODO hopefully unnecessary
      BlockExpr <$> pBlock,
      Prim <$> pPrim,
      Var <$> pName
    ]
    <?> "term"

pFieldAcc :: Parser (Expr -> Expr)
pFieldAcc = symbol "." *> (Acc <$> pName)

pLet :: Parser Expr
pLet = do
  keyword "let"
  fields <- many $ notFollowedBy (keyword "in") *> pFields <* semicolon
  keyword "in"
  Let (concat fields) <$> pExpr
  where
    pFields :: Parser [(Name, Expr)]
    pFields = pInheritFrom <|> (pure <$> pField)
    pField = do
      n <- pName
      symbol "="
      x <- pExpr
      pure (n, x)

pPrim :: Parser Prim
pPrim = choice [PBool <$> try pBool, PInt <$> pInt, PString <$> pString] <?> "primitive"
  where
    pBool :: Parser Bool
    pBool = True <$ keyword "true" <|> False <$ keyword "false"
    pInt :: Parser Int
    pInt = lexeme Lex.decimal
    pString :: Parser String
    pString = lexeme (char '\"' *> manyTill Lex.charLiteral (char '\"')) <?> "string"

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

pCond :: Parser Expr
pCond = do
  keyword "if"
  cond <- pExpr
  keyword "then"
  eTrue <- pExpr
  keyword "else"
  Cond cond eTrue <$> pExpr

pBlock :: Parser (Block Name Name (Maybe Expr) Expr)
pBlock = do
  mname <- try $ optional $ pName <* symbol "@"
  braces $ do
    stmts <- many (try pStatement)
    retExpr <- optional pExpr
    pure $ Block mname (stmts <> maybe [] (pure . Break Nothing . Just) retExpr) Nothing
  where
    -- TODO `try` to avoid ambiguity with naked statement, remove
    -- TODO `try` for decl shouldn't be necessary?
    pStatement :: Parser (Stmt Name Name (Maybe Expr) Expr)
    pStatement = choice [pReturn, pBreak, pContinue, try pDecl, try pAssign, pExprStmt]
    pReturn = Return <$> (keyword "return" *> pExpr <* semicolon)
    pBreak = do
      keyword "break"
      name <- optional $ symbol "@" *> pName
      expr <- optional pExpr
      semicolon
      pure $ Break name expr
    pContinue = do
      keyword "continue"
      name <- optional $ symbol "@" *> pName
      semicolon
      pure $ Continue name
    pExprStmt = ExprStmt <$> pExpr <* semicolon

pWith :: Parser (Expr, Expr)
pWith = do
  keyword "with"
  bind <- pExpr
  semicolon
  body <- pExpr
  pure (bind, body)

pDecl :: Parser (Stmt Name lbl (Maybe Expr) Expr)
pDecl = do
  symbol "var"
  name <- pName
  typ <- optional $ do
    symbol ":"
    pExpr
  symbol "="
  body <- pExpr
  semicolon
  pure $ Decl name typ body

pAssign :: Parser (Stmt Name lbl typ Expr)
pAssign = do
  name <- pName
  symbol "="
  body <- pExpr
  semicolon
  pure $ Assign name body
