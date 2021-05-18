{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.Parser (parse) where

import Control.Applicative
import Control.Applicative.Combinators
import Control.Monad.Combinators.Expr
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Foldable (foldl', toList)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as V
import Expr
import Parser.Lexer qualified as Lex
import Parser.Parsec qualified as P
import Parser.Token (SourcePos (..), Token, descrToken)
import Parser.Token qualified as T

type Parser = P.Parser (Maybe Token) (Set String)

-- TODO some kind of slicing?
-- may be pass what the expression should end with as an argument?
-- the issue is this:
-- in this parser, pFunc has to go before pBinExpr, becuase in the string
--   [] -> int
-- it succeeds on parsing the [] as a list, so it wont't try it as function anymore
-- it'll interpret it as an expression followed by garbage
pExpr :: Parser Expr
pExpr =
  choice
    [ pLam,
      pLet,
      pIf,
      pWith,
      pFunc,
      pBinExpr
    ]

pBinExpr :: Parser Expr
pBinExpr =
  makeExprParser
    pTerm
    [ [InfixL (pure App)],
      [InfixL (BinExpr Concat <$ token T.Cat)],
      [arith T.Mul Mul, arith T.Div Div],
      [arith T.Add Add, arith T.Sub Sub],
      [comp T.Lt Lt, comp T.Gt Gt, comp T.Leq Leq, comp T.Geq Geq],
      [comp T.Eq Eq, comp T.Neq Neq]
    ]
  where
    {-# INLINE arith #-}
    arith :: Token -> ArithOp -> Operator Parser Expr
    arith tk op = InfixL (BinExpr (ArithOp op) <$ token tk)
    {-# INLINE comp #-}
    comp :: Token -> CompOp -> Operator Parser Expr
    comp tk op = InfixN (BinExpr (CompOp op) <$ token tk)

pIf :: Parser Expr
pIf =
  liftA3
    Cond
    (token T.If *> pExpr)
    (token T.Then *> pExpr)
    (token T.Else *> pExpr)

pWith :: Parser Expr
pWith =
  liftA2
    With
    (token T.With *> termSemicolon pExpr)
    pExpr

pAttr :: Parser Expr
pAttr = braces $ Attr <$> sepEndBy pBinding' (token T.Comma)

pFunc :: Parser Expr
pFunc = do
  args <-
    brackets $
      sepBy
        (liftA2 (,) pIdent (token T.Colon *> pExpr))
        (token T.Comma)
  -- TODo proper right arrow operator
  token T.Sub
  token T.Gt
  ret <- pTerm
  Func args ret <$> pExpr

pTerm :: Parser Expr
pTerm = do
  val <-
    choice
      [ Prim <$> pAtom,
        pList,
        pAttr,
        BlockExpr <$> pBlock,
        parens pExpr,
        pVar
      ]
  accessors <- many (token T.Dot *> pIdent)
  pure $ foldl' (flip Acc) val accessors

pBlock :: Parser (Block Name Name (Maybe Expr) Expr)
pBlock = do
  label <- optional $ pIdent <* token T.At
  braces $ do
    stmts <- many pStatement
    terminator <- optional pExpr
    let exprStmt = Break Nothing . Just
    pure $ Block label (stmts <> toList (exprStmt <$> terminator)) Nothing

termSemicolon :: Parser a -> Parser a
termSemicolon = (<* token T.Semicolon)

pStatement :: Parser (Stmt Name Name (Maybe Expr) Expr)
pStatement = choice (fmap termSemicolon [pReturn, pBreak, pDecl, ExprStmt <$> pExpr, pAssign, pContinue])
  where
    pReturn = token T.Return *> (Return <$> pExpr)
    pMLabel = optional $ token T.At *> pIdent
    pBreak = token T.Break *> liftA2 Break pMLabel (optional pExpr)
    pContinue = token T.Continue *> (Continue <$> pMLabel)
    pAssign = liftA2 Assign pIdent (token T.Assign *> pExpr)
    pDecl =
      token T.Var
        *> liftA3
          Decl
          pIdent
          (optional $ token T.Colon *> pExpr)
          (token T.Assign *> pExpr)

pVar :: Parser Expr
pVar = Var <$> pIdent

pIdent :: Parser Name
pIdent = expect "identifier" $ \case
  (T.Ident name) -> Just name
  _ -> Nothing

pList :: Parser Expr
pList = List . Seq.fromList <$> brackets (sepBy pExpr (token T.Comma))

pLam :: Parser Expr
pLam = do
  arg <- pIdent
  token T.Colon
  Lam arg <$> pExpr

pLet :: Parser Expr
pLet = do
  token T.Let
  binds <- many pBinding
  token T.In
  Let binds <$> pExpr

pAtom :: Parser Prim
pAtom = expect "atom" $ \case
  T.Num n -> Just $ PInt n
  T.String s -> Just $ PString s
  T.TTrue -> Just $ PBool True
  T.TFalse -> Just $ PBool False
  _ -> Nothing

-- TODO uses : and ,
-- used until we move attr sets back to = and ;
pBinding' :: Parser Binding
pBinding' = choice [pBind, pInherit, pInheritFrom]
  where
    pBind = do
      name <- pIdent
      args <- many pIdent
      token T.Colon
      Binding name args <$> pExpr
    pInherit = token T.Inherit *> (Inherit <$> some pIdent)
    pInheritFrom = do
      token T.Inherit
      from <- parens pExpr
      names <- many pIdent
      pure $ InheritFrom from names

pBinding :: Parser Binding
pBinding = choice (fmap termSemicolon [pBind, pInherit, pInheritFrom])
  where
    pBind = do
      name <- pIdent
      args <- many pIdent
      token T.Assign
      Binding name args <$> pExpr
    pInherit = token T.Inherit *> (Inherit <$> many pIdent)
    pInheritFrom = do
      token T.Inherit
      from <- parens pExpr
      names <- many pIdent
      pure $ InheritFrom from names

parens :: Parser a -> Parser a
parens p = token T.LParen *> p <* token T.RParen

braces :: Parser a -> Parser a
braces p = token T.LBrace *> p <* token T.RBrace

brackets :: Parser a -> Parser a
brackets p = token T.LBrack *> p <* token T.RBrack

token :: Token -> Parser ()
token tk = expect (T.descrToken tk) $
  \tk' -> if tk == tk' then Just () else Nothing

expect :: String -> (Token -> Maybe a) -> Parser a
expect msg f =
  P.throwAt $ \throw ->
    let err = throw $ Set.singleton msg
     in P.token >>= maybe err (maybe err pure . f)

eof :: Parser ()
eof = P.throwAt $ \throw ->
  P.token >>= \case
    Nothing -> pure ()
    _ -> throw $ Set.singleton "eof"

tokenize :: ByteString -> (Vector Token, Vector SourcePos)
tokenize bs = case Lex.lexer bs of
  Left err -> error (show err)
  Right lexed ->
    let (tokens, pos) = unzip lexed
     in (V.fromList tokens, V.fromList pos)

parse :: ByteString -> Either String Expr
parse bs = first formatError $ P.runParser (tokens V.!?) (pExpr <* eof)
  where
    formatError (errIndex, expected) =
      case tokens V.!? errIndex of
        Nothing -> "unexpected end of file, expected one of " <> unwords (Set.toList expected)
        Just tk ->
          let errPos = pos V.! errIndex
           in unwords ["unexpected", descrToken tk, "at", show errPos, ", expected one of:", unwords (Set.toList expected)]
    (tokens, pos) = tokenize bs
