{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.Parser (parse) where

import Control.Applicative
import Control.Applicative.Combinators
import Control.Monad.Combinators.Expr
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Short qualified as BSS
import Data.Foldable (foldl')
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
    (token T.With *> pExpr <* semicolon)
    pExpr

-- TODO think about unambiguous attr parsing
-- Unforunately, [] means you can't tell the difference between empty attrs and the empty list
pAttr :: Parser Expr
pAttr = braces $ Attr <$> many pBinding

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
        pString,
        pBlock,
        parens pExpr,
        pVar
      ]
  foldl' Acc val <$> many (token T.Dot *> pIdent)

pString :: Parser Expr
pString = expect "string" $ \case
  T.String str -> Just (String str)
  _ -> Nothing

pBlock :: Parser Expr
pBlock = do
  label <- optional $ pIdent <* token T.At
  braces $ Run label <$> pStmts
  where
    pStmts :: Parser ProgE
    pStmts = choice [pDecl, pAssign, pExprStatement, pTerminator]

    pTerminator =
      choice
        [ pBreak,
          pContinue,
          BreakE Nothing . Just <$> pExpr,
          pure $ BreakE Nothing Nothing
        ]

    pBreak = do
      token T.Break
      lbl <- optional (token T.At *> pTerm)
      expr <- optional pExpr
      semicolon
      pure $ BreakE lbl expr

    pContinue = do
      token T.Continue
      lbl <- optional (token T.At *> pTerm)
      semicolon
      pure $ ContinueE lbl

    pAssign = do
      lhs <- pExpr
      token T.Assign
      rhs <- pExpr
      semicolon
      AssignE lhs rhs <$> pStmts

    pExprStatement = do
      expr <- pExpr
      semicolon
      ExprE expr <$> pStmts

    pDecl = do
      token T.Var
      lhs <- pIdent
      token T.Colon
      typ <- pExpr
      token T.Assign
      rhs <- pExpr
      semicolon
      DeclE lhs typ rhs <$> pStmts

semicolon :: Parser ()
semicolon = token T.Semicolon

pVar :: Parser Expr
pVar = Var <$> pIdent

pIdent :: Parser Name
pIdent = expect "identifier" $ \case
  (T.Ident name) -> Just $ BSS.toShort name
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
  T.TTrue -> Just $ PBool True
  T.TFalse -> Just $ PBool False
  _ -> Nothing

pBinding :: Parser Binding
pBinding = choice (fmap (<* semicolon) [pBind, pInherit, pInheritFrom])
  where
    pBind = do
      name <- pIdent
      args <- many pIdent
      token T.Assign
      Simple name args <$> pExpr
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

tokenize :: ByteString -> Either SourcePos (Vector Token, Vector SourcePos)
tokenize bs = case Lex.lexer bs of
  Left err -> Left err
  Right lexed ->
    Right $
      let (tokens, pos) = unzip lexed
       in (V.fromList tokens, V.fromList pos)

formatError :: Vector Token -> Vector SourcePos -> (Int, Set String) -> String
formatError tokens sps (errIndex, expected) =
  case tokens V.!? errIndex of
    Nothing -> "unexpected end of file, expected one of " <> unwords (Set.toList expected)
    Just tk ->
      let errPos = sps V.! errIndex
       in unwords ["unexpected", descrToken tk, "at", show errPos, ", expected one of:", unwords (Set.toList expected)]

parse :: ByteString -> Either String Expr
parse bs = do
  (tokens, sps) <- first (\p -> "Lexical error at " <> show p) (tokenize bs)
  first (formatError tokens sps) $ P.runParser (tokens V.!?) (pExpr <* eof)
