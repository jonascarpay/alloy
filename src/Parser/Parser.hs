{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.Parser (parse, parseTest) where

import Control.Applicative
import Control.Applicative.Combinators
import Control.Monad.Combinators.Expr
import Data.ByteString (ByteString)
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

pExpr :: Parser Expr
pExpr =
  choice
    [ pLam,
      pLet,
      pIf,
      pWith,
      pBinExpr
    ]

pBinExpr :: Parser Expr
pBinExpr =
  makeExprParser
    pTerm
    [ [repeatedPostfix (Acc <$> (token T.Dot *> pIdent))],
      [InfixL (pure App)],
      [InfixL (BinExpr Concat <$ token T.Cat)],
      [arith T.Mul Mul, arith T.Div Div],
      [arith T.Add Add, arith T.Sub Sub],
      [comp T.Lt Lt, comp T.Gt Gt, comp T.Leq Leq, comp T.Geq Geq],
      [comp T.Eq Eq, comp T.Neq Neq]
    ]
  where
    repeatedPostfix :: Parser (Expr -> Expr) -> Operator Parser Expr
    repeatedPostfix = Postfix . fmap (foldr1 (.) . reverse) . some
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
    (token T.With *> pExpr)
    (token T.Semicolon *> pExpr)

pAttr :: Parser Expr
pAttr =
  braces $
    Attr <$> sepEndBy pBinding' (token T.Comma)

pTerm :: Parser Expr
pTerm =
  choice
    [ pVar,
      Prim <$> pAtom,
      parens pExpr,
      pList,
      pAttr
    ]

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

-- TODO totdat we de = fixen
pBinding' :: Parser Binding
pBinding' = choice [pBind, pInherit, pInheritFrom]
  where
    sep = token T.Comma
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
pBinding = choice [pBind <* sep, pInherit <* sep, pInheritFrom <* sep]
  where
    sep = token T.Semicolon
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
  P.throwCC $ \throw ->
    let err = throw $ Set.singleton msg
     in P.token >>= maybe err (maybe err pure . f)

eof :: Parser ()
eof = P.throwCC $ \throw ->
  P.token >>= \case
    Nothing -> pure ()
    _ -> throw $ Set.singleton "eof"

tokenize :: ByteString -> (Vector Token, Vector SourcePos)
tokenize bs = case Lex.lexer bs of
  Left err -> error (show err)
  Right lexed ->
    let (tokens, pos) = unzip lexed
     in (V.fromList tokens, V.fromList pos)

parse :: ByteString -> Expr
parse bs = case P.runParser (tokens V.!?) (pExpr <* eof) of
  Right a -> a
  Left (errIndex, expected) ->
    error $
      case tokens V.!? errIndex of
        Nothing -> "unexpected end of file, expected one of " <> unwords (Set.toList expected)
        Just tk ->
          let errPos = pos V.! errIndex
           in unwords ["unexpected", descrToken tk, "at", show errPos, ", expected one of:", unwords (Set.toList expected)]
  where
    (tokens, pos) = tokenize bs

parseTest :: Expr
parseTest = parse "let inherit (true) asd sdf; in b"
