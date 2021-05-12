{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.Parser (parse, parseTest) where

import Control.Applicative.Combinators
import Data.ByteString (ByteString)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as V
import Parser.Lexer qualified as Lex
import Parser.Parsec qualified as P
import Parser.Token (SourcePos (..), Token, descrToken)
import Parser.Token qualified as T
import Parser.Tree

type Parser = P.Parser (Maybe Token) (Set String)

pExpr :: Parser PTree
pExpr =
  choice
    [ pLam,
      pLet,
      pApp,
      pList
    ]

pApp :: Parser PTree
pApp = do
  h <- pTerm
  foldl App h <$> many pTerm

pTerm :: Parser PTree
pTerm =
  choice
    [ pVar,
      Atom <$> pAtom
    ]

pVar :: Parser PTree
pVar = Var <$> pIdent

pIdent :: Parser Name
pIdent = expect "identifier" $ \case
  (T.Ident name) -> Just name
  _ -> Nothing

pList :: Parser PTree
pList = List . Seq.fromList <$> brackets (sepBy pExpr (token Lex.Comma))

pLam :: Parser PTree
pLam = do
  arg <- pIdent
  token T.Colon
  Lam arg <$> pExpr

pLet :: Parser PTree
pLet = do
  token T.Let
  binds <- sepEndBy pBinding (token T.Semicolon)
  token T.In
  Let binds <$> pExpr

pAtom :: Parser Atom
pAtom = expect "atom" $ \case
  Lex.Num n -> Just $ AInt n
  Lex.String s -> Just $ AString s
  Lex.TTrue -> Just $ ABool True
  Lex.TFalse -> Just $ ABool False
  _ -> Nothing

pBinding :: Parser Binding
pBinding = choice [pBind, pInherit, pInheritFrom]
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
  P.throwCC $ \throw ->
    let err = throw $ Set.singleton msg
     in P.token >>= maybe err (maybe err pure . f)

eof :: Parser ()
eof = P.throwCC $ \throw ->
  P.token >>= \case
    Nothing -> pure ()
    _ -> throw $ Set.singleton "end of input"

tokenize :: ByteString -> (Vector Token, Vector SourcePos)
tokenize bs = case Lex.lexer bs of
  Left err -> error (show err)
  Right lexed ->
    let (tokens, pos) = unzip lexed
     in (V.fromList tokens, V.fromList pos)

parse :: ByteString -> PTree
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

parseTest :: PTree
parseTest = parse "let x = [234, , true]; in"
