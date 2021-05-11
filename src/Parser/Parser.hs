module Parser.Parser where

import Data.ByteString (ByteString)
import Data.Set (Set, singleton)
import Parser.Lexer (Token)
import Parser.Lexer qualified as Lex
import Parser.Parsec qualified as P
import Parser.Tree

type Parser = P.Parser (Token, Int) (Set String)

expect :: Lex.Token -> Parser ()
expect tk =
  P.throwCC $ \throw ->
    P.token >>= \(r, _) ->
      if tk == r
        then pure ()
        else throw $ singleton $ showToken tk

data ParseError = ParseError
  { errPos :: Int,
    errExpected :: Set String
  }

parse :: ByteString -> Either ParseError (PTree Int)
parse = undefined

showToken :: Token -> String
showToken = undefined
