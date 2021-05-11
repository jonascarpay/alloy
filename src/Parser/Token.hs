module Parser.Token where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BS

data SourcePos = SourcePos
  { lineNumber :: Int,
    columnNumber :: Int
  }
  deriving (Eq, Show)

data Token
  = -- Compile time keywords
    Let
  | In
  | With
  | Inherit
  | -- Common keywords
    If
  | Then
  | Else
  | TTrue
  | TFalse
  | -- Runtime keywords
    Return
  | Break
  | Continue
  | Var
  | -- Operators
    Add
  | Sub
  | Mul
  | Div
  | Eq
  | Neq
  | Lt
  | Gt
  | Leq
  | Geq
  | Cat
  | Dot
  | -- Punctuation
    Semicolon
  | Colon
  | Comma
  | Assign
  | At
  | -- Delimiters
    LParen
  | RParen
  | LBrace
  | RBrace
  | LBrack
  | RBrack
  | Ident ByteString
  | String ByteString
  | Num Int
  deriving (Eq, Show)

tok :: Token -> ByteString -> Token
tok = const

tok_num :: ByteString -> Token
tok_num bs = Num (parse bs)
  where
    parse = BS.foldl' f 0
    f acc b = acc * 10 + fromIntegral b - 48

tok_string :: ByteString -> Token
tok_string = String . BS.unsafeInit . BS.unsafeTail

showToken :: Token -> String
showToken = undefined
