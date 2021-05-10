{
module Parser.Lexer
  ( Token(..)
  , SourcePos(..)
  , lexer
  ) where

import Prelude hiding (True, False)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import Data.ByteString (ByteString)
import Data.Word (Word8)
}

$digit = 0-9
$symbolHead = [a-z A-Z]
$symbolTail = [a-z A-Z 0-9 \_ \']

tokens :-

  $white+	;
  "#".*		;

  let		{ tok Let }
  in		{ tok In }
  with		{ tok With }
  inherit	{ tok Inherit }

  if		{ tok If }
  then		{ tok Then }
  else		{ tok Else }
  true		{ tok True }
  false		{ tok False }

  return	{ tok Return }
  break		{ tok Break }
  continue	{ tok Continue }
  var		{ tok Var }

  -- TODO can these be written with single quotes to match with the Parser module?
  "+"		{ tok Add }
  "-"		{ tok Sub }
  "*"		{ tok Mul }
  "/"		{ tok Div }
  "=="		{ tok Eq }
  "/="		{ tok Neq }
  "<"		{ tok Lt }
  ">"		{ tok Gt }
  "<="		{ tok Leq }
  ">="		{ tok Geq }
  "++"		{ tok Cat }
  "."		{ tok Dot }

  ";"		{ tok Semicolon }
  ":"		{ tok Colon }
  ","		{ tok Comma }
  "="		{ tok Assign }
  "@"		{ tok At }

  "("		{ tok LParen }
  ")"		{ tok RParen }
  "["		{ tok LBrack }
  "]"		{ tok RBrack }
  "{"		{ tok LBrace }
  "}"		{ tok RBrace }

  $digit+				{ tok_read Num }
  $symbolHead $symbolTail*		{ tok_read Symbol }
  \" (~ \")* \"				{ tok_string } -- TODO Use start codes to signal unterminated strings

{
data Token

  -- Compile time keywords
  = Let
  | In
  | With
  | Inherit

  -- Common keywords
  | If
  | Then
  | Else
  | True
  | False

  -- Runtime keywords
  | Return
  | Break
  | Continue
  | Var

  -- Operators
  | Add
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

  -- Punctuation
  | Semicolon
  | Colon
  | Comma
  | Assign
  | At

  -- Delimiters
  | LParen
  | RParen
  | LBrace
  | RBrace
  | LBrack
  | RBrack

  | Symbol ByteString
  | String ByteString
  | Num ByteString

  deriving (Eq,Show)

tok :: Token -> ByteString -> Token
tok = const

tok_read :: (ByteString -> Token) -> ByteString -> Token
tok_read = id

tok_string :: ByteString -> Token
tok_string = String . BS.unsafeInit . BS.unsafeTail

data SourcePos = SourcePos
  { lineNumber :: Int
  , columnNumber :: Int
  }
  deriving (Eq,Show)

lexer :: BS.ByteString -> Either SourcePos [(Token, SourcePos)]
lexer bs = go (AlexInput bs 0 0)
  where
    go input@(AlexInput bs line col) =
      case alexScan input 0 of
        AlexEOF -> pure []
        AlexError (AlexInput _ line col) -> Left $ SourcePos line col
        AlexSkip rem _ -> go rem
        -- TODO There are places where it seems this length is in characters, not bytes
        -- investigate whether UTF breaks things
        AlexToken rem len act ->
          let token = act (BS.unsafeTake len bs)
           in ((token, SourcePos line col) :) <$> go rem

data AlexInput = AlexInput
  { aiBS :: ByteString
  , aiLine :: Int
  , aiColumn :: Int
  }

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (AlexInput bs line col)
  | BS.null bs = Nothing
  | otherwise =
      let b = BS.unsafeHead bs
          nl = isNewLine b
          line' = if nl then line + 1 else line
          col'  = if nl then 0        else col + 1
       in Just $ (b, AlexInput (BS.unsafeTail bs) line' col')

isNewLine :: Word8 -> Bool
isNewLine = (== 10)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = error "we don't use this"

}

-- vim: ft=text:noet
