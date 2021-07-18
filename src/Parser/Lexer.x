{
module Parser.Lexer
  ( Token(..)
  , SourcePos(..)
  , lexer
  ) where

import Parser.Token
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import Data.ByteString (ByteString)
import Data.Word (Word8)
}

$digit = 0-9
$idHead = [a-z A-Z]
$idTail = [a-z A-Z 0-9 \_ \']
$stringChars = [$printable \n]

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
  true		{ tok TTrue }
  false		{ tok TFalse }

  return	{ tok Return }
  break		{ tok Break }
  continue	{ tok Continue }
  var		{ tok Var }

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

  $digit+			{ tok_num }
  $idHead $idTail*		{ Ident }
  \" ($stringChars # \")* \"	{ tok_string } -- TODO Use start codes to signal unterminated strings

{
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
  | otherwise = Just $
      let b = BS.unsafeHead bs
          (line', col') = if isNewLine b then (line + 1, 0) else (line, col + 1)
       in (b, AlexInput (BS.unsafeTail bs) line' col')

isNewLine :: Word8 -> Bool
isNewLine = (== 10)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = error "we don't use this"
}

-- vim: ft=text:noet
