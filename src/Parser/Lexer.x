{
module Parser.Lexer
  ( Token(..)
  , SourcePos(..)
  , lexer
  ) where

import Parser.Token
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
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

  let			{ tok Let }
  in			{ tok In }
  with		{ tok With }
  inherit	{ tok Inherit }

  if			{ tok If }
  then		{ tok Then }
  else		{ tok Else }
  true		{ tok TTrue }
  false		{ tok TFalse }

  break			{ tok Break }
  continue	{ tok Continue }
  var				{ tok Var }

  "+"			{ tok Add }
  "-"			{ tok Sub }
  "*"			{ tok Mul }
  "/"			{ tok Div }
  "=="		{ tok Eq }
  "/="		{ tok Neq }
  "<"			{ tok Lt }
  ">"			{ tok Gt }
  "<="		{ tok Leq }
  ">="		{ tok Geq }
  "."			{ tok Dot }
  "<-"		{ tok LArr }
  "->"		{ tok RArr }

  "^"		{ tok Caret }
  "&"		{ tok Ampersand }

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

  $digit+											{ tok_num }
  $idHead $idTail*						{ tok_ident }
  \" ($stringChars # \")* \"	{ tok_string } -- TODO Use start codes to signal unterminated strings

{
lexer :: BS.ByteString -> Either SourcePos [(Token, SourcePos)]
lexer bs = go (AlexInput '\n' pos0 bs 0)
  where
    go input@(AlexInput _ pos bs b) =
      case alexScan input 0 of
        AlexEOF -> pure []
        AlexError (AlexInput _ pos _ _) -> Left $ pos
        AlexSkip rem _ -> go rem
        -- TODO There are places where it seems this length is in characters, not bytes
        -- investigate whether UTF breaks things
        AlexToken rem _ act ->
          let token = act (BS.take (aiOffset rem - b) bs)
           in ((token, pos) :) <$> go rem

posMove :: SourcePos -> Char -> SourcePos
posMove (SourcePos l c b) '\t' = SourcePos l (c + tab - (mod (c-1) tab)) (b+1) where tab = 8
posMove (SourcePos l _ b) '\n' = SourcePos (l+1) 1 (b+1)
posMove (SourcePos l c b) _ = SourcePos l (c+1) (b+1)

pos0 :: SourcePos
pos0 = SourcePos 1 1 0

data AlexInput = AlexInput
  { aiPrevChar :: {-# UNPACK #-} !Char
  , aiPos :: !SourcePos
  , aiBS :: !ByteString
  , aiOffset :: !Int
  }

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (AlexInput _ pos bs by) =
  case BS.uncons bs of
    Nothing -> Nothing
    Just (b, bs') ->
      let c = BSI.w2c b
       in Just (b, AlexInput c (posMove pos c) bs' (by+1))

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = aiPrevChar
}

-- vim: ft=text:noet
