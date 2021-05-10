{
module Parser.Parser where

import Parser.Lexer (Token)
import qualified Parser.Lexer as Lex
import qualified Data.ByteString as BS
}

%name calc
%tokentype { Token }
%error { parseError }

%nonassoc '==' '/='
%nonassoc '>' '<' '>=' '<='
%left '++'
%left '+' '-'
%left '*' '/'

%token
	let		{ Lex.Let }
	in		{ Lex.In }
	with		{ Lex.With }
	inherit		{ Lex.Inherit }

	if		{ Lex.If }
	then		{ Lex.Then }
	else		{ Lex.Else }
	true		{ Lex.True }
	false		{ Lex.False }

	return		{ Lex.Return }
	break		{ Lex.Break }
	continue	{ Lex.Continue }
	var		{ Lex.Var }

	'+'		{ Lex.Add }
	'-'		{ Lex.Sub }
	'*'		{ Lex.Mul }
	'/'		{ Lex.Div }
	'=='		{ Lex.Eq }
	'/='		{ Lex.Neq }
	'<'		{ Lex.Lt }
	'>'		{ Lex.Gt }
	'<='		{ Lex.Leq }
	'>='		{ Lex.Geq }
	'++'		{ Lex.Cat }
	'.'		{ Lex.Dot }

	';'		{ Lex.Semicolon }
	':'		{ Lex.Colon }
	','		{ Lex.Comma }
	'='		{ Lex.Assign }
	'@'		{ Lex.At }

	'('		{ Lex.LParen }
	')'		{ Lex.RParen }
	'['		{ Lex.LBrack }
	']'		{ Lex.RBrack }
	'{'		{ Lex.LBrace }
	'}'		{ Lex.RBrace }

	num		{ Lex.Num $$ }
	sym		{ Lex.Symbol $$ }
	str		{ Lex.String $$ }

%%

-- TODO Type signatures

Exp
  : sym ':' Exp			{ Lam $1 $3 }
  | let Bindings in Exp		{ Let $2 $4 }
  | with Exp ';' Exp		{ With $2 $4 }
  | if Exp then Exp else Exp	{ Cond $2 $4 $6 }
  | '{' Bindings '}'		{ Attr $2 }
  | '[' ListItems ']'		{ List $2 }
  | BinExp			{ $1 }

BinExp
  : BinExp '==' BinExp	{ BinExp (CompOp Eq) $1 $3 }
  | BinExp '/=' BinExp	{ BinExp (CompOp Neq) $1 $3 }
  | BinExp '>' BinExp	{ BinExp (CompOp Gt) $1 $3 }
  | BinExp '<' BinExp	{ BinExp (CompOp Lt) $1 $3 }
  | BinExp '>=' BinExp	{ BinExp (CompOp Geq) $1 $3 }
  | BinExp '<=' BinExp	{ BinExp (CompOp Leq) $1 $3 }
  | BinExp '++' BinExp	{ BinExp ConcatOp $1 $3 }
  | BinExp '+' BinExp	{ BinExp (ArithOp Add) $1 $3 }
  | BinExp '-' BinExp	{ BinExp (ArithOp Sub) $1 $3 }
  | BinExp '*' BinExp	{ BinExp (ArithOp Mul) $1 $3 }
  | BinExp '/' BinExp	{ BinExp (ArithOp Div) $1 $3 }
  | AppExp		{ $1 }

AppExp
  : AppExp AccExp	{ App $1 $2 }
  | AccExp		{ $1 }

AccExp
  : AccExp '.' sym	{ Acc $1 $3 }
  | Exp0		{ $1 }

Exp0
  : '(' Exp ')'		{ $2 }
  | sym			{ Var $1 }
  | Prim		{ Prim $1 }

Prim
  : str		{ PString $1 }
  | true	{ PBool True }
  | false	{ PBool False }

ListItems
  : ListItemsRev	{ reverse $1 }
  | {- empty -}		{ [] }
ListItemsRev
  : Exp				{ [$1] }
  | ListItemsRev ',' Exp	{ $3 : $1 }

Binding
  : sym NameList '=' Exp ';'		{ Binding $1 $2 $4 }
  | inherit sym ';'			{ Inherit $2 }
  | inherit '(' Exp ')' NameList ';'	{ InheritFrom $3 $5 }

Bindings	: BindingsRev			{ reverse $1 }
BindingsRev	: {- empty -}			{ [] }
		| BindingsRev Binding		{ $2 : $1 }

NameList	: NameListRev			{ reverse $1 }
NameListRev	: {- empty -}			{ [] }
	 	| NameListRev sym		{ $2 : $1 }

{
type Name = BS.ByteString

data Prim
  = PInt Int
  | PBool Bool
  | PString BS.ByteString
  deriving (Eq, Show)

data AST
  = Let [Binding] AST
  | Var Name
  | App AST AST
  | Lam Name AST
  | With AST AST
  | Cond AST AST AST
  | Attr [Binding]
  | List [AST]
  | Acc AST Name
  | BinExp BinOp AST AST
  | Prim Prim
  deriving (Eq, Show)

data BinOp
  = ArithOp ArithOp
  | CompOp CompOp
  | ConcatOp
  deriving (Eq, Show)

data ArithOp = Add | Sub | Mul | Div
  deriving (Eq, Show)

data CompOp = Eq | Neq | Lt | Gt | Geq | Leq
  deriving (Eq, Show)

data Binding
  = Binding Name [Name] AST -- with argument list
  | Inherit Name
  | InheritFrom AST [Name]
  deriving (Eq, Show)

parseError :: [Token] -> a
parseError _ = error "parse error hombre"
}

-- vim: ft=text:noet:ts=8:sw=8:autoindent
