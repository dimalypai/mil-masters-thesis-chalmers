{
{-# OPTIONS_GHC
    -fno-warn-missing-signatures
    -fno-warn-unused-binds
    -fno-warn-unused-matches
    -fno-warn-name-shadowing
 #-}

module FunLang.Lexer
  (
    lexer
  , Token(..)
  , TokenWithSpan
  , getToken
  , getTokSrcSpan
  , getId
  ) where

import FunLang.SrcSpan

}

%wrapper "posn"

$alpha = [a-zA-Z]
$lower = [a-z]
$upper = [A-Z]
$digit = [0-9]

@lowerId = $lower [$alpha $digit]*
@upperId = $upper [$alpha $digit]*
@string = \" ($printable # \")* \"

@lineterm = [\n\r] | \r\n
@comment = "#" .* @lineterm

tokens :-

  $white+  ;
  @comment ;

  -- Keywords
  case   { \p s -> (KW_Case,   posn p s) }
  do     { \p s -> (KW_Do,     posn p s) }
  end    { \p s -> (KW_End,    posn p s) }
  forall { \p s -> (KW_Forall, posn p s) }
  of     { \p s -> (KW_Of,     posn p s) }
  return { \p s -> (KW_Return, posn p s) }
  type   { \p s -> (KW_Type,   posn p s) }
  unit   { \p s -> (KW_Unit,   posn p s) }
  -- Keywords for types???
  Input  { \p s -> (KW_TyInput,  posn p s) }
  Int    { \p s -> (KW_TyInt,    posn p s) }
  IO     { \p s -> (KW_TyIO,     posn p s) }
  Output { \p s -> (KW_TyOutput, posn p s) }
  State  { \p s -> (KW_TyState,  posn p s) }
  Unit   { \p s -> (KW_TyUnit,   posn p s) }

  -- Symbols
  "=" { \p s -> (Equal, posn p s) }
  ":" { \p s -> (Colon, posn p s) }

  "+"  { \p s -> (Plus,      posn p s) }
  "-"  { \p s -> (Minus,     posn p s) }
  "*"  { \p s -> (Star,      posn p s) }
  "/"  { \p s -> (Slash,     posn p s) }
  "%"  { \p s -> (Percent,   posn p s) }
  "<"  { \p s -> (Less,      posn p s) }
  ">"  { \p s -> (Greater,   posn p s) }
  "<=" { \p s -> (LessEq,    posn p s) }
  ">=" { \p s -> (GreaterEq, posn p s) }
  "/=" { \p s -> (NotEq,     posn p s) }

  "\"  { \p s -> (Lambda,    posn p s) }
  "->" { \p s -> (Arrow,     posn p s) }
  "/\" { \p s -> (BigLambda, posn p s) }
  "."  { \p s -> (Dot,       posn p s) }
  "<-" { \p s -> (LeftArrow, posn p s) }

  "|"  { \p s -> (Bar,        posn p s) }
  "_"  { \p s -> (Underscore, posn p s) }

  "&&" { \p s -> (And, posn p s) }
  "||" { \p s -> (Or,  posn p s) }

  "(" { \p s -> (OpenParen,   posn p s) }
  ")" { \p s -> (CloseParen,  posn p s) }
  "[" { \p s -> (OpenSquare,  posn p s) }
  "]" { \p s -> (CloseSquare, posn p s) }

  ";" { \p s -> (SemiColon, posn p s) }

  -- Identifiers
  @lowerId { \p s -> (LowerId s, posn p s) }
  @upperId { \p s -> (UpperId s, posn p s) }

  -- Literals
  $digit+                                   { \p s -> (IntLit $ read s, posn p s) }
  $digit+(\.$digit+)? (e (\+|\-)? $digit+)? { \p s -> (FloatLit (read s) s, posn p s) }
  @string { \p s -> (StringLit $ read s, posn p s) }

{

-- | Tokens.
data Token =
  -- Keywords
    KW_Case
  | KW_Do
  | KW_End
  | KW_Forall
  | KW_Of
  | KW_Return
  | KW_Type
  | KW_Unit
  -- Keywords for types
  | KW_TyInput
  | KW_TyInt
  | KW_TyIO
  | KW_TyOutput
  | KW_TyState
  | KW_TyUnit
  -- Symbols
  | Equal
  | Colon

  | Plus
  | Minus
  | Star
  | Slash
  | Percent
  | Less
  | Greater
  | LessEq
  | GreaterEq
  | NotEq

  | Lambda
  | Arrow
  | BigLambda
  | Dot
  | LeftArrow

  | Bar
  | Underscore

  | And
  | Or

  | OpenParen
  | CloseParen
  | OpenSquare
  | CloseSquare

  | SemiColon
  -- Identifiers
  | LowerId String
  | UpperId String
  -- Literals
  | IntLit Int
  | FloatLit Double String  -- keep the user string
  | StringLit String
  deriving Eq

instance Show Token where
  -- Keywords
  show KW_Case   = "case"
  show KW_Do     = "do"
  show KW_End    = "end"
  show KW_Forall = "forall"
  show KW_Of     = "of"
  show KW_Return = "return"
  show KW_Type   = "type"
  show KW_Unit   = "unit"
  -- Keywords for types
  show KW_TyInput  = "Input"
  show KW_TyInt    = "Int"
  show KW_TyIO     = "IO"
  show KW_TyOutput = "Output"
  show KW_TyState  = "State"
  show KW_TyUnit   = "Unit"
  -- Symbols
  show Equal = "="
  show Colon = ":"

  show Plus      = "+"
  show Minus     = "-"
  show Star      = "*"
  show Slash     = "/"
  show Percent   = "%"
  show Less      = "<"
  show Greater   = ">"
  show LessEq    = "<="
  show GreaterEq = ">="
  show NotEq     = "/="

  show Lambda    = "\\"
  show Arrow     = "->"
  show BigLambda = "/\\"
  show Dot       = "."
  show LeftArrow = "<-"

  show Bar        = "|"
  show Underscore = "_"

  show And = "&&"
  show Or  = "||"

  show OpenParen   = "("
  show CloseParen  = ")"
  show OpenSquare  = "["
  show CloseSquare = "]"

  show SemiColon = ";"
  -- Identifiers
  show (LowerId s) = s
  show (UpperId s) = s
  -- Literals
  show (IntLit n)     = show n
  show (FloatLit f s) = s
  show (StringLit s)  = show s

-- | Token with it's source span.
type TokenWithSpan = (Token, SrcSpan)

getToken :: TokenWithSpan -> Token
getToken = fst

getTokSrcSpan :: TokenWithSpan -> SrcSpan
getTokSrcSpan = snd

getId :: Token -> String
getId (LowerId s) = s
getId (UpperId s) = s
getId         tok = error $ show tok ++ " is not an Id token"

-- | Converts Alex source position to source span.
-- Takes token string for length calculation.
-- NONAME is used as a source file name. Should be replaced in the parser.
posn :: AlexPosn -> String -> SrcSpan
posn (AlexPn _ l c) tokStr = SrcSpan "NONAME" l c l (c + length tokStr - 1)

-- | Top-level lexing function.
lexer :: String -> [TokenWithSpan]
lexer = alexScanTokens

}

