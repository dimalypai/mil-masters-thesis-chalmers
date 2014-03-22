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
  , TokenWithSpan(..)
  , getToken
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
  case   { \p s -> TokWSpan KW_Case   (posn p s) }
  do     { \p s -> TokWSpan KW_Do     (posn p s) }
  end    { \p s -> TokWSpan KW_End    (posn p s) }
  forall { \p s -> TokWSpan KW_Forall (posn p s) }
  of     { \p s -> TokWSpan KW_Of     (posn p s) }
  return { \p s -> TokWSpan KW_Return (posn p s) }
  type   { \p s -> TokWSpan KW_Type   (posn p s) }
  unit   { \p s -> TokWSpan KW_Unit   (posn p s) }
  -- Keywords for types???
  Input  { \p s -> TokWSpan KW_TyInput  (posn p s) }
  Int    { \p s -> TokWSpan KW_TyInt    (posn p s) }
  IO     { \p s -> TokWSpan KW_TyIO     (posn p s) }
  Output { \p s -> TokWSpan KW_TyOutput (posn p s) }
  State  { \p s -> TokWSpan KW_TyState  (posn p s) }
  Unit   { \p s -> TokWSpan KW_TyUnit   (posn p s) }

  -- Symbols
  "=" { \p s -> TokWSpan Equal (posn p s) }
  ":" { \p s -> TokWSpan Colon (posn p s) }

  "+"  { \p s -> TokWSpan Plus      (posn p s) }
  "-"  { \p s -> TokWSpan Minus     (posn p s) }
  "*"  { \p s -> TokWSpan Star      (posn p s) }
  "/"  { \p s -> TokWSpan Slash     (posn p s) }
  "%"  { \p s -> TokWSpan Percent   (posn p s) }
  "<"  { \p s -> TokWSpan Less      (posn p s) }
  ">"  { \p s -> TokWSpan Greater   (posn p s) }
  "<=" { \p s -> TokWSpan LessEq    (posn p s) }
  ">=" { \p s -> TokWSpan GreaterEq (posn p s) }
  "/=" { \p s -> TokWSpan NotEq     (posn p s) }

  "\"  { \p s -> TokWSpan Lambda    (posn p s) }
  "->" { \p s -> TokWSpan Arrow     (posn p s) }
  "/\" { \p s -> TokWSpan BigLambda (posn p s) }
  "."  { \p s -> TokWSpan Dot       (posn p s) }
  "<-" { \p s -> TokWSpan LeftArrow (posn p s) }

  "|"  { \p s -> TokWSpan Bar        (posn p s) }
  "_"  { \p s -> TokWSpan Underscore (posn p s) }

  "&&" { \p s -> TokWSpan And (posn p s) }
  "||" { \p s -> TokWSpan Or  (posn p s) }

  "(" { \p s -> TokWSpan OpenParen   (posn p s) }
  ")" { \p s -> TokWSpan CloseParen  (posn p s) }
  "[" { \p s -> TokWSpan OpenSquare  (posn p s) }
  "]" { \p s -> TokWSpan CloseSquare (posn p s) }

  ";" { \p s -> TokWSpan SemiColon (posn p s) }

  -- Identifiers
  @lowerId { \p s -> TokWSpan (LowerId s, posn p s) }
  @upperId { \p s -> TokWSpan (UpperId s, posn p s) }

  -- Literals
  $digit+                                   { \p s -> TokWSpan (IntLit $ read s) (posn p s) }
  $digit+(\.$digit+)? (e (\+|\-)? $digit+)? { \p s -> TokWSpan (FloatLit (read s) s) (posn p s) }
  @string { \p s -> TokWSpan (StringLit $ read s) (posn p s) }

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
data TokenWithSpan = TokWSpan Token SrcSpan
  deriving (Show, Eq)

getToken :: TokenWithSpan -> Token
getToken (TokWSpan tok _) = tok

-- | Converts Alex source position to source span.
-- Takes token string for length calculation.
-- NONAME is used as a source file name. Should be replaced in the parser.
posn :: AlexPosn -> String -> SrcSpan
posn (AlexPn _ l c) tokStr = SrcSpan "NONAME" l c l (c + length tokStr - 1)

-- | Top-level lexing function.
lexer :: String -> [TokenWithSpan]
lexer = alexScanTokens

}

