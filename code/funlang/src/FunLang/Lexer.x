{
{-# OPTIONS_GHC
    -fno-warn-missing-signatures
    -fno-warn-unused-binds
    -fno-warn-unused-matches
    -fno-warn-name-shadowing
 #-}

-- | Lexing module. Written using Alex.
module FunLang.Lexer
  (
    lexer
  , Token(..)
  , TokenWithSpan
  , getToken
  , getTokSrcSpan
  , getId
  , getTokId
  , mkTokSrcSpan
  , getIntLitValue
  , getFloatLitValue
  , getFloatLitString
  , getStringLitValue
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
  catch  { \p s -> (KW_Catch,  posn p s) }
  do     { \p s -> (KW_Do,     posn p s) }
  end    { \p s -> (KW_End,    posn p s) }
  forall { \p s -> (KW_Forall, posn p s) }
  of     { \p s -> (KW_Of,     posn p s) }
  return { \p s -> (KW_Return, posn p s) }
  throw  { \p s -> (KW_Throw,  posn p s) }
  type   { \p s -> (KW_Type,   posn p s) }
  unit   { \p s -> (KW_Unit,   posn p s) }

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
  "=>" { \p s -> (FatArrow,  posn p s) }
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
  | KW_Catch
  | KW_Do
  | KW_End
  | KW_Forall
  | KW_Of
  | KW_Return
  | KW_Throw
  | KW_Type
  | KW_Unit
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
  | FatArrow
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
  | FloatLit Double String  -- ^ The user string (for displaying)
  | StringLit String
  deriving Eq

instance Show Token where
  -- Keywords
  show KW_Case   = "case"
  show KW_Catch  = "catch"
  show KW_Do     = "do"
  show KW_End    = "end"
  show KW_Forall = "forall"
  show KW_Of     = "of"
  show KW_Return = "return"
  show KW_Throw  = "throw"
  show KW_Type   = "type"
  show KW_Unit   = "unit"
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
  show FatArrow  = "=>"
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
  show (IntLit i)     = show i
  show (FloatLit f s) = s
  show (StringLit s)  = show s

-- | Token with it's source span.
type TokenWithSpan = (Token, SrcSpan)

getToken :: TokenWithSpan -> Token
getToken = fst

getTokSrcSpan :: TokenWithSpan -> SrcSpan
getTokSrcSpan = snd

-- | Returns a string from a token that represents identifier ('LowerId' or 'UpperId').
-- For others - throws an error.
getId :: Token -> String
getId (LowerId s) = s
getId (UpperId s) = s
getId         tok = error $ show tok ++ " is not an Id token"

-- | Returns a string from a token with source span that represents identifier ('LowerId' or 'UpperId').
-- For others - throws an error.
getTokId :: TokenWithSpan -> String
getTokId = getId . getToken

-- | Sets a file name (second argument) in the source span of a given token with span.
mkTokSrcSpan :: TokenWithSpan -> String -> SrcSpan
mkTokSrcSpan tok fileName = setSrcSpanFileName (getTokSrcSpan tok) fileName

-- | Returns a value of integer literal.
-- For tokens other than IntLit - throws an error.
getIntLitValue :: TokenWithSpan -> Int
getIntLitValue (IntLit i, _) = i
getIntLitValue           tok = error $ show tok ++ " is not an integer literal token"

-- | Returns a value of float literal.
-- For tokens other than FloatLit - throws an error.
getFloatLitValue :: TokenWithSpan -> Double
getFloatLitValue (FloatLit f _, _) = f
getFloatLitValue               tok = error $ show tok ++ " is not a float literal token"

-- | Returns a user string of float literal.
-- For tokens other than FloatLit - throws an error.
getFloatLitString :: TokenWithSpan -> String
getFloatLitString (FloatLit _ s, _) = s
getFloatLitString               tok = error $ show tok ++ " is not a float literal token"

-- | Returns a value of string literal.
-- For tokens other than StringLit - throws an error.
getStringLitValue :: TokenWithSpan -> String
getStringLitValue (StringLit s, _) = s
getStringLitValue              tok = error $ show tok ++ " is not a string literal token"

-- | Converts Alex source position to source span.
-- Takes token string for length calculation.
-- NONAME is used as a source file name. Should be replaced in the parser.
posn :: AlexPosn -> String -> SrcSpan
posn (AlexPn _ l c) tokStr = mkSrcSpan "NONAME" l c l (c + length tokStr - 1)

-- | Top-level lexing function.
lexer :: String -> [TokenWithSpan]
lexer = alexScanTokens

}

