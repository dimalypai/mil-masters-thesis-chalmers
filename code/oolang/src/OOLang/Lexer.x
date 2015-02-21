{
{-# OPTIONS_GHC
    -fno-warn-missing-signatures
    -fno-warn-unused-binds
    -fno-warn-unused-matches
    -fno-warn-name-shadowing
 #-}

-- | Lexing module. Written using Alex.
module OOLang.Lexer
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

import OOLang.SrcSpan

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
  catch     { \p s -> (KW_Catch,     posn p s) }
  class     { \p s -> (KW_Class,     posn p s) }
  def       { \p s -> (KW_Def,       posn p s) }
  do        { \p s -> (KW_Do,        posn p s) }
  else      { \p s -> (KW_Else,      posn p s) }
  end       { \p s -> (KW_End,       posn p s) }
  false     { \p s -> (KW_False,     posn p s) }
  finally   { \p s -> (KW_Finally,   posn p s) }
  if        { \p s -> (KW_If,        posn p s) }
  just      { \p s -> (KW_Just,      posn p s) }
  nothing   { \p s -> (KW_Nothing,   posn p s) }
  otherwise { \p s -> (KW_Otherwise, posn p s) }
  private   { \p s -> (KW_Private,   posn p s) }
  public    { \p s -> (KW_Public,    posn p s) }
  ref       { \p s -> (KW_Ref,       posn p s) }
  return    { \p s -> (KW_Return,    posn p s) }
  static    { \p s -> (KW_Static,    posn p s) }
  then      { \p s -> (KW_Then,      posn p s) }
  true      { \p s -> (KW_True,      posn p s) }
  try       { \p s -> (KW_Try,       posn p s) }
  unit      { \p s -> (KW_Unit,      posn p s) }
  when      { \p s -> (KW_When,      posn p s) }
  while     { \p s -> (KW_While,     posn p s) }

  -- Keywords for types
  Bool    { \p s -> (KW_TyBool,    posn p s) }
  Float   { \p s -> (KW_TyFloat,   posn p s) }
  Int     { \p s -> (KW_TyInt,     posn p s) }
  String  { \p s -> (KW_TyString,  posn p s) }
  Maybe   { \p s -> (KW_TyMaybe,   posn p s) }
  Mutable { \p s -> (KW_TyMutable, posn p s) }
  Pure    { \p s -> (KW_TyPure,    posn p s) }
  Ref     { \p s -> (KW_TyRef,     posn p s) }
  Unit    { \p s -> (KW_TyUnit,    posn p s) }

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

  "=>" { \p s -> (FatArrow, posn p s) }
  "->" { \p s -> (Arrow,    posn p s) }
  "\"  { \p s -> (Lambda,   posn p s) }

  "."  { \p s -> (Dot,            posn p s) }
  "::" { \p s -> (DoubleColon,    posn p s) }
  "?"  { \p s -> (Question,       posn p s) }
  "??" { \p s -> (DoubleQuestion, posn p s) }

  "<-" { \p s -> (LeftArrow, posn p s) }
  ":=" { \p s -> (ColonEq,   posn p s) }
  "!"  { \p s -> (Bang,      posn p s) }

  "&&" { \p s -> (And, posn p s) }
  "||" { \p s -> (Or,  posn p s) }

  "(" { \p s -> (OpenParen,   posn p s) }
  ")" { \p s -> (CloseParen,  posn p s) }
  "{" { \p s -> (OpenCurly,   posn p s) }
  "}" { \p s -> (CloseCurly,  posn p s) }
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
    KW_Catch
  | KW_Class
  | KW_Def
  | KW_Do
  | KW_Else
  | KW_End
  | KW_False
  | KW_Finally
  | KW_If
  | KW_Just
  | KW_Nothing
  | KW_Otherwise
  | KW_Private
  | KW_Public
  | KW_Ref
  | KW_Return
  | KW_Static
  | KW_Then
  | KW_True
  | KW_Try
  | KW_Unit
  | KW_When
  | KW_While
  -- Keywords for types
  | KW_TyBool
  | KW_TyFloat
  | KW_TyInt
  | KW_TyString
  | KW_TyMaybe
  | KW_TyMutable
  | KW_TyPure
  | KW_TyRef
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

  | FatArrow
  | Arrow
  | Lambda

  | Dot
  | DoubleColon
  | Question
  | DoubleQuestion

  | LeftArrow
  | ColonEq
  | Bang

  | And
  | Or

  | OpenParen
  | CloseParen
  | OpenCurly
  | CloseCurly
  | OpenSquare
  | CloseSquare

  | SemiColon
  -- Identifiers
  | LowerId String
  | UpperId String
  -- Literals
  | IntLit Int
  | FloatLit Double String  -- ^ The user string (for displaying).
  | StringLit String
  deriving Eq

instance Show Token where
  -- Keywords
  show KW_Catch     = "catch"
  show KW_Class     = "class"
  show KW_Def       = "def"
  show KW_Do        = "do"
  show KW_Else      = "else"
  show KW_End       = "end"
  show KW_False     = "false"
  show KW_Finally   = "finally"
  show KW_If        = "if"
  show KW_Just      = "just"
  show KW_Nothing   = "nothing"
  show KW_Otherwise = "otherwise"
  show KW_Private   = "private"
  show KW_Public    = "public"
  show KW_Ref       = "ref"
  show KW_Return    = "return"
  show KW_Static    = "static"
  show KW_Then      = "then"
  show KW_True      = "true"
  show KW_Try       = "try"
  show KW_Unit      = "unit"
  show KW_When      = "when"
  show KW_While     = "while"
  -- Keywords for types
  show KW_TyBool    = "Bool"
  show KW_TyFloat   = "Float"
  show KW_TyInt     = "Int"
  show KW_TyString  = "String"
  show KW_TyMaybe   = "Maybe"
  show KW_TyMutable = "Mutable"
  show KW_TyPure    = "Pure"
  show KW_TyRef     = "Ref"
  show KW_TyUnit    = "Unit"
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

  show FatArrow = "=>"
  show Arrow    = "->"
  show Lambda   = "\\"

  show Dot            = "."
  show DoubleColon    = "::"
  show Question       = "?"
  show DoubleQuestion = "??"

  show LeftArrow = "<-"
  show ColonEq   = ":="
  show Bang      = "!"

  show And = "&&"
  show Or  = "||"

  show OpenParen   = "("
  show CloseParen  = ")"
  show OpenCurly   = "{"
  show CloseCurly  = "}"
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

