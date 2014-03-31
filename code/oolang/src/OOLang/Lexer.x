{
{-# OPTIONS_GHC
    -fno-warn-missing-signatures
    -fno-warn-unused-binds
    -fno-warn-unused-matches
    -fno-warn-name-shadowing
 #-}

module OOLang.Lexer
  (
    lexer
  , Token(..)
  , TokenWithSpan
  , getToken
  , getTokSrcSpan
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
  class     { \p s -> (KW_Class,     posn p s) }
  def       { \p s -> (KW_Def,       posn p s) }
  do        { \p s -> (KW_Do,        posn p s) }
  else      { \p s -> (KW_Else,      posn p s) }
  end       { \p s -> (KW_End,       posn p s) }
  if        { \p s -> (KW_If,        posn p s) }
  just      { \p s -> (KW_Just,      posn p s) }
  nothing   { \p s -> (KW_Nothing,   posn p s) }
  otherwise { \p s -> (KW_Otherwise, posn p s) }
  private   { \p s -> (KW_Private,   posn p s) }
  public    { \p s -> (KW_Public,    posn p s) }
  pure      { \p s -> (KW_Pure,      posn p s) }
  ref       { \p s -> (KW_Ref,       posn p s) }
  return    { \p s -> (KW_Return,    posn p s) }
  static    { \p s -> (KW_Static,    posn p s) }
  then      { \p s -> (KW_Then,      posn p s) }
  unit      { \p s -> (KW_Unit,      posn p s) }
  when      { \p s -> (KW_When,      posn p s) }
  while     { \p s -> (KW_While,     posn p s) }

  -- Keywords for types
  Int     { \p s -> (KW_TyInt,     posn p s) }
  Maybe   { \p s -> (KW_TyMaybe,   posn p s) }
  Mutable { \p s -> (KW_TyMutable, posn p s) }
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

  "(" { \p s -> (OpenParen,  posn p s) }
  ")" { \p s -> (CloseParen, posn p s) }

  ";" { \p s -> (SemiColon, posn p s) }

  -- Identifiers
  @lowerId { \p s -> (LowerId s, posn p s) }
  @upperId { \p s -> (UpperId s, posn p s) }

  -- Literals
  false     { \p s -> (FalseLit, posn p s) }
  true      { \p s -> (TrueLit,  posn p s) }
  $digit+                                   { \p s -> (IntLit $ read s, posn p s) }
  $digit+(\.$digit+)? (e (\+|\-)? $digit+)? { \p s -> (FloatLit (read s) s, posn p s) }
  @string { \p s -> (StringLit $ read s, posn p s) }

{

-- | Tokens.
data Token =
  -- Keywords
    KW_Class
  | KW_Def
  | KW_Do
  | KW_Else
  | KW_End
  | KW_If
  | KW_Just
  | KW_Nothing
  | KW_Otherwise
  | KW_Private
  | KW_Public
  | KW_Pure
  | KW_Ref
  | KW_Return
  | KW_Static
  | KW_Then
  | KW_Unit
  | KW_When
  | KW_While
  -- Keywords for types
  | KW_TyInt
  | KW_TyMaybe
  | KW_TyMutable
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

  | SemiColon
  -- Identifiers
  | LowerId String
  | UpperId String
  -- Literals
  | FalseLit
  | TrueLit
  | IntLit Int
  | FloatLit Double String  -- keep the user string
  | StringLit String
  deriving Eq

instance Show Token where
  -- Keywords
  show KW_Class     = "class"
  show KW_Def       = "def"
  show KW_Do        = "do"
  show KW_Else      = "else"
  show KW_End       = "end"
  show KW_If        = "if"
  show KW_Just      = "just"
  show KW_Nothing   = "nothing"
  show KW_Otherwise = "otherwise"
  show KW_Private   = "private"
  show KW_Public    = "public"
  show KW_Pure      = "pure"
  show KW_Ref       = "ref"
  show KW_Return    = "return"
  show KW_Static    = "static"
  show KW_Then      = "then"
  show KW_Unit      = "unit"
  show KW_When      = "when"
  show KW_While     = "while"
  -- Keywords for types
  show KW_TyInt     = "Int"
  show KW_TyMaybe   = "Maybe"
  show KW_TyMutable = "Mutable"
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

  show OpenParen  = "("
  show CloseParen = ")"

  show SemiColon = ";"
  -- Identifiers
  show (LowerId s) = s
  show (UpperId s) = s
  -- Literals
  show FalseLit       = "false"
  show TrueLit        = "true"
  show (IntLit n)     = show n
  show (FloatLit f s) = s
  show (StringLit s)  = show s

-- | Token with it's source span.
type TokenWithSpan = (Token, SrcSpan)

getToken :: TokenWithSpan -> Token
getToken = fst

getTokSrcSpan :: TokenWithSpan -> SrcSpan
getTokSrcSpan = snd

-- | Converts Alex source position to source span.
-- Takes token string for length calculation.
-- NONAME is used as a source file name. Should be replaced in the parser.
posn :: AlexPosn -> String -> SrcSpan
posn (AlexPn _ l c) tokStr = mkSrcSpan "NONAME" l c l (c + length tokStr - 1)

-- | Top-level lexing function.
lexer :: String -> [TokenWithSpan]
lexer = alexScanTokens

}

