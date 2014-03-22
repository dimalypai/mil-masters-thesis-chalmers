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
  , TokenWithSpan(..)
  , getToken
  ) where

import OOLang.SrcSpan

}

%wrapper "posn"

$alpha = [a-zA-Z]
$lower = [a-z]
$upper = [A-Z]
$digit = [0-9]

@lowerName = $lower [$alpha $digit]*
@upperName = $upper [$alpha $digit]*
@string = \" ($printable # \")* \"

@lineterm = [\n\r] | \r\n
@comment = "#" .* @lineterm

tokens :-

  $white+  ;
  @comment ;

  -- Keywords
  class     { \p s -> TokWSpan KW_Class     (posn p s) }
  def       { \p s -> TokWSpan KW_Def       (posn p s) }
  do        { \p s -> TokWSpan KW_Do        (posn p s) }
  else      { \p s -> TokWSpan KW_Else      (posn p s) }
  end       { \p s -> TokWSpan KW_End       (posn p s) }
  if        { \p s -> TokWSpan KW_If        (posn p s) }
  just      { \p s -> TokWSpan KW_Just      (posn p s) }
  nothing   { \p s -> TokWSpan KW_Nothing   (posn p s) }
  otherwise { \p s -> TokWSpan KW_Otherwise (posn p s) }
  private   { \p s -> TokWSpan KW_Private   (posn p s) }
  public    { \p s -> TokWSpan KW_Public    (posn p s) }
  pure      { \p s -> TokWSpan KW_Pure      (posn p s) }
  ref       { \p s -> TokWSpan KW_Ref       (posn p s) }
  return    { \p s -> TokWSpan KW_Return    (posn p s) }
  static    { \p s -> TokWSpan KW_Static    (posn p s) }
  then      { \p s -> TokWSpan KW_Then      (posn p s) }
  unit      { \p s -> TokWSpan KW_Unit      (posn p s) }
  when      { \p s -> TokWSpan KW_When      (posn p s) }
  while     { \p s -> TokWSpan KW_While     (posn p s) }

  -- Keywords for types
  Int     { \p s -> TokWSpan KW_TyInt     (posn p s) }
  Maybe   { \p s -> TokWSpan KW_TyMaybe   (posn p s) }
  Mutable { \p s -> TokWSpan KW_TyMutable (posn p s) }
  Ref     { \p s -> TokWSpan KW_TyRef     (posn p s) }
  Unit    { \p s -> TokWSpan KW_TyUnit    (posn p s) }

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

  "=>" { \p s -> TokWSpan FatArrow (posn p s) }
  "->" { \p s -> TokWSpan Arrow    (posn p s) }
  "\"  { \p s -> TokWSpan Lambda   (posn p s) }

  "."  { \p s -> TokWSpan Dot            (posn p s) }
  "::" { \p s -> TokWSpan DoubleColon    (posn p s) }
  "?"  { \p s -> TokWSpan Question       (posn p s) }
  "??" { \p s -> TokWSpan DoubleQuestion (posn p s) }

  "<-" { \p s -> TokWSpan LeftArrow (posn p s) }
  ":=" { \p s -> TokWSpan ColonEq   (posn p s) }
  "!"  { \p s -> TokWSpan Bang      (posn p s) }

  "&&" { \p s -> TokWSpan And (posn p s) }
  "||" { \p s -> TokWSpan Or  (posn p s) }

  "(" { \p s -> TokWSpan OpenParen  (posn p s) }
  ")" { \p s -> TokWSpan CloseParen (posn p s) }

  ";" { \p s -> TokWSpan SemiColon (posn p s) }

  -- Identifiers
  @lowerName { \p s -> TokWSpan (LowerId s) (posn p s) }
  @upperName { \p s -> TokWSpan (UpperId s) (posn p s) }

  -- Literals
  false     { \p s -> TokWSpan FalseLit (posn p s) }
  true      { \p s -> TokWSpan TrueLit  (posn p s) }
  $digit+                                   { \p s -> TokWSpan (IntLit $ read s) (posn p s) }
  $digit+(\.$digit+)? (e (\+|\-)? $digit+)? { \p s -> TokWSpan (FloatLit (read s) s) (posn p s) }
  @string { \p s -> TokWSpan (StringLit $ read s) (posn p s) }

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

