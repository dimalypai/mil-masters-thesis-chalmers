{
module OOLang.Lexer
  (
    lexer
  , Token(..)
  , TokenWithSpan(..)
  ) where

import OOLang.SrcSpan

}

%wrapper "posn"

@lineterm = [\n\r] | \r\n
@comment = "#" .* @lineterm

tokens :-

  $white+  ;
  @comment ;

  -- Keywords
  def { \p s -> TokWSpan KW_Def (posn p s) }

{

-- | Tokens.
data Token = KW_Def

-- | Token with it's source span.
data TokenWithSpan = TokWSpan Token SrcSpan

-- | Converts Alex source position to source span.
-- Takes token string for length calculation.
-- NONAME is used as a source file name. Should be replaced in the parser.
posn :: AlexPosn -> String -> SrcSpan
posn (AlexPn _ l c) tokStr = SrcSpan "NONAME" l c l (c + length tokStr - 1)

-- | Top-level lexing function.
lexer :: String -> [TokenWithSpan]
lexer = alexScanTokens

}

