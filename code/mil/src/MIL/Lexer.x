{
module MIL.Lexer
  (
    lexer
  , Token(..)
  ) where

}

%wrapper "basic"

@lineterm = [\n\r] | \r\n
@comment = "#" .* @lineterm

tokens :-

  $white+  ;
  @comment ;

  -- Keywords
  type { \_ -> KW_Type }

{

-- | Tokens.
data Token = KW_Type

-- | Top-level lexing function.
lexer :: String -> [Token]
lexer = alexScanTokens

}

