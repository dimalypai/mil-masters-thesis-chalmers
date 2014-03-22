{
{-# OPTIONS_GHC
    -fno-warn-missing-signatures
    -fno-warn-unused-binds
    -fno-warn-unused-matches
    -fno-warn-name-shadowing
 #-}

module MIL.Lexer
  (
    lexer
  , Token(..)
  ) where

}

%wrapper "basic"

$alpha = [a-zA-Z]
$lower = [a-z]
$upper = [A-Z]
$digit = [0-9]

@lowerName = $lower [$alpha $digit]*
@upperName = $upper [$alpha $digit]*

@lineterm = [\n\r] | \r\n
@comment = "#" .* @lineterm

tokens :-

  $white+  ;
  @comment ;

  -- Keywords
  case   { const KW_Case   }
  forall { const KW_Forall }
  let    { const KW_Let    }
  lift   { const KW_Lift   }
  in     { const KW_In     }
  of     { const KW_Of     }
  rec    { const KW_Rec    }
  return { const KW_Return }
  type   { const KW_Type   }
  unit   { const KW_Unit   }

  -- Symbols
  "=" { const Equal }
  ":" { const Colon }

  "\"  { const Lambda    }
  "->" { const Arrow     }
  "/\" { const BigLambda }
  "."  { const Dot       }
  "<-" { const LeftArrow }

  "|"  { const Bar        }
  "_"  { const Underscore }

  "(" { const OpenParen   }
  ")" { const CloseParen  }
  "[" { const OpenSquare  }
  "]" { const CloseSquare }
  "{" { const OpenCurly   }
  "}" { const CloseCurly  }

  ":=" { const ColonEq }
  "!"  { const Bang    }

  ":::" { const TripleColon }

  "," { const Comma     }
  ";" { const SemiColon }

  -- Identifiers
  @lowerName { LowerId }
  @upperName { UpperId }

  -- Literals
  $digit+                                   { \s -> IntLit $ read s }
  $digit+(\.$digit+)? (e (\+|\-)? $digit+)? { \s -> FloatLit (read s) s }

{

-- | Tokens.
data Token =
  -- Keywords
    KW_Case
  | KW_Forall
  | KW_Let
  | KW_Lift
  | KW_In
  | KW_Of
  | KW_Rec
  | KW_Return
  | KW_Type
  | KW_Unit
  -- Symbols
  | Equal
  | Colon

  | Lambda
  | Arrow
  | BigLambda
  | Dot
  | LeftArrow

  | Bar
  | Underscore

  | OpenParen
  | CloseParen
  | OpenSquare
  | CloseSquare
  | OpenCurly
  | CloseCurly

  | ColonEq
  | Bang

  | TripleColon

  | Comma
  | SemiColon
  -- Identifiers
  | LowerId String
  | UpperId String
  -- Literals
  | IntLit Int
  | FloatLit Double String  -- keep the user string
  deriving Eq

instance Show Token where
  -- Keywords
  show KW_Case   = "case"
  show KW_Forall = "forall"
  show KW_Let    = "let"
  show KW_Lift   = "lift"
  show KW_In     = "in"
  show KW_Of     = "of"
  show KW_Rec    = "rec"
  show KW_Return = "return"
  show KW_Type   = "type"
  show KW_Unit   = "unit"
  -- Symbols
  show Equal = "="
  show Colon = ":"

  show Lambda    = "\\"
  show Arrow     = "->"
  show BigLambda = "/\\"
  show Dot       = "."
  show LeftArrow = "<-"

  show Bar        = "|"
  show Underscore = "_"

  show OpenParen   = "("
  show CloseParen  = ")"
  show OpenSquare  = "["
  show CloseSquare = "]"
  show OpenCurly   = "{"
  show CloseCurly  = "}"

  show ColonEq = ":="
  show Bang    = "!"

  show TripleColon = ":::"

  show Comma     = ","
  show SemiColon = ";"
  -- Identifiers
  show (LowerId s) = s
  show (UpperId s) = s
  -- Literals
  show (IntLit n)     = show n
  show (FloatLit f s) = s

-- | Top-level lexing function.
lexer :: String -> [Token]
lexer = alexScanTokens

}

