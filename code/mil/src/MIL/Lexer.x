{
{-# OPTIONS_GHC
    -fno-warn-missing-signatures
    -fno-warn-unused-binds
    -fno-warn-unused-matches
    -fno-warn-name-shadowing
 #-}

-- | Lexing module. Written using Alex.
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

@lowerId = $lower [$alpha $digit \_]*
@upperId = $upper [$alpha $digit \_]*

@lineterm = [\n\r] | \r\n
@comment = "#" .* @lineterm
@char = ' [$printable \n \r \t \v] '

tokens :-

  $white+  ;
  @comment ;

  -- Keywords
  alias  { const KW_Alias  }
  case   { const KW_Case   }
  end    { const KW_End    }
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
  "=>" { const FatArrow  }
  "<-" { const LeftArrow }

  "|"  { const Bar        }
  "_"  { const Underscore }

  "(" { const OpenParen   }
  ")" { const CloseParen  }
  "[" { const OpenSquare  }
  "]" { const CloseSquare }
  "{" { const OpenCurly   }
  "}" { const CloseCurly  }

  ":::" { const TripleColon }

  "," { const Comma     }
  ";" { const SemiColon }

  -- Identifiers
  @lowerId { LowerId }
  @upperId { UpperId }

  -- Literals
  $digit+                                   { \s -> IntLit $ read s }
  $digit+(\.$digit+)? (e (\+|\-)? $digit+)? { \s -> FloatLit (read s) s }
  @char { \s -> CharLit $ read s }

{

-- | Tokens.
data Token =
  -- Keywords
    KW_Alias
  | KW_Case
  | KW_End
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
  | FatArrow
  | LeftArrow

  | Bar
  | Underscore

  | OpenParen
  | CloseParen
  | OpenSquare
  | CloseSquare
  | OpenCurly
  | CloseCurly

  | TripleColon

  | Comma
  | SemiColon
  -- Identifiers
  | LowerId String
  | UpperId String
  -- Literals
  | IntLit Int
  | FloatLit Double String  -- ^ The user string (for displaying)
  | CharLit Char
  deriving Eq

instance Show Token where
  -- Keywords
  show KW_Alias  = "alias"
  show KW_Case   = "case"
  show KW_End    = "end"
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
  show FatArrow  = "=>"
  show LeftArrow = "<-"

  show Bar        = "|"
  show Underscore = "_"

  show OpenParen   = "("
  show CloseParen  = ")"
  show OpenSquare  = "["
  show CloseSquare = "]"
  show OpenCurly   = "{"
  show CloseCurly  = "}"

  show TripleColon = ":::"

  show Comma     = ","
  show SemiColon = ";"
  -- Identifiers
  show (LowerId s) = s
  show (UpperId s) = s
  -- Literals
  show (IntLit n)     = show n
  show (FloatLit f s) = s
  show (CharLit c)    = show c

-- | Top-level lexing function.
lexer :: String -> [Token]
lexer = alexScanTokens

}

