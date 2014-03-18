{

module MIL.Parser where

import MIL.Lexer
import MIL.AST

}

%name      parseMil
%tokentype { Token }
%error     { parseError }

%token
  type { undefined }
%%

Program : { undefined }

{

parseMil :: [Token] -> Program

parseError :: [Token] -> a
parseError toks = error $ "MIL parsing error"

}

