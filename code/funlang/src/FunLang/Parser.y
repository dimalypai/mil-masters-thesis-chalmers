{

module FunLang.Parser where

import FunLang.Lexer
import FunLang.AST

}

%name      parseFunLang
%tokentype { TokenWithSpan }
%error     { parseError }

%token
  type { undefined }
%%

Program : { undefined }

{

parseFunLang :: [TokenWithSpan] -> Program

parseError :: [TokenWithSpan] -> a
parseError toks = error $ "FunLang parsing error"

}

