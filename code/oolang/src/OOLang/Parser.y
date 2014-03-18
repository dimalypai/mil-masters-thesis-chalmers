{

module OOLang.Parser where

import OOLang.Lexer
import OOLang.AST

}

%name      parseOOLang
%tokentype { TokenWithSpan }
%error     { parseError }

%token
  def { undefined }
%%

Program : { undefined }

{

parseOOLang :: [TokenWithSpan] -> Program

parseError :: [TokenWithSpan] -> a
parseError toks = error $ "OOLang parsing error"

}

