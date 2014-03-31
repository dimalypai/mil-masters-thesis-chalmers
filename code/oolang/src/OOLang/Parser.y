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

program :: { SrcProgram }
program : { undefined }

{

parseOOLang :: [TokenWithSpan] -> SrcProgram

parseError :: [TokenWithSpan] -> a
parseError toks = error $ "OOLang parsing error"

}

