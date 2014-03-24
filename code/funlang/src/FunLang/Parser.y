{

module FunLang.Parser where

import Control.Monad.Error
import Control.Monad.Reader

import FunLang.Lexer as Lex
import FunLang.AST
import FunLang.SrcSpan

}

%name      funLang
%tokentype { TokenWithSpan }
%error     { parseError }
%monad     { ParseM }

%token
  -- Keywords
  case   { $$@(KW_Case,   _) }
  do     { $$@(KW_Do,     _) }
  end    { $$@(KW_End,    _) }
  forall { $$@(KW_Forall, _) }
  of     { $$@(KW_Of,     _) }
  return { $$@(KW_Return, _) }
  type   { $$@(KW_Type,   _) }
  unit   { $$@(KW_Unit,   _) }
  -- Symbols
  '=' { $$@(Lex.Equal, _) }
  ':' { $$@(Colon,     _) }

  '+'  { $$@(Plus,          _) }
  '-'  { $$@(Minus,         _) }
  '*'  { $$@(Star,          _) }
  '/'  { $$@(Slash,         _) }
  '%'  { $$@(Percent,       _) }
  '<'  { $$@(Lex.Less,      _) }
  '>'  { $$@(Lex.Greater,   _) }
  '<=' { $$@(Lex.LessEq,    _) }
  '>=' { $$@(Lex.GreaterEq, _) }
  '/=' { $$@(Lex.NotEq,     _) }

  '\\'  { $$@(Lambda,    _) }
  '->'  { $$@(Arrow,     _) }
  '/\\' { $$@(BigLambda, _) }
  '.'   { $$@(Dot,       _) }
  '<-'  { $$@(LeftArrow, _) }

  '|' { $$@(Bar,        _) }
  '_' { $$@(Underscore, _) }

  '&&' { $$@(And, _) }
  '||' { $$@(Or,  _) }

  '(' { $$@(OpenParen,   _) }
  ')' { $$@(CloseParen,  _) }
  '[' { $$@(OpenSquare,  _) }
  ']' { $$@(CloseSquare, _) }

  ';' { $$@(SemiColon, _) }
  -- Identifiers
  lowerId { $$@(LowerId _, _) }
  upperId { $$@(UpperId _, _) }
  -- Literals
  intLit    { $$@(Lex.IntLit _, _) }
  floatLit  { $$@(FloatLit _ _, _) }
  stringLit { $$@(StringLit  _, _) }
%%

program :: { SrcProgram }
program : list1(topdef) {% withFileName $ \fileName ->
                             Program (combineSrcSpan $ map ann $1)
                                     (filterMap isTypeDef getTypeDef $1)
                                     (filterMap isFunDef  getFunDef  $1) }

topdef :: { TopDef SrcSpan }
topdef : typedef { TopTypeDef $1 }
       | fundef  { TopFunDef $1 }

typedef :: { TypeDef SrcSpan }
typedef : type upperId '=' {% withFileName $ \fileName ->
                                TypeDef (SrcSpan fileName 1 1 2 2)
                                        (TypeName $ getId (getToken $2)) [] [] }
        | type lowerId '=' {% throwError "Type name must begin with a capital letter" }

fundef :: { FunDef SrcSpan }
fundef : lowerId ':' {% withFileName $ \fileName ->
                          FunDef (SrcSpan fileName 3 3 4 4)
                                 (FunName $ getId (getToken $1)) TyUnit [] }

-- Helper productions

-- Optional p.
opt(p) : p           { Just $1 }
       | {- empty -} { Nothing }

-- Zero or more occurences of p.
list(p) : list1(p)    { $1 }
        | {- empty -} { [] }

-- One or more occurences of p.
list1(p) : list1rev(p) { reverse $1 }

-- Helper for list1. Returns reversed list because of using left recursion
-- for performance.
list1rev(p) : p             { [$1] }
            | list1rev(p) p { $2 : $1 }

-- Zero or more occurences of p separated by s.
seplist(p, s) : seplist1(p, s) { $1 }
              | {- empty -}    { [] }

-- One or more occurences of p separated by s.
seplist1(p, s) : seplist1rev(p, s) { reverse $1 }

-- Helper for seplist1. Returns reversed list because of using left recursion
-- for performace.
seplist1rev(p, s) : p                  { [$1] }
                  | seplist1(p, s) s p { $3 : $1 }

{

newtype ParseM a = ParseM { runParse :: ErrorT String (Reader String) a }
  deriving (Monad, MonadError String, MonadReader String)

funLang :: [TokenWithSpan] -> ParseM SrcProgram

parseFunLang :: String -> [TokenWithSpan] -> Either String SrcProgram
parseFunLang fileName toks = runReader (runErrorT $ runParse $ funLang toks) fileName

parseError :: [TokenWithSpan] -> ParseM a
parseError toks = throwError $ "FunLang parsing error at " ++ getFirstTokenPosString toks

getFirstTokenPosString :: [TokenWithSpan] -> String
getFirstTokenPosString []    = "end of file"
getFirstTokenPosString (t:_) = show $ srcSpanToPos $ getSrcSpan t

filterMap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filterMap p f = map f . filter p

withFileName :: (String -> a) -> ParseM a
withFileName f = ask >>= \fileName -> return $ f fileName

}

