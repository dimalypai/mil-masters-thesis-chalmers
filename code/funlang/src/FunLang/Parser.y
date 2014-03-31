{

module FunLang.Parser
  ( parse
  , parseFunLang
  , module FunLang.Parser.ParseError
  ) where

import Control.Monad.Error
import Control.Monad.Reader

import FunLang.Lexer as Lex
import FunLang.AST
import FunLang.AST.SrcAnnotated
import FunLang.SrcSpan
import FunLang.Parser.ParseError

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
  intLit    { $$@(Lex.IntLit     _, _) }
  floatLit  { $$@(Lex.FloatLit _ _, _) }
  stringLit { $$@(Lex.StringLit  _, _) }
%%

program :: { SrcProgram }
program : list1(topdef) {% withFileName $ \fileName ->
                             Program (combineSrcSpans (map getSrcSpan2 $1) fileName)
                                     (filterMap isTypeDef getTypeDef $1)
                                     (filterMap isFunDef  getFunDef  $1) }
        | {- empty -} {% withFileNameM $ \fileName -> throwError $ EmptyProgram fileName }

topdef :: { SrcTopDef }
topdef : typedef { TopTypeDef $1 }
       | fundef  { TopFunDef $1 }

typedef :: { SrcTypeDef }
typedef
  : type upperId list(typevar) '=' seplist1(condef, '|')
      {% withFileName $ \fileName ->
           TypeDef (combineSrcSpans [getTokSrcSpan $1, getSrcSpan (last $5)] fileName)
                   (mkTokSrcSpan $2 fileName, TypeName $ getTokId $2)
                   $3
                   $5 }
  | type lowerId list(typevar) '=' seplist1(condef, '|')
      {% withFileNameM $ \fileName ->
           throwError $ TypeDefLowerId (getTokId $2)
                                       (setSrcSpanFileName (getTokSrcSpan $2) fileName) }
  | type upperId list(typevar) '='
      {% withFileNameM $ \fileName ->
           throwError $ TypeDefNoCons (getTokId $2)
                                      (combineSrcSpans [getTokSrcSpan $1, getTokSrcSpan $4] fileName) }

condef :: { SrcConDef }
condef
  : upperId {% withFileName $ \fileName ->
                 ConDef (combineSrcSpans [getTokSrcSpan $1] fileName)
                        (mkTokSrcSpan $1 fileName, ConName $ getTokId $1)
                        [] }

fundef :: { SrcFunDef }
fundef
  : lowerId ':' {% withFileName $ \fileName ->
                     FunDef (combineSrcSpans [getTokSrcSpan $1, getTokSrcSpan $2] fileName)
                            (mkTokSrcSpan $1 fileName, FunName $ getTokId $1)
                            undefined
                            [] }

typevar :: { SrcTypeVar SrcSpan }
typevar : upperId {% withFileName $ \fileName ->
                       (mkTokSrcSpan $1 fileName, TypeVar $ getTokId $1) }

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

newtype ParseM a = ParseM { runParse :: ErrorT ParseError (Reader FileName) a }
  deriving (Monad, MonadError ParseError, MonadReader FileName)

type FileName = String

funLang :: [TokenWithSpan] -> ParseM SrcProgram

parse :: FileName -> [TokenWithSpan] -> Either ParseError SrcProgram
parse fileName toks = runReader (runErrorT $ runParse $ funLang toks) fileName

parseFunLang :: FileName -> String -> Either ParseError SrcProgram
parseFunLang fileName input = parse fileName (lexer input)

parseError :: [TokenWithSpan] -> ParseM a
parseError toks = withFileNameM $ \fileName ->
  throwError $ GeneralError (getFirstTokenPosString toks fileName)

getFirstTokenPosString :: [TokenWithSpan] -> FileName -> String
getFirstTokenPosString []    fileName = fileName ++ ":end of file"
getFirstTokenPosString (t:_) fileName =
  prPrint $ setSrcPosFileName (srcSpanToPos $ getTokSrcSpan t) fileName

filterMap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filterMap p f = map f . filter p

withFileName :: (String -> a) -> ParseM a
withFileName f = ask >>= \fileName -> return $ f fileName

withFileNameM :: (String -> ParseM a) -> ParseM a
withFileNameM f = ask >>= \fileName -> f fileName

}

