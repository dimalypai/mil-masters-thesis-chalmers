{

module OOLang.Parser
  ( parse
  , parseOOLang
  , module OOLang.Parser.ParseError
  ) where

import Control.Monad.Error
import Control.Monad.Reader

import OOLang.Lexer as Lex
import OOLang.AST
import OOLang.AST.SrcAnnotated
import OOLang.SrcSpan
import OOLang.Parser.ParseError

}

%name      ooLang
%tokentype { TokenWithSpan }
%error     { parseError }
%monad     { ParseM }

%token
  -- Keywords
  class     { $$@(KW_Class,     _) }
  def       { $$@(KW_Def,       _) }
  do        { $$@(KW_Do,        _) }
  else      { $$@(KW_Else,      _) }
  end       { $$@(KW_End,       _) }
  if        { $$@(KW_If,        _) }
  just      { $$@(KW_Just,      _) }
  nothing   { $$@(KW_Nothing,   _) }
  otherwise { $$@(KW_Otherwise, _) }
  private   { $$@(KW_Private,   _) }
  public    { $$@(KW_Public,    _) }
  pure      { $$@(KW_Pure,      _) }
  ref       { $$@(KW_Ref,       _) }
  return    { $$@(KW_Return,    _) }
  static    { $$@(KW_Static,    _) }
  then      { $$@(KW_Then,      _) }
  unit      { $$@(KW_Unit,      _) }
  when      { $$@(KW_When,      _) }
  while     { $$@(KW_While,     _) }
  -- Keywords for types
  Int     { $$@(KW_TyInt,     _) }
  Maybe   { $$@(KW_TyMaybe,   _) }
  Mutable { $$@(KW_TyMutable, _) }
  Ref     { $$@(KW_TyRef,     _) }
  Unit    { $$@(KW_TyUnit,    _) }
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

  '=>' { $$@(FatArrow, _) }
  '->' { $$@(Arrow,    _) }
  '\\' { $$@(Lambda,   _) }

  '.'  { $$@(Dot,            _) }
  '::' { $$@(DoubleColon,    _) }
  '?'  { $$@(Question,       _) }
  '??' { $$@(DoubleQuestion, _) }

  '<-' { $$@(LeftArrow, _) }
  ':=' { $$@(ColonEq,   _) }
  '!'  { $$@(Bang,      _) }

  '&&' { $$@(And, _) }
  '||' { $$@(Or,  _) }

  '(' { $$@(OpenParen,  _) }
  ')' { $$@(CloseParen, _) }

  ';' { $$@(SemiColon, _) }

  -- Identifiers
  lowerId { $$@(LowerId s, _) }
  upperId { $$@(UpperId s, _) }

  -- Literals
  falseLit  { $$@(FalseLit, _) }
  trueLit   { $$@(TrueLit,  _) }
  intLit    { $$@(Lex.IntLit     _, _) }
  floatLit  { $$@(Lex.FloatLit _ _, _) }
  stringLit { $$@(Lex.StringLit  _, _) }
%%

program :: { SrcProgram }
program : list1(topdef) {% withFileName $ \fileName ->
                             Program (combineSrcSpans (map getSrcSpan2 $1) fileName)
                                     (filterMap isClassDef getClassDef $1)
                                     (filterMap isFunDef   getFunDef   $1) }

topdef :: { SrcTopDef }
topdef : classdef { TopClassDef $1 }
       | fundef   { TopFunDef $1 }

classdef :: { SrcClassDef }
classdef
  : class upperId opt(superclass) '=>' end
      {% withFileName $ \fileName ->
           ClassDef (combineSrcSpans [getTokSrcSpan $1, getTokSrcSpan $5] fileName)
                    (mkTokSrcSpan $2 fileName, ClassName $ getTokId $2)
                    $3
                    [] }

fundef :: { SrcFunDef }
fundef
  : def optb(pure) lowerId ':' funtype '=>' end
      {% withFileName $ \fileName ->
           FunDef (combineSrcSpans [getTokSrcSpan $1, getTokSrcSpan $7] fileName)
                  (mkTokSrcSpan $3 fileName, FunName $ getTokId $3)
                  $5
                  []
                  $2 }

type :: { SrcType SrcSpan }
type : Unit {% withFileName $ \fileName -> SrcTyUnit $ mkTokSrcSpan $1 fileName}

varbinder :: { VarBinder SrcSpan }
varbinder
  : '(' lowerId ':' type ')'
      {% withFileName $ \fileName ->
           VarBinder (combineSrcSpans [getTokSrcSpan $1, getTokSrcSpan $5] fileName)
                     (mkTokSrcSpan $2 fileName, Var $ getTokId $2)
                     $4 }

funtype :: { SrcFunType SrcSpan }
funtype
  : funargs type {% withFileName $ \fileName ->
                      SrcFunType (combineSrcSpans
                                    (tokensToSrcSpanList $1 ++ [getSrcSpan $2])
                                    fileName)
                                 $1
                                 $2 }

funargs :: { [VarBinder SrcSpan] }
funargs : {- empty -}                    { [] }
        | seplist1(varbinder, '->') '->' { $1 }

superclass :: { SrcClassName SrcSpan }
superclass : '<' upperId {% withFileName $ \fileName ->
                              (mkTokSrcSpan $2 fileName, ClassName $ getTokId $2) }

-- Helper productions

-- Optional p.
opt(p) : p           { Just $1 }
       | {- empty -} { Nothing }

-- Optional p which returns a boolean indicating its presence.
optb(p) : p           { True  }
        | {- empty -} { False }

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

ooLang :: [TokenWithSpan] -> ParseM SrcProgram

parse :: FileName -> [TokenWithSpan] -> Either ParseError SrcProgram
parse fileName toks = runReader (runErrorT $ runParse $ ooLang toks) fileName

parseOOLang :: FileName -> String -> Either ParseError SrcProgram
parseOOLang fileName input = parse fileName (lexer input)

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

tokensToSrcSpanList :: SrcAnnotated a => [a SrcSpan] -> [SrcSpan]
tokensToSrcSpanList xs = if null xs then [] else [getSrcSpan (head xs)]

}

