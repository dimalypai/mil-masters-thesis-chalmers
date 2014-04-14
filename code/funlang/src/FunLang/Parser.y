{

-- | Parsing module. Written using Happy.
module FunLang.Parser
  ( parse
  , parseFunLang
  , ParseError
  , prPrint
  ) where

import Control.Monad.Error
import Control.Monad.Reader

import FunLang.Lexer as Lex
import FunLang.AST as AST
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

%right '->'
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
                   $3 $5 }
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
  : upperId list(confield)
      {% withFileName $ \fileName ->
           ConDef (combineSrcSpans (getTokSrcSpan $1 : srcAnnListToSrcSpanListLast $2) fileName)
                  (mkTokSrcSpan $1 fileName, ConName $ getTokId $1)
                  $2 }

-- Constructor field gets a special production instead of using srctype to
-- remove ambiguity with the type application
confield :: { SrcType }
confield
  : upperId {% withFileName $ \fileName ->
                 SrcTyCon (mkTokSrcSpan $1 fileName, TypeName $ getTokId $1) }
  | '(' srctype ')'
      {% withFileName $ \fileName ->
           SrcTyParen (combineSrcSpans [getTokSrcSpan $1, getTokSrcSpan $3] fileName)
                      $2 }

fundef :: { SrcFunDef }
fundef
  : lowerId ':' srctype list1(funeq) ';'
      {% withFileName $ \fileName ->
           FunDef (combineSrcSpans [getTokSrcSpan $1, getTokSrcSpan $5] fileName)
                  (mkTokSrcSpan $1 fileName, FunName $ getTokId $1)
                  $3 $4 }

funeq :: { SrcFunEq }
funeq : lowerId '=' expr ';'
  {% withFileName $ \fileName ->
       FunEq (combineSrcSpans [getTokSrcSpan $1, getTokSrcSpan $4] fileName)
             (mkTokSrcSpan $1 fileName, FunName $ getTokId $1)
             [] $3 }

expr :: { SrcExpr }
expr : literal { LitE $1 }
     | lowerId {% withFileName $ \fileName ->
                    VarE (mkTokSrcSpan $1 fileName)
                         (Var $ getTokId $1) }

literal :: { SrcLiteral }
literal
  : unit {% withFileName $ \fileName ->
              (mkTokSrcSpan $1 fileName, UnitLit) }
  | intLit {% withFileName $ \fileName ->
                (mkTokSrcSpan $1 fileName, AST.IntLit $ getIntLitValue $1) }
  | floatLit {% withFileName $ \fileName ->
                  (mkTokSrcSpan $1 fileName, AST.FloatLit (getFloatLitValue $1) (getFloatLitString $1)) }
  | stringLit {% withFileName $ \fileName ->
                   (mkTokSrcSpan $1 fileName, AST.StringLit $ getStringLitValue $1) }

srctype :: { SrcType }
srctype
  : apptype { $1 }
  | srctype '->' srctype
      {% withFileName $ \fileName ->
           SrcTyArrow (combineSrcSpans [getSrcSpan $1, getSrcSpan $3] fileName)
                      $1 $3 }
  | forall typevar '.' srctype
      {% withFileName $ \fileName ->
           SrcTyForAll (combineSrcSpans [getTokSrcSpan $1, getSrcSpan $4] fileName)
                       $2 $4 }

apptype :: { SrcType }
apptype
  : upperId {% withFileName $ \fileName ->
                 SrcTyCon (mkTokSrcSpan $1 fileName, TypeName $ getTokId $1) }
  | '(' srctype ')'
      {% withFileName $ \fileName ->
           SrcTyParen (combineSrcSpans [getTokSrcSpan $1, getTokSrcSpan $3] fileName)
                      $2 }
  | apptype '(' srctype ')'
      {% withFileName $ \fileName ->
           SrcTyApp (combineSrcSpans [getSrcSpan $1, getTokSrcSpan $4] fileName)
                    $1
                    (SrcTyParen (combineSrcSpans [getTokSrcSpan $2, getTokSrcSpan $4] fileName) $3) }
  | apptype upperId
      {% withFileName $ \fileName ->
           SrcTyApp (combineSrcSpans [getSrcSpan $1, getTokSrcSpan $2] fileName)
                    $1 (SrcTyCon (mkTokSrcSpan $2 fileName, TypeName $ getTokId $2)) }

typevar :: { SrcTypeVar }
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

-- | Parsing monad.
-- Uses 'ErrorT' for error reporting and 'Reader' for having file name in the
-- environment (to file names in spans after lexing).
newtype ParseM a = ParseM { runParse :: ErrorT ParseError (Reader FileName) a }
  deriving (Monad, MonadError ParseError, MonadReader FileName)

type FileName = String

-- | Main parsing function provided by Happy.
funLang :: [TokenWithSpan] -> ParseM SrcProgram

-- | One of the entry points to the Parser. Works on list of tokens.
parse :: FileName -> [TokenWithSpan] -> Either ParseError SrcProgram
parse fileName toks = runReader (runErrorT $ runParse $ funLang toks) fileName

-- | One of the entry points to the Parser. Works on program text.
parseFunLang :: FileName -> String -> Either ParseError SrcProgram
parseFunLang fileName input = parse fileName (lexer input)

-- | Generates a general parsing error with a source position of the token where the error occured.
parseError :: [TokenWithSpan] -> ParseM a
parseError toks = withFileNameM $ \fileName ->
  throwError $ GeneralError (getFirstTokenPosString toks fileName)

-- | Given a list of tokens and a file name, returns a string representing the source position
-- of the first token in the list (or end of file - if it is empty).
-- Used in error messages.
getFirstTokenPosString :: [TokenWithSpan] -> FileName -> String
getFirstTokenPosString []    fileName = fileName ++ ":end of file"
getFirstTokenPosString (t:_) fileName =
  prPrint $ setSrcPosFileName (srcSpanToPos $ getTokSrcSpan t) fileName

-- | Helper function that does filtering and then mapping.
filterMap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filterMap p f = map f . filter p

-- | Helper function providing a file name.
withFileName :: (FileName -> a) -> ParseM a
withFileName f = ask >>= \fileName -> return $ f fileName

-- | Helper monadic function providing a file name.
withFileNameM :: (FileName -> ParseM a) -> ParseM a
withFileNameM f = ask >>= \fileName -> f fileName

-- | Takes a list of source tree nodes annotated with SrcSpan.
-- If it's empty - returns an empty list
-- , otherwise - returns a source span of the last element.
srcAnnListToSrcSpanListLast :: SrcAnnotated a => [a SrcSpan] -> [SrcSpan]
srcAnnListToSrcSpanListLast xs = if null xs then [] else [getSrcSpan (last xs)]

}

