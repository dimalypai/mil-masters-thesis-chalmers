{

-- | Parsing module. Written using Happy.
module OOLang.Parser
  ( parse
  , parseOOLang
  , ParseError
  , prPrint
  ) where

import Control.Monad.Error
import Control.Monad.Reader
import Data.Maybe (maybeToList)

import OOLang.Lexer as Lex
import OOLang.AST as AST
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
  false     { $$@(KW_False,     _) }
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
  true      { $$@(KW_True,      _) }
  unit      { $$@(KW_Unit,      _) }
  when      { $$@(KW_When,      _) }
  while     { $$@(KW_While,     _) }
  -- Keywords for types
  Bool    { $$@(KW_TyBool,    _) }
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
  '{' { $$@(OpenCurly,  _) }
  '}' { $$@(CloseCurly, _) }

  ';' { $$@(SemiColon, _) }

  -- Identifiers
  lowerId { $$@(LowerId s, _) }
  upperId { $$@(UpperId s, _) }

  -- Literals
  intLit    { $$@(Lex.IntLit     _, _) }
  floatLit  { $$@(Lex.FloatLit _ _, _) }
  stringLit { $$@(Lex.StringLit  _, _) }

%right '->'
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
  : class upperId opt(superclass) '=>' list(memberdecl) end
      {% withFileName $ \fileName ->
           ClassDef (combineSrcSpans [getTokSrcSpan $1, getTokSrcSpan $6] fileName)
                    (mkTokSrcSpan $2 fileName, ClassName $ getTokId $2)
                    $3 $5 }

memberdecl :: { SrcMemberDecl }
memberdecl : decl ';' {% withFileName $ \fileName ->
                           FieldDecl (combineSrcSpans [getSrcSpan2 $1, getTokSrcSpan $2] fileName)
                                     $1 [] }
           | fundef { MethodDecl (getSrcSpan2 $1)
                                 $1 [] }

fundef :: { SrcFunDef }
fundef
  : def optb(pure) lowerId ':' funtype '=>' list1(stmt) end
      {% withFileName $ \fileName ->
           FunDef (combineSrcSpans [getTokSrcSpan $1, getTokSrcSpan $8] fileName)
                  (mkTokSrcSpan $3 fileName, FunName $ getTokId $3)
                  $5 $7 $2 }

stmt :: { SrcStmt }
stmt
  : expr ';'
      {% withFileName $ \fileName ->
           ExprS (combineSrcSpans [getSrcSpan2 $1, getTokSrcSpan $2] fileName)
                 $1 }
  | decl ';'
      {% withFileName $ \fileName ->
           DeclS (combineSrcSpans [getSrcSpan2 $1, getTokSrcSpan $2] fileName)
                 $1 }
  | lowerId assignop expr ';'
      {% withFileName $ \fileName ->
           AssignS (combineSrcSpans [getTokSrcSpan $1, getTokSrcSpan $4] fileName)
                   $2 (mkTokSrcSpan $1 fileName, Var $ getTokId $1) $3 }

expr :: { SrcExpr }
expr : appexpr { $1 }

-- This production is introduced in order to avoid shift/reduce conflicts
appexpr :: { SrcExpr }
appexpr
  : atomexpr { $1 }
  | appexpr atomexpr
      {% withFileName $ \fileName ->
           BinOpE (combineSrcSpans [getSrcSpan2 $1, getSrcSpan2 $2] fileName)
                  (srcSpanBetween (getSrcSpan2 $1) (getSrcSpan2 $2) fileName, App) $1 $2 }

atomexpr :: { SrcExpr }
atomexpr : literal { LitE $1 }
         | lowerId {% withFileName $ \fileName ->
                        VarE (mkTokSrcSpan $1 fileName)
                             (Var $ getTokId $1) }
         | '(' expr ')' {% withFileName $ \fileName ->
                             ParenE (combineSrcSpans [getTokSrcSpan $1, getTokSrcSpan $3] fileName)
                                    $2 }

literal :: { SrcLiteral }
literal
  : unit {% withFileName $ \fileName ->
              (mkTokSrcSpan $1 fileName, UnitLit) }
  | false {% withFileName $ \fileName ->
               (mkTokSrcSpan $1 fileName, AST.BoolLit False) }
  | true {% withFileName $ \fileName ->
              (mkTokSrcSpan $1 fileName, AST.BoolLit True) }
  | intLit {% withFileName $ \fileName ->
                (mkTokSrcSpan $1 fileName, AST.IntLit $ getIntLitValue $1) }
  | floatLit {% withFileName $ \fileName ->
                  (mkTokSrcSpan $1 fileName, AST.FloatLit (getFloatLitValue $1) (getFloatLitString $1)) }
  | stringLit {% withFileName $ \fileName ->
                   (mkTokSrcSpan $1 fileName, AST.StringLit $ getStringLitValue $1) }

assignop :: { SrcAssignOp }
assignop : '<-' {% withFileName $ \fileName -> (mkTokSrcSpan $1 fileName, AssignMut) }

decl :: { SrcDeclaration }
decl
  : varbinder opt(init)
      {% withFileName $ \fileName ->
           Decl (combineSrcSpans (getSrcSpan $1 : maybeToList (fmap getSrcSpan2 $2)) fileName)
                $1 $2 }

init :: { SrcInit }
init : initop expr {% withFileName $ \fileName ->
                        Init (combineSrcSpans [getSrcSpan2 $1, getSrcSpan2 $2] fileName)
                             $1 $2 }

initop :: { SrcInitOp }
initop : '=' {% withFileName $ \fileName -> (mkTokSrcSpan $1 fileName, InitEqual) }
       | '<-' {% withFileName $ \fileName -> (mkTokSrcSpan $1 fileName, InitMut) }

-- Ref and Mutable may be only the outermost type, but then they can contain
-- anything inside (Maybe, functions, atomic types, nesting of Maybes etc.).
-- This is done to keep things simple and moreover, Ref and Mutable nesting doesn't
-- make so much sense (if we think about normal variables in OO-languages, for example).
type :: { SrcType }
type : maybearrtype { $1 }
     | atomtype     { $1 }
     | Mutable '(' maybearrtype ')'
         {% withFileName $ \fileName ->
              SrcTyMutable (combineSrcSpans [getTokSrcSpan $1, getTokSrcSpan $4] fileName)
                           $3 }
     | Ref '(' maybearrtype ')'
         {% withFileName $ \fileName ->
              SrcTyRef (combineSrcSpans [getTokSrcSpan $1, getTokSrcSpan $4] fileName)
                       $3 }
     | Mutable atomtype
         {% withFileName $ \fileName ->
              SrcTyMutable (combineSrcSpans [getTokSrcSpan $1, getSrcSpan $2] fileName)
                           $2 }
     | Ref atomtype
         {% withFileName $ \fileName ->
              SrcTyRef (combineSrcSpans [getTokSrcSpan $1, getSrcSpan $2] fileName)
                       $2 }
     | '(' type ')'
         {% withFileName $ \fileName ->
              SrcTyParen (combineSrcSpans [getTokSrcSpan $1, getTokSrcSpan $3] fileName)
                         $2 }

-- We allow arbitrary nesting of Maybe types, but they cannot contain Ref and
-- Mutable types inside. This is done because Maybe denotes an immutable type
-- and having anything mutable or a reference inside either violates this
-- notion (with Ref, for example) or is just rather useless (with Mutable).
maybearrtype :: { SrcType }
maybearrtype
  : Maybe atomtype
      {% withFileName $ \fileName ->
           SrcTyMaybe (combineSrcSpans [getTokSrcSpan $1, getSrcSpan $2] fileName)
                      $2 }
  | type '->' type
      {% withFileName $ \fileName ->
           SrcTyArrow (combineSrcSpans [getSrcSpan $1, getSrcSpan $3] fileName)
                      $1
                      $3 }
  | Maybe '(' maybearrtype ')'
      {% withFileName $ \fileName ->
           SrcTyMaybe (combineSrcSpans [getTokSrcSpan $1, getTokSrcSpan $4] fileName)
                      $3 }

atomtype :: { SrcType }
atomtype : Unit    {% withFileName $ \fileName -> SrcTyUnit $ mkTokSrcSpan $1 fileName }
         | Bool    {% withFileName $ \fileName -> SrcTyBool $ mkTokSrcSpan $1 fileName }
         | Int     {% withFileName $ \fileName -> SrcTyInt  $ mkTokSrcSpan $1 fileName }
         | upperId {% withFileName $ \fileName -> SrcTyClass ( mkTokSrcSpan $1 fileName
                                                             , ClassName $ getTokId $1 ) }

varbinder :: { SrcVarBinder }
varbinder
  : lowerId ':' type
      {% withFileName $ \fileName ->
           VarBinder (combineSrcSpans [getTokSrcSpan $1, getSrcSpan $3] fileName)
                     (mkTokSrcSpan $1 fileName, Var $ getTokId $1)
                     $3 }

funtype :: { SrcFunType }
funtype
  : funargs type {% withFileName $ \fileName ->
                      FunType (combineSrcSpans
                                 (srcAnnListToSrcSpanListHead $1 ++ [getSrcSpan $2])
                                 fileName)
                              $1 $2 }

funargs :: { [SrcVarBinder] }
funargs : {- empty -}                 { [] }
        | seplist1(funarg, '->') '->' { $1 }

funarg :: { SrcVarBinder }
funarg
  : '{' varbinder '}'
      {% withFileName $ \fileName ->
           setVarBinderAnn $2 (combineSrcSpans [getTokSrcSpan $1, getTokSrcSpan $3] fileName) }

superclass :: { SrcClassName }
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

-- | Parsing monad.
-- Uses 'ErrorT' for error reporting and 'Reader' for having file name in the
-- environment (to file names in spans after lexing).
newtype ParseM a = ParseM { runParse :: ErrorT ParseError (Reader FileName) a }
  deriving (Monad, MonadError ParseError, MonadReader FileName)

type FileName = String

-- | Main parsing function provided by Happy.
ooLang :: [TokenWithSpan] -> ParseM SrcProgram

-- | One of the entry points to the Parser. Works on list of tokens.
parse :: FileName -> [TokenWithSpan] -> Either ParseError SrcProgram
parse fileName toks = runReader (runErrorT $ runParse $ ooLang toks) fileName

-- | One of the entry points to the Parser. Works on program text.
parseOOLang :: FileName -> String -> Either ParseError SrcProgram
parseOOLang fileName input = parse fileName (lexer input)

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
-- , otherwise - returns a source span of the head.
srcAnnListToSrcSpanListHead :: SrcAnnotated a => [a SrcSpan] -> [SrcSpan]
srcAnnListToSrcSpanListHead xs = if null xs then [] else [getSrcSpan (head xs)]

}

