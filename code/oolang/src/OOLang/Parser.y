{

-- | Parsing module. Written using Happy.
module OOLang.Parser
  ( parse
  , parseOOLang
  , parseExpr
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

%name      parseProgram program
%name      parseExprToks expr
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
  Float   { $$@(KW_TyFloat,   _) }
  Int     { $$@(KW_TyInt,     _) }
  Maybe   { $$@(KW_TyMaybe,   _) }
  Mutable { $$@(KW_TyMutable, _) }
  Pure    { $$@(KW_TyPure,    _) }
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

  '(' { $$@(OpenParen,   _) }
  ')' { $$@(CloseParen,  _) }
  '{' { $$@(OpenCurly,   _) }
  '}' { $$@(CloseCurly,  _) }
  '[' { $$@(OpenSquare,  _) }
  ']' { $$@(CloseSquare, _) }

  ';' { $$@(SemiColon, _) }

  -- Identifiers
  lowerId { $$@(LowerId s, _) }
  upperId { $$@(UpperId s, _) }

  -- Literals
  intLit    { $$@(Lex.IntLit     _, _) }
  floatLit  { $$@(Lex.FloatLit _ _, _) }
  stringLit { $$@(Lex.StringLit  _, _) }

%right '->'

%right '??'  -- there is no real reason for it not to be left as well
%left '<' '>' '<=' '>=' '=' '/='
%left '+' '-'
%left '*' '/'
%left '.' '?'
%right '!'
%%

program :: { SrcProgram }
program : list1(topdef) {% withFileName $ \fileName ->
                             Program (combineSrcSpans (map getSrcSpan $1) fileName)
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
                    (ClassName $ getTokId $2, mkTokSrcSpan $2 fileName)
                    $3 $5 }

memberdecl :: { SrcMemberDecl }
memberdecl : decl ';' {% withFileName $ \fileName ->
                           FieldMemberDecl $
                             FieldDecl (combineSrcSpans [getSrcSpan $1, getTokSrcSpan $2] fileName)
                                       $1 [] }
           | fundef { MethodMemberDecl $
                        MethodDecl (getSrcSpan $1)
                                   $1 [] }

fundef :: { SrcFunDef }
fundef
  : def lowerId ':' funtype '=>' list1(stmt) end
      {% withFileName $ \fileName ->
           FunDef (combineSrcSpans [getTokSrcSpan $1, getTokSrcSpan $7] fileName)
                  (FunName $ getTokId $2, mkTokSrcSpan $2 fileName)
                  $4 $6 }

stmt :: { SrcStmt }
stmt
  : expr ';'
      {% withFileName $ \fileName ->
           ExprS (combineSrcSpans [getSrcSpan $1, getTokSrcSpan $2] fileName)
                 $1 }
  | decl ';'
      {% withFileName $ \fileName ->
           DeclS (combineSrcSpans [getSrcSpan $1, getTokSrcSpan $2] fileName)
                 $1 }
  | lowerId assignop expr ';'
      {% withFileName $ \fileName ->
           AssignS (combineSrcSpans [getTokSrcSpan $1, getTokSrcSpan $4] fileName)
                   $2 (Var $ getTokId $1, mkTokSrcSpan $1 fileName) $3 }
  | when expr do list(stmt) otherwise list(stmt) end ';'
      {% withFileName $ \fileName ->
           WhenS (combineSrcSpans [getTokSrcSpan $1, getTokSrcSpan $8] fileName)
                 $2 $4 $6 }
  | while expr do list(stmt) end ';'
      {% withFileName $ \fileName ->
           WhileS (combineSrcSpans [getTokSrcSpan $1, getTokSrcSpan $6] fileName)
                  $2 $4 }

expr :: { SrcExpr }
expr
  : appexpr { $1 }
  | just atomexpr {% withFileName $ \fileName ->
                       JustE (combineSrcSpans [getTokSrcSpan $1, getSrcSpan $2] fileName)
                             $2 }
  | expr '??' expr {% binOp $1 $2 $3 NothingCoalesce }
  | expr '+' expr  {% binOp $1 $2 $3 Add }
  | expr '-' expr  {% binOp $1 $2 $3 Sub }
  | expr '*' expr  {% binOp $1 $2 $3 Mul }
  | expr '/' expr  {% binOp $1 $2 $3 Div }
  | expr '<' expr  {% binOp $1 $2 $3 AST.Less }
  | expr '>' expr  {% binOp $1 $2 $3 AST.Greater }
  | expr '<=' expr {% binOp $1 $2 $3 AST.LessEq }
  | expr '>=' expr {% binOp $1 $2 $3 AST.GreaterEq }
  | expr '=' expr  {% binOp $1 $2 $3 AST.Equal }
  | expr '/=' expr {% binOp $1 $2 $3 AST.NotEq }

-- This production is introduced in order to avoid shift/reduce conflicts
appexpr :: { SrcExpr }
appexpr
  : atomexpr { $1 }
  | appexpr atomexpr
      {% withFileName $ \fileName ->
           BinOpE (combineSrcSpans [getSrcSpan $1, getSrcSpan $2] fileName)
                  (App, srcSpanBetween (getSrcSpan $1) (getSrcSpan $2) fileName) $1 $2 }

atomexpr :: { SrcExpr }
atomexpr
  : literal { LitE $1 }
  | lowerId {% withFileName $ \fileName ->
                 VarE (mkTokSrcSpan $1 fileName)
                      (Var $ getTokId $1) }
  | '(' expr ')' {% withFileName $ \fileName ->
                      ParenE (combineSrcSpans [getTokSrcSpan $1, getTokSrcSpan $3] fileName)
                             $2 }
  | atomexpr '.' lowerId
      {% withFileName $ \fileName ->
           MemberAccessE (combineSrcSpans [getSrcSpan $1, getTokSrcSpan $3] fileName)
                         $1
                         (FunName $ getTokId $3, mkTokSrcSpan $3 fileName) }
  | atomexpr '?' lowerId
      {% withFileName $ \fileName ->
           MemberAccessMaybeE (combineSrcSpans [getSrcSpan $1, getTokSrcSpan $3] fileName)
                              $1
                              (FunName $ getTokId $3, mkTokSrcSpan $3 fileName) }
  | upperId '.' lowerId
      {% withFileName $ \fileName ->
           ClassAccessE (combineSrcSpans [getTokSrcSpan $1, getTokSrcSpan $3] fileName)
                        (ClassName $ getTokId $1, mkTokSrcSpan $1 fileName)
                        (FunName $ getTokId $3, mkTokSrcSpan $3 fileName) }
  | '!' atomexpr {% withFileName $ \fileName ->
                      DerefE (combineSrcSpans [getTokSrcSpan $1, getSrcSpan $2] fileName)
                             $2 }

literal :: { SrcLiteral }
literal
  : unit {% withFileName $ \fileName ->
              UnitLit (mkTokSrcSpan $1 fileName) }
  | false {% withFileName $ \fileName ->
               AST.BoolLit (mkTokSrcSpan $1 fileName) False }
  | true {% withFileName $ \fileName ->
              AST.BoolLit (mkTokSrcSpan $1 fileName) True }
  | intLit {% withFileName $ \fileName ->
                AST.IntLit (mkTokSrcSpan $1 fileName) (getIntLitValue $1) }
  | floatLit {% withFileName $ \fileName ->
                  AST.FloatLit (mkTokSrcSpan $1 fileName) (getFloatLitValue $1) (getFloatLitString $1) }
  | stringLit {% withFileName $ \fileName ->
                   AST.StringLit (mkTokSrcSpan $1 fileName) (getStringLitValue $1) }
  | nothing '[' type ']'
      {% withFileName $ \fileName ->
           NothingLit (combineSrcSpans [getTokSrcSpan $1, getTokSrcSpan $4] fileName) $3 }

assignop :: { SrcAssignOp }
assignop : '<-' {% withFileName $ \fileName -> (AssignMut, mkTokSrcSpan $1 fileName) }

decl :: { SrcDeclaration }
decl
  : varbinder opt(init)
      {% withFileName $ \fileName ->
           Decl (combineSrcSpans (getSrcSpan $1 : maybeToList (fmap getSrcSpan $2)) fileName)
                $1 $2 }

init :: { SrcInit }
init : initop expr {% withFileName $ \fileName ->
                        Init (combineSrcSpans [getSrcSpan $1, getSrcSpan $2] fileName)
                             $1 $2 }

initop :: { SrcInitOp }
initop : '=' {% withFileName $ \fileName -> (InitEqual, mkTokSrcSpan $1 fileName) }
       | '<-' {% withFileName $ \fileName -> (InitMut, mkTokSrcSpan $1 fileName) }

-- For simplicity we parse much more generally than it is allowed by the language.
type :: { SrcType }
type
  : atomtype  { $1 }
  | type '->' type
      {% withFileName $ \fileName ->
           SrcTyArrow (combineSrcSpans [getSrcSpan $1, getSrcSpan $3] fileName)
                      $1
                      $3 }
  | Pure atomtype
      {% withFileName $ \fileName ->
           SrcTyPure (combineSrcSpans [getTokSrcSpan $1, getSrcSpan $2] fileName)
                     $2 }
  | Maybe atomtype
      {% withFileName $ \fileName ->
           SrcTyMaybe (combineSrcSpans [getTokSrcSpan $1, getSrcSpan $2] fileName)
                      $2 }
  | Mutable atomtype
      {% withFileName $ \fileName ->
           SrcTyMutable (combineSrcSpans [getTokSrcSpan $1, getSrcSpan $2] fileName)
                        $2 }
  | Ref atomtype
      {% withFileName $ \fileName ->
           SrcTyRef (combineSrcSpans [getTokSrcSpan $1, getSrcSpan $2] fileName)
                    $2 }

atomtype :: { SrcType }
atomtype
  : Unit    {% withFileName $ \fileName -> SrcTyUnit  $ mkTokSrcSpan $1 fileName }
  | Bool    {% withFileName $ \fileName -> SrcTyBool  $ mkTokSrcSpan $1 fileName }
  | Int     {% withFileName $ \fileName -> SrcTyInt   $ mkTokSrcSpan $1 fileName }
  | Float   {% withFileName $ \fileName -> SrcTyFloat $ mkTokSrcSpan $1 fileName }
  | upperId {% withFileName $ \fileName -> SrcTyClass ( ClassName $ getTokId $1
                                                      , mkTokSrcSpan $1 fileName ) }
  | '(' type ')'
      {% withFileName $ \fileName ->
           SrcTyParen (combineSrcSpans [getTokSrcSpan $1, getTokSrcSpan $3] fileName)
                      $2 }

varbinder :: { SrcVarBinder }
varbinder
  : lowerId ':' type
      {% withFileName $ \fileName ->
           VarBinder (combineSrcSpans [getTokSrcSpan $1, getSrcSpan $3] fileName)
                     (Var $ getTokId $1, mkTokSrcSpan $1 fileName)
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
                              (ClassName $ getTokId $2, mkTokSrcSpan $2 fileName) }

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
parseProgram :: [TokenWithSpan] -> ParseM SrcProgram

-- | Expression parsing function provided by Happy.
parseExprToks :: [TokenWithSpan] -> ParseM SrcExpr

-- | One of the entry points to the Parser. Works on list of tokens.
parse :: FileName -> [TokenWithSpan] -> Either ParseError SrcProgram
parse fileName toks = runReader (runErrorT $ runParse $ parseProgram toks) fileName

-- | One of the entry points to the Parser. Works on program text.
parseOOLang :: FileName -> String -> Either ParseError SrcProgram
parseOOLang fileName input = parse fileName (lexer input)

-- | One of the entry points to the Parser. Parses expression. Works on list of tokens.
parseExpr :: FileName -> [TokenWithSpan] -> Either ParseError SrcExpr
parseExpr fileName toks = runReader (runErrorT $ runParse $ parseExprToks toks) fileName

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

-- | Parsing action asbtraction for binary operations.
-- Takes first operand, operation token, second operand, and operation constructor.
binOp :: SrcExpr -> TokenWithSpan -> SrcExpr -> BinOp -> ParseM SrcExpr
binOp e1 opTok e2 op =
  withFileName $ \fileName ->
    BinOpE (combineSrcSpans [getSrcSpan e1, getSrcSpan e2] fileName)
           (op, mkTokSrcSpan opTok fileName)
           e1 e2

}

