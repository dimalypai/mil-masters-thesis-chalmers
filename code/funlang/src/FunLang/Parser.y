{

-- | Parsing module. Written using Happy.
module FunLang.Parser
  ( parse
  , parseFunLang
  , parseExpr
  , ParseError
  , prPrint
  ) where

import Control.Monad.Error
import Control.Monad.Reader

import FunLang.Lexer as Lex
import FunLang.AST as AST
import FunLang.AST.Helpers
import FunLang.AST.SrcAnnotated
import FunLang.SrcSpan
import FunLang.Parser.ParseError

}

%name      parseProgram  program
%name      parseExprToks expr
%tokentype { TokenWithSpan }
%error     { parseError }
%monad     { ParseM }

%token
  -- Keywords
  case   { $$@(KW_Case,   _) }
  catch  { $$@(KW_Catch,  _) }
  do     { $$@(KW_Do,     _) }
  end    { $$@(KW_End,    _) }
  forall { $$@(KW_Forall, _) }
  of     { $$@(KW_Of,     _) }
  return { $$@(KW_Return, _) }
  throw  { $$@(KW_Throw,  _) }
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
  '=>'  { $$@(FatArrow,  _) }
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

%nonassoc '.' '->'  -- lambdas bind less tightly than others, go to the right as far as possible
%left catch
%left '<' '>' '<=' '>=' '=' '/='
%left '+' '-'
%left '*' '/'
%%

program :: { SrcProgram }
program : list1(topdef) {% withFileName $ \fileName ->
                             Program (combineSrcSpans (map getSrcSpan $1) fileName)
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
                   (TypeName $ getTokId $2, mkTokSrcSpan $2 fileName)
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
  : upperId list(atomsrctype)
      {% withFileName $ \fileName ->
           ConDef (combineSrcSpans (getTokSrcSpan $1 : srcAnnListToSrcSpanListLast $2) fileName)
                  (ConName $ getTokId $1, mkTokSrcSpan $1 fileName)
                  $2 }

fundef :: { SrcFunDef }
fundef
  : lowerId ':' srctype list1(funeq) ';'
      {% withFileName $ \fileName ->
           FunDef (combineSrcSpans [getTokSrcSpan $1, getTokSrcSpan $5] fileName)
                  (FunName $ getTokId $1, mkTokSrcSpan $1 fileName)
                  $3 $4 }

funeq :: { SrcFunEq }
funeq : lowerId list(atompattern) '=' funeqbody ';'
  {% withFileName $ \fileName ->
       FunEq (combineSrcSpans [getTokSrcSpan $1, getTokSrcSpan $5] fileName)
             (FunName $ getTokId $1, mkTokSrcSpan $1 fileName)
             $2 $4 }

funeqbody :: { SrcExpr }
funeqbody
  : expr { $1 }
  | do list1(stmt) end
      {% withFileName $ \fileName ->
           DoE (combineSrcSpans [getTokSrcSpan $1, getTokSrcSpan $3] fileName)
               $2 }

expr :: { SrcExpr }
expr
  : appexpr { $1 }
  | '\\' list1(lambdavarbinder) '->' expr
      {% withFileName $ \fileName ->
           LambdaE (combineSrcSpans [getTokSrcSpan $1, getSrcSpan $4] fileName)
                   $2 $4}
  | '/\\' list1(typevar) '.' expr
      {% withFileName $ \fileName ->
           TypeLambdaE (combineSrcSpans [getTokSrcSpan $1, getSrcSpan $4] fileName)
                       $2 $4 }
  | case expr of list1(casealt) end
      {% withFileName $ \fileName ->
           CaseE (combineSrcSpans [getTokSrcSpan $1, getTokSrcSpan $5] fileName)
                 $2 $4 }
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
  | expr catch expr {% binOp $1 $2 $3 AST.Catch }

-- This production is introduced in order to avoid shift/reduce conflicts
appexpr :: { SrcExpr }
appexpr
  : atomexpr { $1 }
  | appexpr atomexpr
      {% withFileName $ \fileName ->
           BinOpE (combineSrcSpans [getSrcSpan $1, getSrcSpan $2] fileName)
                  (App, srcSpanBetween (getSrcSpan $1) (getSrcSpan $2) fileName) $1 $2 }
  | appexpr '[' srctype ']'
      {% withFileName $ \fileName ->
           TypeAppE (combineSrcSpans [getSrcSpan $1, getTokSrcSpan $4] fileName)
                    $1 $3 }

atomexpr :: { SrcExpr }
atomexpr : literal { LitE $1 }
         | lowerId {% withFileName $ \fileName ->
                        VarE (mkTokSrcSpan $1 fileName)
                             (Var $ getTokId $1) }
         | upperId {% withFileName $ \fileName ->
                        ConNameE (ConName $ getTokId $1, mkTokSrcSpan $1 fileName) }
         | throw '[' srctype ']' {% withFileName $ \fileName ->
                                      ThrowE (mkTokSrcSpan $1 fileName) $3 }
         | '(' expr ')' {% withFileName $ \fileName ->
                             ParenE (combineSrcSpans [getTokSrcSpan $1, getTokSrcSpan $3] fileName)
                                    $2 }

literal :: { SrcLiteral }
literal
  : unit {% withFileName $ \fileName ->
              (UnitLit, mkTokSrcSpan $1 fileName) }
  | intLit {% withFileName $ \fileName ->
                (AST.IntLit $ getIntLitValue $1, mkTokSrcSpan $1 fileName) }
  | floatLit {% withFileName $ \fileName ->
                  (AST.FloatLit (getFloatLitValue $1) (getFloatLitString $1), mkTokSrcSpan $1 fileName) }
  | stringLit {% withFileName $ \fileName ->
                   (AST.StringLit $ getStringLitValue $1, mkTokSrcSpan $1 fileName) }

casealt :: { SrcCaseAlt }
casealt
  : '|' pattern '=>' expr {% withFileName $ \fileName ->
                               CaseAlt (combineSrcSpans [getTokSrcSpan $1, getSrcSpan $4] fileName)
                                       $2 $4 }

pattern :: { SrcPattern }
pattern
  : atompattern { $1 }
  | varbinder   { VarP $1 }
  | upperId list1(atompattern)
      {% withFileName $ \fileName ->
           ConP (combineSrcSpans [getTokSrcSpan $1, getSrcSpan (last $2)] fileName)
                (ConName $ getTokId $1, mkTokSrcSpan $1 fileName)
                $2 }

atompattern :: { SrcPattern }
atompattern
  : literal { LitP $1 }
  | upperId {% withFileName $ \fileName ->
                 ConP (mkTokSrcSpan $1 fileName)
                      (ConName $ getTokId $1, mkTokSrcSpan $1 fileName)
                      [] }
  | '_'     {% withFileName $ \fileName -> DefaultP (mkTokSrcSpan $1 fileName) }
  | '(' pattern ')'
      {% withFileName $ \fileName ->
           ParenP (combineSrcSpans [getTokSrcSpan $1, getTokSrcSpan $3] fileName)
                  $2 }

stmt :: { SrcStmt }
stmt
  : expr ';' {% withFileName $ \fileName ->
                  ExprS (combineSrcSpans [getSrcSpan $1, getTokSrcSpan $2] fileName)
                        $1 }
  | varbinder '<-' expr ';'
      {% withFileName $ \fileName ->
           BindS (combineSrcSpans [getSrcSpan $1, getTokSrcSpan $4] fileName)
                 $1 $3 }
  | return expr ';'
      {% withFileName $ \fileName ->
           ReturnS (combineSrcSpans [getTokSrcSpan $1, getTokSrcSpan $3] fileName)
                   $2 }

varbinder :: { SrcVarBinder }
varbinder
  : lowerId ':' srctype
      {% withFileName $ \fileName ->
           VarBinder (combineSrcSpans [getTokSrcSpan $1, getSrcSpan $3] fileName)
                     (Var $ getTokId $1, mkTokSrcSpan $1 fileName) $3}

lambdavarbinder :: { SrcVarBinder }
lambdavarbinder : '(' varbinder ')' { $2 }

srctype :: { SrcType }
srctype
  : appsrctype { $1 }
  | appsrctype '->' srctype
      {% withFileName $ \fileName ->
           SrcTyArrow (combineSrcSpans [getSrcSpan $1, getSrcSpan $3] fileName)
                      $1 $3 }
  | forall typevar '.' srctype
      {% withFileName $ \fileName ->
           SrcTyForAll (combineSrcSpans [getTokSrcSpan $1, getSrcSpan $4] fileName)
                       $2 $4 }

-- This production is introduced in order to avoid shift/reduce conflicts
appsrctype :: { SrcType }
appsrctype
  : atomsrctype { $1 }
  | appsrctype atomsrctype {% withFileName $ \fileName ->
                                SrcTyApp (combineSrcSpans [getSrcSpan $1, getSrcSpan $2] fileName)
                                         $1 $2 }

atomsrctype :: { SrcType }
atomsrctype
  : upperId {% withFileName $ \fileName ->
                 SrcTyCon (TypeName $ getTokId $1, mkTokSrcSpan $1 fileName) }
  | '(' srctype ')'
      {% withFileName $ \fileName ->
           SrcTyParen (combineSrcSpans [getTokSrcSpan $1, getTokSrcSpan $3] fileName)
                      $2 }

typevar :: { SrcTypeVar }
typevar : upperId {% withFileName $ \fileName ->
                       (TypeVar $ getTokId $1, mkTokSrcSpan $1 fileName) }

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
parseProgram :: [TokenWithSpan] -> ParseM SrcProgram

-- | Expression parsing function provided by Happy.
parseExprToks :: [TokenWithSpan] -> ParseM SrcExpr

-- | One of the entry points to the Parser. Works on list of tokens.
parse :: FileName -> [TokenWithSpan] -> Either ParseError SrcProgram
parse fileName toks = runReader (runErrorT $ runParse $ parseProgram toks) fileName

-- | One of the entry points to the Parser. Works on program text.
parseFunLang :: FileName -> String -> Either ParseError SrcProgram
parseFunLang fileName input = parse fileName (lexer input)

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
-- , otherwise - returns a source span of the last element.
srcAnnListToSrcSpanListLast :: SrcAnnotated a => [a SrcSpan] -> [SrcSpan]
srcAnnListToSrcSpanListLast xs = if null xs then [] else [getSrcSpan (last xs)]

-- | Parsing action asbtraction for binary operations.
-- Takes first operand, operation token, second operand, and operation constructor.
binOp :: SrcExpr -> TokenWithSpan -> SrcExpr -> BinOp -> ParseM SrcExpr
binOp e1 opTok e2 op =
  withFileName $ \fileName ->
    BinOpE (combineSrcSpans [getSrcSpan e1, getSrcSpan e2] fileName)
           (op, mkTokSrcSpan opTok fileName)
           e1 e2

}

