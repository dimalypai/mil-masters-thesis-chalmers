{

-- | Parsing module. Written using Happy.
module MIL.Parser
  ( parseProgram
  , parseExprToks
  , parseMil
  , parseExpr
  ) where

import MIL.Lexer as Lex
import MIL.AST as AST

}

%name      parseProgram program
%name      parseExprToks expr
%tokentype { Token }
%error     { parseError }

%token
  -- Keywords
  alias  { KW_Alias  }
  case   { KW_Case   }
  end    { KW_End    }
  forall { KW_Forall }
  let    { KW_Let    }
  lift   { KW_Lift   }
  in     { KW_In     }
  of     { KW_Of     }
  rec    { KW_Rec    }
  return { KW_Return }
  type   { KW_Type   }
  unit   { KW_Unit   }

  -- Symbols
  '=' { Equal }
  ':' { Colon }

  '\\'  { Lambda    }
  '->'  { Arrow     }
  '/\\' { BigLambda }
  '.'   { Dot       }
  '=>'  { FatArrow  }
  '<-'  { LeftArrow }

  '|'  { Bar        }
  '_'  { Underscore }

  '(' { OpenParen   }
  ')' { CloseParen  }
  '[' { OpenSquare  }
  ']' { CloseSquare }
  '{' { OpenCurly   }
  '}' { CloseCurly  }

  ':::' { TripleColon }

  ',' { Comma     }
  ';' { SemiColon }

  -- Identifiers
  lowerId { LowerId $$ }
  upperId { UpperId $$ }

  -- Literals
  intLit   { Lex.IntLit $$ }
  floatLit { Lex.FloatLit $$ _ }
  charLit  { Lex.CharLit $$ }
%%

program :: { SrcProgram }
program
  : list(typedef)
    list(aliasdef)
    list(fundef) { Program ($1, $2, $3) }

typedef :: { SrcTypeDef }
typedef
  : type upperId list(typevar) '=' seplist1(condef, '|') ';'
      { TypeDef (TypeName $2) $3 $5 }

condef :: { SrcConDef }
condef
  : upperId list(atomsrctype)
      { ConDef (ConName $1) $2 }

aliasdef :: { SrcAliasDef }
aliasdef
  : alias upperId '=' srctype ';'
      { AliasDef (TypeName $2) $4 }

fundef :: { SrcFunDef }
fundef
  : lowerId ':' srctype '=' expr ';'
      { FunDef (FunName $1) $3 $5 }

expr :: { SrcExpr }
expr
  : appexpr { $1 }
  | '\\' varbinder '->' expr { LambdaE $2 $4 }
  | '/\\' typevar '.' expr { TypeLambdaE $2 $4 }
  | let letbinder in expr { LetE (fst $2) (snd $2) $4 }
  | return '[' srctype ']' expr { ReturnE $3 $5 }
  | lift '[' atomsrctype '->' atomsrctype ']' expr { LiftE $7 $3 $5 }
  | let rec seplist1(letbinder, ';') in expr { LetRecE $3 $5 }
  | case expr of list1(casealt) end { CaseE $2 $4 }

-- This production is introduced in order to avoid shift/reduce conflicts
appexpr :: { SrcExpr }
appexpr
  : atomexpr { $1 }
  | appexpr atomexpr { AppE $1 $2 }
  | appexpr '[' srctype ']' { TypeAppE $1 $3 }

atomexpr :: { SrcExpr }
atomexpr
  : literal { LitE $1 }
  | lowerId { VarE (Var $1) }
  | upperId { ConNameE (ConName $1) () }
  | '{' seplist1(expr, ',') '}' { TupleE $2 }
  | '(' expr ')' { $2 }

literal :: { Literal }
literal
  : unit     { UnitLit }
  | intLit   { AST.IntLit $1 }
  | floatLit { AST.FloatLit $1 }
  | charLit  { AST.CharLit $1 }

letbinder :: { (SrcVarBinder, SrcExpr) }
letbinder : varbinder '<-' expr { ($1, $3) }

casealt :: { SrcCaseAlt }
casealt : '|' pattern '=>' expr { CaseAlt ($2, $4) }

pattern :: { SrcPattern }
pattern
  : literal { LitP $1 }
  | varbinder { VarP $1 }
  | upperId list(varbinder) { ConP (ConName $1) $2 }
  | '{' seplist1(varbinder, ',') '}' { TupleP $2 }
  | '_' { DefaultP }

srctype :: { SrcType }
srctype
  : appsrctype { $1 }
  | appsrctype '->' srctype { SrcTyArrow $1 $3 }
  | forall typevar '.' srctype { SrcTyForAll $2 $4 }
  | '{' seplist1(srctype, ',') '}' { SrcTyTuple $2 }
  | appsrctype ':::' appsrctype { SrcTyMonadCons $1 $3 }

-- This production is introduced in order to avoid shift/reduce conflicts
appsrctype :: { SrcType }
appsrctype
  : atomsrctype { $1 }
  | appsrctype atomsrctype { SrcTyApp $1 $2 }

atomsrctype :: { SrcType }
atomsrctype
  : upperId { SrcTyTypeCon (TypeName $1) }
  | '(' srctype ')' { $2 }

typevar :: { TypeVar }
typevar : upperId { TypeVar $1 }

varbinder :: { SrcVarBinder }
varbinder : '(' lowerId ':' srctype ')' { VarBinder (Var $2, $4) }

-- * Helper productions

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

-- | Main parsing function provided by Happy.
parseProgram :: [Token] -> SrcProgram

-- | Expression parsing function provided by Happy.
parseExprToks :: [Token] -> SrcExpr

-- | One of the entry points to the Parser. Works on program text.
parseMil :: String -> SrcProgram
parseMil = parseProgram . lexer

-- | One of the entry points to the Parser. Parses expression. Works on expression string.
parseExpr :: String -> SrcExpr
parseExpr = parseExprToks . lexer

parseError :: [Token] -> a
parseError toks = error $ "MIL parsing error"

}

