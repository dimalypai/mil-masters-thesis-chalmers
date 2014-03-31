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
import OOLang.SrcSpan
import OOLang.Parser.ParseError

}

%name      ooLang
%tokentype { TokenWithSpan }
%error     { parseError }
%monad     { ParseM }

%token
  def { undefined }
%%

program :: { SrcProgram }
program : { undefined }

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

withFileName :: (String -> a) -> ParseM a
withFileName f = ask >>= \fileName -> return $ f fileName

withFileNameM :: (String -> ParseM a) -> ParseM a
withFileNameM f = ask >>= \fileName -> f fileName

}

