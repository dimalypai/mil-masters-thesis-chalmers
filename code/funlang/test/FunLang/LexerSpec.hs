-- | Lexer tests.
module FunLang.LexerSpec (main, spec) where

import Test.Hspec
import Data.List (intercalate)

import FunLang.Lexer
import FunLang.SrcSpan

-- | To be able to run this module from GHCi.
main :: IO ()
main = hspec spec

-- | Main specification function.
spec :: Spec
spec =
  describe "lexer" $ do
    it "handles comments correctly" $
      map getToken
        (lexer "# this is a comment \n \
           \ type # this is a comment \n \
           \ case  ")
        `shouldBe`
      [KW_Type, KW_Case]

    it "recognises all keywords correctly" $
      map getToken
        (lexer "case do end forall of return type unit")
        `shouldBe`
      [ KW_Case, KW_Do, KW_End, KW_Forall
      , KW_Of, KW_Return, KW_Type, KW_Unit
      ]

    it "recognises all symbols correctly" $
      map getToken
        (lexer "=  :  +  -  *  /  %  <  >  <=  >=  /= \
              \ \\  ->  /\\  .  =>  <- \
              \ |  _  &&  ||  (  )  [  ]  ;")
        `shouldBe`
      [ Equal, Colon, Plus, Minus, Star, Slash, Percent, Less, Greater
      , LessEq, GreaterEq, NotEq, Lambda, Arrow, BigLambda, Dot, FatArrow, LeftArrow
      , Bar, Underscore, And, Or, OpenParen, CloseParen, OpenSquare, CloseSquare
      , SemiColon
      ]

    it "recognises all literals correctly" $
      map getToken
        (lexer "1  0.1  1e-2  1.0e-2  \"str\"")
        `shouldBe`
      [ IntLit 1, FloatLit 0.1 "0.1", FloatLit 0.01 "1e-2"
      , FloatLit 0.01 "1.0e-2", StringLit "str"
      ]

    it "recognises identifiers correctly" $
      map getToken
        (lexer "x T var123 True")
        `shouldBe`
      [ LowerId "x", UpperId "T", LowerId "var123", UpperId "True" ]

    it "handles source span correctly" $
      lexer "case \n   var"
        `shouldBe`
      [ (KW_Case, mkSrcSpan "NONAME" 1 1 1 4)
      , (LowerId "var", mkSrcSpan "NONAME" 2 4 2 6)
      ]

    it "shows tokens correctly" $
      intercalate " " (map (show . getToken)
        (lexer "case do end forall of return type unit \
              \ =  :  +  -  *  /  %  <  >  <=  >=  /= \
              \ \\  ->  /\\  .  =>  <- \
              \ |  _  &&  ||  (  )  [  ]  ; \
              \ 1 0.1 1.0e-2 1e-2 \"str\" \
              \ x T var123 True"))
        `shouldBe`
      "case do end forall of return type unit\
     \ = : + - * / % < > <= >= /=\
     \ \\ -> /\\ . => <-\
     \ | _ && || ( ) [ ] ;\
     \ 1 0.1 1.0e-2 1e-2 \"str\"\
     \ x T var123 True"

