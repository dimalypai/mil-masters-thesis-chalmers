module MIL.LexerSpec (main, spec) where

import Test.Hspec
import Data.List (intercalate)

import MIL.Lexer

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "lexer" $ do
    it "handles comments correctly" $
      lexer "# this is a comment \n \
           \ type # this is a comment \n \
           \ case  "
        `shouldBe`
      [KW_Type, KW_Case]

    it "recognises all keywords correctly" $
      lexer "case forall let lift in of rec return type unit"
        `shouldBe`
      [ KW_Case, KW_Forall, KW_Let, KW_Lift, KW_In, KW_Of
      , KW_Rec, KW_Return, KW_Type, KW_Unit
      ]

    it "recognises all symbols correctly" $
      lexer "=  :  \\  ->  /\\  .  <- \
           \ |  _  (  )  [  ]  {  }  :::  ,  ;"
        `shouldBe`
      [ Equal, Colon, Lambda, Arrow, BigLambda, Dot, LeftArrow
      , Bar, Underscore, OpenParen, CloseParen, OpenSquare, CloseSquare
      , OpenCurly, CloseCurly, TripleColon, Comma, SemiColon
      ]

    it "recognises all literals correctly" $
      lexer "1  0.1  1e-2  1.0e-2"
        `shouldBe`
      [ IntLit 1, FloatLit 0.1 "0.1", FloatLit 0.01 "1e-2", FloatLit 0.01 "1.0e-2" ]

    it "recognises identifiers correctly" $
      lexer "x T var123 True"
        `shouldBe`
      [ LowerId "x", UpperId "T", LowerId "var123", UpperId "True" ]

    it "shows tokens correctly" $
      intercalate " " (map show
        (lexer "case forall let lift in of rec return type unit \
              \ =  :  \\  ->  /\\  .  <- \
              \ |  _  (  )  [  ]  {  }  :::  ,  ; \
              \ 1 0.1 1.0e-2 1e-2 \
              \ x T var123 True"))
        `shouldBe`
      "case forall let lift in of rec return type unit\
     \ = : \\ -> /\\ . <-\
     \ | _ ( ) [ ] { } ::: , ;\
     \ 1 0.1 1.0e-2 1e-2\
     \ x T var123 True"

