-- | MIL AST pretty printer tests.
module MIL.AST.PrettyPrinterSpec (main, spec) where

import Test.Hspec
import System.FilePath((</>), (<.>))

import MIL.AST
import MIL.AST.Builder
import MIL.PrettyPrinter
import MIL.AST.PrettyPrinter()

-- | To be able to run this module from GHCi.
main :: IO ()
main = hspec spec

-- Configuration

testDir :: FilePath
testDir = "test" </> "prettyprintertests"

-- | Main specification function.
spec :: Spec
spec =
  describe "prPrint" $ do
    describe "pretty prints typed representations of" $ do
      it "data type definitions" $
        let baseName = "DataTypes"
            ast = Program
                    ( [ TypeDef
                          (TypeName "T")
                          []
                          [ConDef (ConName "MkT") []]
                      , TypeDef
                          (TypeName "Tree")
                          [TypeVar "A"]
                          [ ConDef
                              (ConName "Empty")
                              []
                          , ConDef
                              (ConName "Node")
                              [ mkTypeVar "A"
                              , TyApp
                                  (TyTypeCon (TypeName "Tree"))
                                  (mkTypeVar "A")
                              , TyApp
                                  (TyTypeCon (TypeName "Tree"))
                                  (mkTypeVar "A")]]]
                    , [])
        in testCaseTy baseName ast

      it "function definitions" $
        let baseName = "Functions"
            ast = Program
                    ( []
                    , [ FunDef
                          (FunName "fun")
                          (mkSimpleType "Unit")
                          (LitE UnitLit)
                      , FunDef
                          (FunName "fun2")
                          (mkSimpleType "Int")
                          (LitE $ IntLit 2)])
        in testCaseTy baseName ast

      it "data type and function definitions" $
        let baseName = "DataTypesAndFunctions"
            ast = Program
                    ( [TypeDef
                         (TypeName "T")
                         []
                         [ConDef (ConName "MkT") []]]
                    , [ FunDef
                          (FunName "fun")
                          (mkSimpleType "Unit")
                          (LitE UnitLit)
                      , FunDef
                          (FunName "fun2")
                          (mkSimpleType "Int")
                          (LitE $ IntLit 2)])
        in testCaseTy baseName ast

      -- TODO: test src types

      it "types" $
        let baseName = "Types"
            ast = Program
                    ( []
                    , [ FunDef
                          (FunName "fun")
                          (TyForAll (TypeVar "A")
                             (TyArrow (mkTypeVar "A") (mkTypeVar "A")))
                          (LitE UnitLit)
                      , FunDef
                          (FunName "fun2")
                          (TyArrow
                             (TyArrow
                                (TyApp
                                   (TyApp
                                      (TyTypeCon (TypeName "Pair"))
                                      (mkSimpleType "Int"))
                                   (mkSimpleType "Int"))
                                (mkSimpleType "Int"))
                             (TyArrow
                                (mkSimpleType "Int")
                                (mkSimpleType "Unit")))
                          (LitE UnitLit)
                      , FunDef
                          (FunName "fun3")
                          (TyArrow
                             (TyForAll (TypeVar "A")
                                (TyArrow (mkTypeVar "A") (mkTypeVar "A")))
                             (TyArrow
                                (mkSimpleType "Int")
                                (mkSimpleType "Unit")))
                          (LitE UnitLit)
                      , FunDef
                          (FunName "fun4")
                          (TyApp
                             (TyMonad
                                (MTyMonadCons
                                   (SinMonad State)
                                   (MTyMonadCons
                                      (SinMonadApp (SinMonad Error) (TyTypeCon $ TypeName "String"))
                                      (MTyMonad $ SinMonad Id))))
                             (mkTypeVar "A"))
                          (LitE UnitLit)
                      , FunDef
                          (FunName "fun5")
                          (TyTuple [mkSimpleType "Int", mkSimpleType "Float"])
                          (LitE UnitLit)])
        in testCaseTy baseName ast

      -- TODO: let rec
      it "expressions" $
        let baseName = "Expressions"
            ast = Program
                    ( []
                    , [ FunDef
                          (FunName "fun")
                          (mkSimpleType "Unit")
                          (TypeLambdaE
                             (TypeVar "A")
                             (LambdaE
                                (VarBinder (Var "x", TyVar $ TypeVar "A"))
                                (AppE
                                   (AppE
                                      (TypeAppE
                                         (ConNameE (ConName "MkT") (mkSimpleType "T"))
                                         (mkSimpleType "Int"))
                                      (VarE $ VarBinder (Var "x", (mkTypeVar "A"))))
                                   (LitE $ CharLit 'c'))))
                      , FunDef
                          (FunName "fun2")
                          (mkSimpleType "Unit")
                          (LetE
                             (VarBinder (Var "x", TyTuple [mkSimpleType "Int", mkSimpleType "Float"]))
                             (ReturnE
                                (MTyMonad $ SinMonad IO)
                                (TupleE [LitE $ IntLit 1, LitE $ FloatLit 0.01]))
                             (VarE $ VarBinder (Var "x", TyTuple [mkSimpleType "Int", mkSimpleType "Float"])))
                      , FunDef
                          (FunName "fun3")
                          (mkSimpleType "Unit")
                          (CaseE (LitE UnitLit)
                             [ CaseAlt (LitP $ IntLit 0, LitE UnitLit)
                             , CaseAlt (VarP $ VarBinder (Var "y", mkSimpleType "Int"), LitE UnitLit)
                             , CaseAlt ( ConP (ConName "MkA") [ VarBinder (Var "a", mkSimpleType "Int")
                                                              , VarBinder (Var "b", mkSimpleType "Unit")]
                                       , LitE UnitLit)
                             , CaseAlt ( TupleP [ VarBinder (Var "a", mkSimpleType "Int")
                                                , VarBinder (Var "b", mkSimpleType "Unit")]
                                       , LitE UnitLit)
                             , CaseAlt (DefaultP, LitE UnitLit)])])
        in testCaseTy baseName ast

-- * Infrastructure

testCaseTy :: String -> TyProgram -> IO ()
testCaseTy baseName tyProgram = do
  result <- readFile (testDir </> mkFileName baseName)
  prPrint tyProgram `shouldBe` result

mkFileName :: String -> String
mkFileName baseName = baseName <.> "mil"

