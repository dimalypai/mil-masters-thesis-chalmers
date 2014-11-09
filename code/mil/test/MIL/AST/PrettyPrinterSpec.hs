-- | MIL AST pretty printer tests.
module MIL.AST.PrettyPrinterSpec (main, spec) where

import Test.Hspec
import System.FilePath((</>), (<.>))

import MIL.AST
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
    it "pretty prints data type definitions" $
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
      in testCase baseName ast

    it "pretty prints function definitions" $
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
      in testCase baseName ast

    it "pretty prints data type and function definitions" $
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
      in testCase baseName ast

    it "pretty prints types" $
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
                                 State
                                 (MTyMonadCons
                                    (Error (TyTypeCon (TypeName "String")))
                                    (MTyMonad Id))))
                           (mkTypeVar "A"))
                        (LitE UnitLit)
                    , FunDef
                        (FunName "fun5")
                        (TyTuple [mkSimpleType "Int", mkSimpleType "Float"])
                        (LitE UnitLit)])
      in testCase baseName ast

    it "pretty prints expressions" $
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
                                       (ConNameE (ConName "MkT") undefined)
                                       (mkSimpleType "Int"))
                                    (VarE $ VarBinder (Var "x", undefined)))
                                 (LitE $ CharLit 'c'))))
                    , FunDef
                        (FunName "fun2")
                        (mkSimpleType "Unit")
                        (LetE
                           (VarBinder (Var "x", TyTuple [mkSimpleType "Int", mkSimpleType "Float"]))
                           (ReturnE
                              (MTyMonad IO)
                              (TupleE [LitE $ IntLit 1, LitE $ FloatLit 0.01]))
                           (VarE $ VarBinder (Var "x", undefined)))
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
      in testCase baseName ast

-- * Infrastructure

testCase :: String -> TyProgram -> IO ()
testCase baseName ast = do
  result <- readFile (testDir </> mkFileName baseName)
  prPrint ast `shouldBe` result

mkFileName :: String -> String
mkFileName baseName = baseName <.> "mil"

