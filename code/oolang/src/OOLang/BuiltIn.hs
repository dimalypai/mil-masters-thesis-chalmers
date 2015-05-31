module OOLang.BuiltIn where

import Data.Maybe (isJust, fromJust)

import OOLang.AST

import qualified MIL.AST as MIL
import qualified MIL.AST.Builder as MIL
import qualified MIL.BuiltIn as MIL

builtInFunctions :: [(FunName, (Type, ReturnType, Int))]
builtInFunctions =
  [ (FunName "printString", (TyArrow TyString TyUnit, ReturnType TyUnit, 1))
  , (FunName "readString",  (TyString, ReturnType TyString, 0))
  , (FunName "printBool",   (TyArrow TyBool TyUnit, ReturnType TyUnit, 1))
  , (FunName "readBool",    (TyBool, ReturnType TyBool, 0))
  , (FunName "printInt",    (TyArrow TyInt TyUnit, ReturnType TyUnit, 1))
  , (FunName "readInt",     (TyInt, ReturnType TyInt, 0))
  , (FunName "printFloat",  (TyArrow TyFloat TyUnit, ReturnType TyUnit, 1))
  , (FunName "readFloat",   (TyFloat, ReturnType TyFloat, 0))
  ]

isBuiltInFunction :: FunName -> Bool
isBuiltInFunction funName = isJust $ lookup funName builtInFunctions

-- | Unsafe. Make sure that there exists such a built-in function.
getBuiltInFunctionType :: FunName -> (Type, ReturnType, Int)
getBuiltInFunctionType funName = fromJust $ lookup funName builtInFunctions

-- * Monads

pureSrcMonadMil :: MIL.SrcType
pureSrcMonadMil =
  MIL.SrcTyMonadCons (MIL.SrcTyApp (MIL.mkSimpleSrcType "Error") exceptionSrcType)
    (MIL.mkSimpleSrcType "NonTerm")

impureSrcMonadMil :: MIL.SrcType
impureSrcMonadMil =
  MIL.SrcTyMonadCons (MIL.SrcTyApp (MIL.mkSimpleSrcType "Error") exceptionSrcType) $
    MIL.SrcTyMonadCons (MIL.mkSimpleSrcType "NonTerm") $
      MIL.SrcTyMonadCons (MIL.mkSimpleSrcType "State")
        (MIL.mkSimpleSrcType "IO")

impureSrcMonadMilWithStateBase :: MIL.SrcType
impureSrcMonadMilWithStateBase =
  MIL.SrcTyMonadCons (MIL.SrcTyApp (MIL.mkSimpleSrcType "Error") exceptionSrcType) $
    MIL.SrcTyMonadCons (MIL.mkSimpleSrcType "NonTerm")
      (MIL.mkSimpleSrcType "State")

exceptionSrcType :: MIL.SrcType
exceptionSrcType = MIL.mkSimpleSrcType "Unit"

-- * MIL definitions for built-ins

builtInMilTypeDefs :: [MIL.SrcTypeDef]
builtInMilTypeDefs =
  [ MIL.TypeDef (MIL.TypeName "Maybe") [MIL.TypeVar "A"]
      [ MIL.ConDef (MIL.ConName "Nothing") []
      , MIL.ConDef (MIL.ConName "Just")    [MIL.mkSimpleSrcType "A"]]
  , MIL.TypeDef (MIL.TypeName "String") []
      [ MIL.ConDef (MIL.ConName "Empty_Str") []
      , MIL.ConDef (MIL.ConName "Cons_Str") [MIL.mkSimpleSrcType "Char", MIL.mkSimpleSrcType "String"]]
  ]

maybeDefaultExpr :: Type -> TyExpr
maybeDefaultExpr t@(TyRef mt) = NewRefE undefined t (LitE $ NothingLit undefined mt undefined)
maybeDefaultExpr t = LitE $ NothingLit undefined t undefined

builtInMilFunTypes :: [(MIL.FunName, MIL.SrcType)]
builtInMilFunTypes =
  [ (MIL.FunName "printString", MIL.SrcTyArrow (MIL.mkSimpleSrcType "String")
                                               (MIL.SrcTyApp impureSrcMonadMil (MIL.mkSimpleSrcType "Unit")))
  , (MIL.FunName "readString",  MIL.SrcTyApp impureSrcMonadMil (MIL.mkSimpleSrcType "String"))
  , (MIL.FunName "printBool",   MIL.SrcTyArrow (MIL.mkSimpleSrcType "Bool")
                                               (MIL.SrcTyApp impureSrcMonadMil (MIL.mkSimpleSrcType "Unit")))
  , (MIL.FunName "readBool",    MIL.SrcTyApp impureSrcMonadMil (MIL.mkSimpleSrcType "Bool"))
  , (MIL.FunName "printInt",    MIL.SrcTyArrow (MIL.mkSimpleSrcType "Int")
                                               (MIL.SrcTyApp impureSrcMonadMil (MIL.mkSimpleSrcType "Unit")))
  , (MIL.FunName "readInt",     MIL.SrcTyApp impureSrcMonadMil (MIL.mkSimpleSrcType "Int"))
  , (MIL.FunName "printFloat",  MIL.SrcTyArrow (MIL.mkSimpleSrcType "Float")
                                               (MIL.SrcTyApp impureSrcMonadMil (MIL.mkSimpleSrcType "Unit")))
  , (MIL.FunName "readFloat",   MIL.SrcTyApp impureSrcMonadMil (MIL.mkSimpleSrcType "Float"))
  ]

-- | Unsafe. Make sure that there exists such a built-in function.
getMilBuiltInFunType :: MIL.FunName -> MIL.SrcType
getMilBuiltInFunType milFunName = fromJust $ lookup milFunName builtInMilFunTypes

builtInMilFunDefs :: [MIL.SrcFunDef]
builtInMilFunDefs =
  [ printStringMilDef
  , readStringMilDef
  , printBoolMilDef
  , readBoolMilDef
  , printIntMilDef
  , readIntMilDef
  , printFloatMilDef
  , readFloatMilDef
  ]

printStringMilDef :: MIL.SrcFunDef
printStringMilDef =
  MIL.mkSrcFunDef "printString" (getMilBuiltInFunType $ MIL.FunName "printString")
    (MIL.mkSrcLambda (MIL.Var "s_") (MIL.mkSimpleSrcType "String") $ MIL.ReturnE impureSrcMonadMil (MIL.LitE MIL.UnitLit))

readStringMilDef :: MIL.SrcFunDef
readStringMilDef =
  MIL.mkSrcFunDef "readString" (getMilBuiltInFunType $ MIL.FunName "readString")
    (MIL.ReturnE impureSrcMonadMil (MIL.mkSrcConName "Empty_Str"))

printBoolMilDef :: MIL.SrcFunDef
printBoolMilDef =
  MIL.mkSrcFunDef "printBool" (getMilBuiltInFunType $ MIL.FunName "printBool")
    (MIL.mkSrcLambda (MIL.Var "b_") (MIL.mkSimpleSrcType "Bool") $ MIL.ReturnE impureSrcMonadMil (MIL.LitE MIL.UnitLit))

readBoolMilDef :: MIL.SrcFunDef
readBoolMilDef =
  MIL.mkSrcFunDef "readBool" (getMilBuiltInFunType $ MIL.FunName "readBool")
    (MIL.ReturnE impureSrcMonadMil (MIL.mkSrcConName "True"))

printIntMilDef :: MIL.SrcFunDef
printIntMilDef =
  MIL.mkSrcFunDef "printInt" (getMilBuiltInFunType $ MIL.FunName "printInt")
    (MIL.mkSrcLambda (MIL.Var "i_") (MIL.mkSimpleSrcType "Int") $ MIL.ReturnE impureSrcMonadMil (MIL.LitE MIL.UnitLit))

readIntMilDef :: MIL.SrcFunDef
readIntMilDef =
  MIL.mkSrcFunDef "readInt" (getMilBuiltInFunType $ MIL.FunName "readInt")
    (MIL.ReturnE impureSrcMonadMil (MIL.LitE $ MIL.IntLit 1))

printFloatMilDef :: MIL.SrcFunDef
printFloatMilDef =
  MIL.mkSrcFunDef "printFloat" (getMilBuiltInFunType $ MIL.FunName "printFloat")
    (MIL.mkSrcLambda (MIL.Var "f_") (MIL.mkSimpleSrcType "Float") $ MIL.ReturnE impureSrcMonadMil (MIL.LitE MIL.UnitLit))

readFloatMilDef :: MIL.SrcFunDef
readFloatMilDef =
  MIL.mkSrcFunDef "readFloat" (getMilBuiltInFunType $ MIL.FunName "readFloat")
    (MIL.ReturnE impureSrcMonadMil (MIL.LitE $ MIL.FloatLit 1.0))

{-
  [ MIL.FunDef (MIL.FunName "printString")
      (getMilBuiltInFunType (MIL.FunName "printString"))
      (MIL.LambdaE (MIL.VarBinder (MIL.Var "str", stringTypeMil))
         (MIL.CaseE (MIL.VarE $ MIL.VarBinder (MIL.Var "str", stringTypeMil))
            [ MIL.CaseAlt (MIL.ConP (MIL.ConName "Empty_Str") [],
                MIL.ReturnE (MIL.MTyMonad MIL.IO) (MIL.LitE MIL.UnitLit))
            ]))

  , MIL.FunDef (MIL.FunName "printBool")
      (getMilBuiltInFunType (MIL.FunName "printBool"))
      (MIL.LambdaE (MIL.VarBinder (MIL.Var "b", MIL.boolType))
         (MIL.CaseE (MIL.VarE $ MIL.VarBinder (MIL.Var "b", MIL.boolType))
            [ MIL.CaseAlt (MIL.ConP (MIL.ConName "True") [],
                MIL.AppE (MIL.VarE $ MIL.VarBinder ( MIL.Var "printString"
                                                   , getMilBuiltInFunType (MIL.FunName "printString")))
                         (stringMil "true"))
            , MIL.CaseAlt (MIL.ConP (MIL.ConName "False") [],
                MIL.AppE (MIL.VarE $ MIL.VarBinder ( MIL.Var "printString"
                                                   , getMilBuiltInFunType (MIL.FunName "printString")))
                         (stringMil "false"))]))

  , MIL.FunDef (MIL.FunName "readBool")
      (getMilBuiltInFunType (MIL.FunName "readBool"))
      (MIL.LetE (MIL.VarBinder (MIL.Var "c_1", MIL.charType))
         (MIL.VarE $ MIL.VarBinder (MIL.Var "read_char", MIL.getBuiltInFunctionType (MIL.FunName "read_char")))
         (MIL.CaseE (MIL.VarE $ MIL.VarBinder (MIL.Var "c_1", MIL.charType))
            [ readBoolCaseAlt "true" (MIL.ConName "True") 2
            , readBoolCaseAlt "false" (MIL.ConName "False") 2
            , readBoolErrorCaseAlt]))
  ]

readBoolCaseAlt :: String -> MIL.ConName -> Int -> MIL.CaseAlt
readBoolCaseAlt "" conName _ =
  MIL.CaseAlt ( MIL.LitP $ MIL.CharLit ' '
              , MIL.ReturnE impureMonadMil (MIL.ConNameE conName MIL.boolType))
readBoolCaseAlt (c:cs) conName i =
  MIL.CaseAlt ( MIL.LitP $ MIL.CharLit c
              , MIL.LetE (MIL.VarBinder (MIL.Var ("c_" ++ show i), MIL.charType))
                  (MIL.VarE $ MIL.VarBinder (MIL.Var "read_char", MIL.getBuiltInFunctionType (MIL.FunName "read_char")))
                  (MIL.CaseE (MIL.VarE $ MIL.VarBinder (MIL.Var ("c_" ++ show i), MIL.charType))
                     [ readBoolCaseAlt cs conName (i+1)
                     , readBoolErrorCaseAlt]))

readBoolErrorCaseAlt :: MIL.CaseAlt
readBoolErrorCaseAlt =
  MIL.CaseAlt (MIL.DefaultP,
                 MIL.AppE
                   (MIL.TypeAppE
                      (MIL.TypeAppE
                         (MIL.VarE $ MIL.VarBinder ( MIL.Var "throw_error"
                                                   , MIL.getBuiltInFunctionType (MIL.FunName "throw_error")))
                         MIL.unitType)
                      MIL.boolType)
                   (MIL.LitE MIL.UnitLit))
-}

stringMil :: String -> MIL.SrcExpr
stringMil "" = MIL.mkSrcConName "Empty_Str"
stringMil (c:cs) =
  MIL.AppE (MIL.AppE (MIL.mkSrcConName "Cons_Str")
                     (MIL.mkCharLit c))
           (stringMil cs)

