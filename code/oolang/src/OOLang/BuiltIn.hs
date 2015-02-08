module OOLang.BuiltIn where

import Data.Maybe (isJust, fromJust)

import OOLang.AST

import qualified MIL.AST as MIL
import qualified MIL.AST.Builder as MIL
import qualified MIL.BuiltIn as MIL

builtInFunctions :: [(FunName, (Type, ReturnType))]
builtInFunctions =
  [ (FunName "printString", (TyArrow TyString TyUnit, ReturnType TyUnit))
  , (FunName "readString",  (TyString, ReturnType TyString))
  , (FunName "printBool",   (TyArrow TyBool TyUnit, ReturnType TyUnit))
  , (FunName "readBool",    (TyBool, ReturnType TyBool))
  , (FunName "printInt",    (TyArrow TyInt TyUnit, ReturnType TyUnit))
  , (FunName "readInt",     (TyInt, ReturnType TyInt))
  , (FunName "printFloat",  (TyArrow TyFloat TyUnit, ReturnType TyUnit))
  , (FunName "readFloat",   (TyFloat, ReturnType TyFloat))
  ]

isBuiltInFunction :: FunName -> Bool
isBuiltInFunction funName = isJust $ lookup funName builtInFunctions

-- | Unsafe. Make sure that there exists such a built-in function.
getBuiltInFunctionType :: FunName -> (Type, ReturnType)
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

pureMonadMil :: MIL.MonadType
pureMonadMil =
  MIL.MTyMonadCons (MIL.SinMonadApp (MIL.SinMonad MIL.Error) exceptionType) $
    MIL.MTyMonad (MIL.SinMonad MIL.NonTerm)

impureMonadMil :: MIL.MonadType
impureMonadMil =
  MIL.MTyMonadCons (MIL.SinMonadApp (MIL.SinMonad MIL.Error) exceptionType) $
    MIL.MTyMonadCons (MIL.SinMonad MIL.NonTerm) $
      MIL.MTyMonadCons (MIL.SinMonad MIL.State) $
        MIL.MTyMonad (MIL.SinMonad MIL.IO)

exceptionType :: MIL.Type
exceptionType = MIL.unitType

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

stringTypeMil :: MIL.Type
stringTypeMil = MIL.TyTypeCon (MIL.TypeName "String")

maybeDefaultExpr :: Type -> TyExpr
maybeDefaultExpr t@(TyRef mt) = NewRefE undefined t (LitE $ NothingLit undefined mt undefined)
maybeDefaultExpr t = LitE $ NothingLit undefined t undefined

-- | TODO: revise
builtInMilFunTypes :: [(MIL.FunName, MIL.SrcType)]
builtInMilFunTypes = []
{-
  [ (MIL.FunName "printString", MIL.TyArrow stringTypeMil (MIL.ioType MIL.unitType))
  , (MIL.FunName "readString",  MIL.ioType stringTypeMil)
  , (MIL.FunName "printBool",   MIL.TyArrow MIL.boolType (MIL.ioType MIL.unitType))
  , (MIL.FunName "readBool",    MIL.TyApp (MIL.TyMonad impureMonadMil) MIL.boolType)
  , (MIL.FunName "printInt",    MIL.TyArrow MIL.intType (MIL.ioType MIL.unitType))
  , (MIL.FunName "readInt",     MIL.TyApp (MIL.TyMonad impureMonadMil) MIL.intType)
  , (MIL.FunName "printFloat",  MIL.TyArrow MIL.floatType (MIL.ioType MIL.unitType))
  , (MIL.FunName "readFloat",   MIL.TyApp (MIL.TyMonad impureMonadMil) MIL.floatType)
  ]
-}
-- | Unsafe. Make sure that there exists such a built-in function.
getMilBuiltInFunType :: MIL.FunName -> MIL.SrcType
getMilBuiltInFunType milFunName = fromJust $ lookup milFunName builtInMilFunTypes

builtInMilFunDefs :: [MIL.SrcFunDef]
builtInMilFunDefs = []
{-
  [ printBoolMilDef
  , readBoolMilDef]
-}
printBoolMilDef :: MIL.SrcFunDef
printBoolMilDef =
  MIL.mkSrcFunDef "printBool" (getMilBuiltInFunType $ MIL.FunName "printBool")
    undefined--(MIL.mkSrcLambda () ())

readBoolMilDef :: MIL.SrcFunDef
readBoolMilDef =
  MIL.mkSrcFunDef "readBool" (getMilBuiltInFunType $ MIL.FunName "readBool")
    undefined--(MIL.mkSrcLet () () ())

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

