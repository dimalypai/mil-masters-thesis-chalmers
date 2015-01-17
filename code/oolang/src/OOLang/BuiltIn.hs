module OOLang.BuiltIn where

import Data.Maybe (isJust, fromJust)

import OOLang.AST

import qualified MIL.AST as MIL
import qualified MIL.BuiltIn as MIL

builtInFunctions :: [(FunName, Type)]
builtInFunctions =
  [ (FunName "printString", TyArrow TyString TyUnit)
  , (FunName "readString",  TyString)
  , (FunName "printBool",   TyArrow TyBool   TyUnit)
  , (FunName "readBool",    TyBool)
  , (FunName "printInt",    TyArrow TyInt    TyUnit)
  , (FunName "readInt",     TyInt)
  , (FunName "printFloat",  TyArrow TyFloat  TyUnit)
  , (FunName "readFloat",   TyFloat)
  ]

isBuiltInFunction :: FunName -> Bool
isBuiltInFunction funName = isJust $ lookup funName builtInFunctions

-- | Unsafe. Make sure that there exists such a built-in function.
getBuiltInFunctionType :: FunName -> Type
getBuiltInFunctionType funName = fromJust $ lookup funName builtInFunctions

-- * Monads

pureMonadMilName :: MIL.TypeName
pureMonadMilName = MIL.TypeName "Pure_M"

pureMonadMil :: MIL.TypeM
pureMonadMil = MIL.MTyAlias pureMonadMilName

pureMonadMilType :: MIL.TypeM
pureMonadMilType =
  MIL.MTyMonadCons (MIL.Error exceptionType) $
    MIL.MTyMonad MIL.NonTerm

impureMonadMilName :: MIL.TypeName
impureMonadMilName = MIL.TypeName "Impure_M"

impureMonadMil :: MIL.TypeM
impureMonadMil = MIL.MTyAlias impureMonadMilName

impureMonadMilType :: MIL.TypeM
impureMonadMilType =
  MIL.MTyMonadCons (MIL.Error exceptionType) $
    MIL.MTyMonadCons MIL.NonTerm $
      MIL.MTyMonadCons MIL.State $
        MIL.MTyMonad MIL.IO

exceptionType :: MIL.Type
exceptionType = MIL.unitType

-- * MIL definitions for built-ins

builtInTypeDefs :: [MIL.TypeDef]
builtInTypeDefs =
  [ MIL.TypeDef (MIL.TypeName "Maybe") [MIL.TypeVar "A"]
      [ MIL.ConDef (MIL.ConName "Nothing") []
      , MIL.ConDef (MIL.ConName "Just")    [MIL.mkTypeVar "A"]]
  , MIL.TypeDef (MIL.TypeName "String") []
      [ MIL.ConDef (MIL.ConName "Empty_Str") []
      , MIL.ConDef (MIL.ConName "Cons_Str") [MIL.charType, MIL.TyTypeCon (MIL.TypeName "String")]]
  ]

stringTypeMil :: MIL.Type
stringTypeMil = MIL.TyTypeCon (MIL.TypeName "String")

builtInAliasDefs :: [MIL.AliasDef]
builtInAliasDefs =
  [ MIL.AliasDef pureMonadMilName   $ MIL.TyMonad pureMonadMilType
  , MIL.AliasDef impureMonadMilName $ MIL.TyMonad impureMonadMilType]

maybeDefaultExpr :: Type -> TyExpr
maybeDefaultExpr t@(TyRef mt) = NewRefE undefined t (LitE $ NothingLit undefined mt undefined)
maybeDefaultExpr t = LitE $ NothingLit undefined t undefined

builtInMilFunTypes :: [(MIL.FunName, MIL.Type)]
builtInMilFunTypes =
  [ (MIL.FunName "printString", MIL.TyArrow stringTypeMil (MIL.ioType MIL.unitType))
  , (MIL.FunName "printBool",   MIL.TyArrow MIL.boolType (MIL.ioType MIL.unitType))
  , (MIL.FunName "readBool",    MIL.TyApp (MIL.TyMonad impureMonadMil) MIL.boolType)
  , (MIL.FunName "printInt",    MIL.TyArrow MIL.intType (MIL.ioType MIL.unitType))
  , (MIL.FunName "readInt",     MIL.TyApp (MIL.TyMonad impureMonadMil) MIL.intType)
  , (MIL.FunName "printFloat",  MIL.TyArrow MIL.floatType (MIL.ioType MIL.unitType))
  , (MIL.FunName "readFloat",   MIL.TyApp (MIL.TyMonad impureMonadMil) MIL.floatType)
  ]

-- | Unsafe. Make sure that there exists such a built-in function.
getMilBuiltInFunType :: MIL.FunName -> MIL.Type
getMilBuiltInFunType milFunName = fromJust $ lookup milFunName builtInMilFunTypes

builtInMilFunDefs :: [MIL.FunDef]
builtInMilFunDefs =
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

  , MIL.FunDef (MIL.FunName "printInt")
      (getMilBuiltInFunType (MIL.FunName "printInt"))
      _

  , MIL.FunDef (MIL.FunName "readInt")
      (getMilBuiltInFunType (MIL.FunName "readInt"))
      _
{-
  , MIL.FunDef (MIL.FunName "printFloat")
      (getMilBuiltInFunType (MIL.FunName "printFloat"))
      ()

  , MIL.FunDef (MIL.FunName "readFloat")
      (getMilBuiltInFunType (MIL.FunName "readFloat"))
      ()-}
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

stringMil :: String -> MIL.Expr
stringMil "" = MIL.ConNameE (MIL.ConName "Empty_Str") stringTypeMil
stringMil (c:cs) =
  MIL.AppE (MIL.AppE (MIL.ConNameE (MIL.ConName "Cons_Str")
                        (MIL.TyArrow MIL.charType
                                     (MIL.TyArrow stringTypeMil stringTypeMil)))
                     (MIL.LitE $ MIL.CharLit c))
           (stringMil cs)

