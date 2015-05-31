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

maybeDefaultExpr :: Type -> TyExpr
maybeDefaultExpr t@(TyRef mt) = NewRefE undefined t (LitE $ NothingLit undefined mt undefined)
maybeDefaultExpr t = LitE $ NothingLit undefined t undefined

