module OOLang.BuiltIn where

import OOLang.AST

builtInFunctions :: [(FunName, Type)]
builtInFunctions =
  [ (FunName "printString", TyArrow TyString TyUnit)
  , (FunName "printBool",   TyArrow TyBool   TyUnit)
  , (FunName "readBool",    TyBool)
  , (FunName "printInt",    TyArrow TyInt    TyUnit)
  , (FunName "readInt",     TyInt)
  , (FunName "printFloat",  TyArrow TyFloat  TyUnit)
  , (FunName "readFloat",   TyFloat)
  ]

