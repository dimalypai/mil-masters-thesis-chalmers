module FunLang.Optimiser where

import FunLang.Utils

import qualified MIL.AST as MIL
import qualified MIL.Transformations.MonadLaws as MILTrans
import qualified MIL.Transformations.ConstantFolding as MILTrans

optimiseMil :: MIL.TyProgram -> MIL.TyProgram
optimiseMil milProgram =
  milProgram
  |> MILTrans.associativity
  |> MILTrans.leftIdentity
  |> MILTrans.rightIdentity
  |> MILTrans.associativity
  |> MILTrans.associativity
  |> MILTrans.foldConstants
  |> MILTrans.leftIdentity
  |> MILTrans.foldConstants

