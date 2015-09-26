module OOLang.Optimiser where

import OOLang.Utils

import qualified MIL.AST as MIL
import qualified MIL.Transformations.MonadLaws as MILTrans
import qualified MIL.Transformations.ConstantFolding as MILTrans
import qualified MIL.Transformations.CaseExpression as MILTrans

optimiseMil :: MIL.TyProgram -> MIL.TyProgram
optimiseMil milProgram =
  milProgram
  |> MILTrans.associativity
  |> MILTrans.leftIdentity
  |> MILTrans.rightIdentity
  |> MILTrans.foldConstants
  |> MILTrans.leftIdentity
  |> MILTrans.foldConstants
  |> MILTrans.eliminateConstantCase

