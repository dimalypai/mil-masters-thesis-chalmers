module FunLang.Optimiser where

import FunLang.Utils

import qualified MIL.AST as MIL
import qualified MIL.Transformations.MonadLaws as MILTrans

optimiseMil :: MIL.Program -> MIL.Program
optimiseMil milProgram = milProgram
                      |> MILTrans.associativity
                      |> MILTrans.leftIdentity
                      |> MILTrans.rightIdentity

