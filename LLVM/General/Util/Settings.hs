{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-unrecognised-pragmas #-}

module LLVM.General.Util.Settings(
  RelocationModel,
  pattern RelocationModelDefault,
  pattern RelocationModelStatic,
  pattern RelocationModelPIC,
  pattern RelocationModelDynamicNoPIC,

  CodeModel,
  pattern CodeModelDefault,
  pattern CodeModelJITDefault,
  pattern CodeModelSmall,
  pattern CodeModelKernel,
  pattern CodeModelMedium,
  pattern CodeModelLarge,

  OptimizationLevel,
  pattern OptimizationLevelNone,
  pattern OptimizationLevelLess,
  pattern OptimizationLevelDefault,
  pattern OptimizationLevelAggressive,

  Settings(..),
  defaultOptions, defaultSettings,

  module LLVM.General.Target.Options
  ) where

import qualified LLVM.General.Relocation
import qualified LLVM.General.CodeModel
import qualified LLVM.General.CodeGenOpt
import LLVM.General.Target.Options


type RelocationModel = LLVM.General.Relocation.Model
pattern RelocationModelDefault = LLVM.General.Relocation.Default
pattern RelocationModelStatic = LLVM.General.Relocation.Static
pattern RelocationModelPIC = LLVM.General.Relocation.PIC
pattern RelocationModelDynamicNoPIC = LLVM.General.Relocation.DynamicNoPIC
{-# COMPLETE_PATTERNS RelocationModelDefault && RelocationModelStatic &&
                      RelocationModelPIC && RelationModelDynamicNoPIC #-}

type CodeModel = LLVM.General.CodeModel.Model
pattern CodeModelDefault = LLVM.General.CodeModel.Default
pattern CodeModelJITDefault = LLVM.General.CodeModel.JITDefault
pattern CodeModelSmall = LLVM.General.CodeModel.Small
pattern CodeModelKernel = LLVM.General.CodeModel.Kernel
pattern CodeModelMedium = LLVM.General.CodeModel.Medium
pattern CodeModelLarge = LLVM.General.CodeModel.Large
{-# COMPLETE_PATTERNS CodeModelDefault && CodeModelJITDefault &&
                      CodeModelSmall && CodeModelKernel && CodeModelMedium &&
                      CodeModelLarge #-}

type OptimizationLevel = LLVM.General.CodeGenOpt.Level
pattern OptimizationLevelNone = LLVM.General.CodeGenOpt.None
pattern OptimizationLevelLess = LLVM.General.CodeGenOpt.Less
pattern OptimizationLevelDefault = LLVM.General.CodeGenOpt.Default
pattern OptimizationLevelAggressive = LLVM.General.CodeGenOpt.Aggressive
{-# COMPLETE_PATTERNS OptimizationLevelNone && OptimizationLevelLess &&
                      OptimizationLevelDefault && OptimizationLevelAggressive
  #-}

data Settings = Settings {
  settingsRelocationModel :: RelocationModel,
  settingsCodeModel :: CodeModel,
  settingsOptimizationLevel :: OptimizationLevel,
  settingsOptions :: Options }

defaultOptions :: Options
defaultOptions =
  Options False False False False False False False False False False
  False False False False False False False 0 "" FloatABIDefault
  FloatingPointOperationFusionStandard

defaultSettings :: Settings
defaultSettings = Settings RelocationModelDefault CodeModelDefault
                  OptimizationLevelDefault defaultOptions