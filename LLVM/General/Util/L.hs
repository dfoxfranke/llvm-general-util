{-# OPTIONS_GHC -Wall #-}
module LLVM.General.Util.L ( 
  L(),
  getPlatform, getSettings, getSettingsMaybe, getSettingsOrDefault,
  getDataLayout, getDataLayoutMaybe, newName,
  
  runL, runAnyL, runDefaultL, runNativeL, runVoidL,
  runInL, runAnyInL, runVoidInL
  ) where

import LLVM.General.Util.Internal.L