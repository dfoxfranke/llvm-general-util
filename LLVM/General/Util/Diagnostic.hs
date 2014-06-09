{-# LANGUAGE DeriveDataTypeable, NoImplicitPrelude, FlexibleContexts #-}

module LLVM.General.Util.Diagnostic (
  LLVMError(..),
  llvmErrorFunctionName, llvmErrorString,

  module LLVM.General.Diagnostic
) where

import LLVM.General.Util.Internal.Diagnostic
import LLVM.General.Diagnostic
