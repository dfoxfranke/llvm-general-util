{-# LANGUAGE NoImplicitPrelude, DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module LLVM.General.Util.PassSetSpec (
  PassSetSpec(..),
  defaultPassSetSpec, defaultCuratedPassSetSpec,

  module LLVM.General.Transforms

  ) where

import ClassyPrelude
import Data.Data(Data)

import LLVM.General.Transforms

data PassSetSpec
  = PassSetSpec { pssTransforms :: [Pass] }
  | CuratedPassSetSpec { pssOptLevel :: Maybe Word,
                       pssSizeLevel :: Maybe Word,
                       pssUnitAtATime :: Maybe Bool,
                       pssSimplifyLibCalls :: Maybe Bool,
                       pssUseInlinerWithThreshold :: Maybe Word }
  deriving (Eq,Ord,Show,Typeable,Data)

defaultPassSetSpec :: PassSetSpec
defaultPassSetSpec = PassSetSpec []

defaultCuratedPassSetSpec :: PassSetSpec
defaultCuratedPassSetSpec =
  CuratedPassSetSpec Nothing Nothing Nothing Nothing Nothing