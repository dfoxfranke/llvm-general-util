{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
module LLVM.General.Util.LibraryInfo( 
  getLibraryFunction, getLibraryFunctionName,
  setLibraryFunctionAvailableWithName,
  
  module LLVM.General.Target.LibraryFunction
  ) where

import ClassyPrelude

import Control.Monad.Base(MonadBase(..))
import Control.Monad.Trans.Control(MonadBaseControl(..))
import qualified LLVM.General.Target as T
import LLVM.General.Target.LibraryFunction
import LLVM.General.Util.Internal.Diagnostic
import LLVM.General.Util.Internal.L

getLibraryFunction :: (MonadBaseControl IO m)
                      => String -> L b m (Maybe LibraryFunction)
getLibraryFunction name =
  do li <- getLibraryInfo
     liftBase $ catchInternal 'T.getLibraryFunction $
       T.getLibraryFunction li name

getLibraryFunctionName :: (MonadBaseControl IO m) 
                          => LibraryFunction -> L b m String
getLibraryFunctionName lf = 
  do li <- getLibraryInfo
     liftBase $ catchInternal 'T.getLibraryFunctionName $
       T.getLibraryFunctionName li lf

setLibraryFunctionAvailableWithName :: (MonadBaseControl IO m)
                                       => LibraryFunction -> String
                                       -> L b m ()
setLibraryFunctionAvailableWithName lf name =
  do li <- getLibraryInfo
     liftBase $ catchInternal 'T.setLibraryFunctionAvailableWithName $
       T.setLibraryFunctionAvailableWithName li lf name