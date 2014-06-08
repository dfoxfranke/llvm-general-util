{-# LANGUAGE DeriveDataTypeable, NoImplicitPrelude, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}
module LLVM.General.Util.Internal.Diagnostic (
  LLVMError(..),
  llvmErrorFunctionName, llvmErrorString,
  
  errorToIO, diagToIO, catchInternal, catchCaller, catchCallerError,
  catchCallerDiag,
  
  module LLVM.General.Diagnostic
) where

import ClassyPrelude
import Control.Exception.Lifted(catches, Handler(..))
import Control.Monad.Trans.Control(MonadBaseControl(..))
import Control.Monad.Trans.Error(ErrorT(..))
import Language.Haskell.TH(Name)

import LLVM.General.Diagnostic

data LLVMError = LLVMError Name String
               | LLVMInternalError Name String
               | LLVMDiagnostic Name Diagnostic
               deriving (Eq,Ord,Show,Typeable)
                          
instance Exception LLVMError

llvmErrorFunctionName :: LLVMError -> Name
llvmErrorFunctionName (LLVMError funcName _) = funcName
llvmErrorFunctionName (LLVMInternalError funcName _) = funcName
llvmErrorFunctionName (LLVMDiagnostic funcName _) = funcName

llvmErrorString :: LLVMError -> String
llvmErrorString (LLVMError _ str) = str
llvmErrorString (LLVMInternalError _ str) = str
llvmErrorString (LLVMDiagnostic _ diag) = diagnosticDisplay diag

errorToIO :: Name -> ErrorT String IO a -> IO a
errorToIO funcName m =
  do result <- catchInternal funcName $ runErrorT m
     case result of 
       Left str -> throwIO (LLVMError funcName str)
       Right x -> return x
       
diagToIO :: Name -> ErrorT (Either String Diagnostic) IO a -> IO a
diagToIO funcName m =
  do result <- catchInternal funcName $ runErrorT m
     case result of 
       Left (Left str) -> throwIO (LLVMError funcName str)
       Left (Right diag) -> throwIO (LLVMDiagnostic funcName diag)
       Right x -> return x

catchInternal :: (MonadBaseControl IO m) => Name -> m a -> m a
catchInternal funcName m =
  catch m catcher
  where catcher e = 
          if isUserError e
          then throwIO (LLVMInternalError funcName (ioeGetErrorString e))
          else throwIO e

newtype AntiCatch = AntiCatch IOError
                  deriving (Show,Typeable)

instance Exception AntiCatch

catchCaller :: (MonadBaseControl IO m) => 
               Name
               -> ((a -> m b) -> m b) 
               -> (a -> m b) 
               -> m b

catchCaller funcName f g =
  catches (f g') [Handler catcherA, Handler catcherB]
  where g' x = catch (g x) (throwIO . AntiCatch)
        catcherA (AntiCatch e) = throwIO e
        catcherB e =
          if isUserError e
          then throwIO (LLVMInternalError funcName (ioeGetErrorString e))
          else throwIO e

catchCallerError :: (MonadBaseControl IO m) => 
                    Name
                    -> ((a -> m b) -> ErrorT String m b) 
                    -> (a -> m b) 
                    -> m b

catchCallerError funcName f g =
  catchCaller funcName f' g
  where f' g' = 
          do result <- (runErrorT . f) g'
             case result of
               Left str -> throwIO (LLVMError funcName str)
               Right x -> return x
               
catchCallerDiag :: (MonadBaseControl IO m) => 
                    Name
                    -> ((a -> m b) -> ErrorT (Either String Diagnostic) m b) 
                    -> (a -> m b) 
                    -> m b
catchCallerDiag funcName f g =
  catchCaller funcName f' g
  where f' g' = 
          do result <- (runErrorT . f) g'
             case result of
               Left (Left str) -> throwIO (LLVMError funcName str)
               Left (Right diag) -> throwIO (LLVMDiagnostic funcName diag)
               Right x -> return x
