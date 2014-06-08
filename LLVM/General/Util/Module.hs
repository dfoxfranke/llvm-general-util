{-# LANGUAGE NoImplicitPrelude, DataKinds, FlexibleContexts,
    TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module LLVM.General.Util.Module (
  ModuleBuilder(),
  
  ModuleAST,
  moduleFromAST, moduleFromBitcode, moduleFromLLVMAssembly,
  moduleFromLLVMAssemblyFile, moduleFromBitcodeFile, linkModules,
  transformModule,
  
  studyModule,
  
  moduleAST, moduleBitcode, moduleTargetAssembly, moduleObject,
  writeLLVMAssemblyToFile, writeBitcodeToFile, writeTargetAssemblyToFile,
  writeObjectToFile,
  
  ExecutableModule(),
  jitCompile, jitGetFunction
  ) where

import ClassyPrelude

import Data.Data(Data)
import Foreign.Ptr(FunPtr)
import qualified LLVM.General.Analysis as LL
import qualified LLVM.General.AST as AST
import qualified LLVM.General.ExecutionEngine as LL
import qualified LLVM.General.Module as LL
import qualified LLVM.General.PassManager as LL
import LLVM.General.Util.Internal.Diagnostic
import LLVM.General.Util.Internal.L
import LLVM.General.Util.PassSetSpec
import LLVM.General.Util.Platform
import LLVM.General.Util.Settings
import qualified LLVM.General.CodeGenOpt as CG
import qualified System.IO as IO
import Control.Monad.Base(MonadBase(..))
import Control.Monad.Trans.Control(MonadBaseControl(..), liftBaseOp)

type ModuleIO = LL.Module
type ModuleAST = AST.Module

data ModuleBuilder = MBAST ModuleAST
                   | MBBitcode String ByteString
                   | MBVerifiedBitcode ByteString
                   | MBAssembly String
                   | MBAssemblyFile IO.FilePath
                   | MBBitcodeFile IO.FilePath
                   | MBLink ModuleBuilder ModuleBuilder
                   | MBTransform PassSetSpec ModuleBuilder
                   deriving (Eq,Show,Typeable,Data)
                     
moduleFromAST :: ModuleAST -> ModuleBuilder
moduleFromAST = MBAST

moduleFromBitcode :: String -> ByteString -> ModuleBuilder
moduleFromBitcode = MBBitcode

moduleFromLLVMAssembly :: String -> ModuleBuilder
moduleFromLLVMAssembly = MBAssembly

moduleFromLLVMAssemblyFile :: IO.FilePath -> ModuleBuilder
moduleFromLLVMAssemblyFile = MBAssemblyFile

moduleFromBitcodeFile ::  IO.FilePath -> ModuleBuilder
moduleFromBitcodeFile = MBBitcodeFile

linkModules :: ModuleBuilder -> ModuleBuilder -> ModuleBuilder
linkModules = MBLink

transformModule :: PassSetSpec -> ModuleBuilder -> ModuleBuilder
transformModule = MBTransform

pss :: (MonadBaseControl IO m) => PassSetSpec -> L b m LL.PassSetSpec
pss spec =
  do c <- getC 
     let dl = fromBackendMaybe (lDataLayout c)
     let tli = Just $ lLibraryInfo c
     let tm = fromBackendMaybe (lTargetMachine c)
     case spec of
       (PassSetSpec t) ->
         return $ LL.PassSetSpec t dl tli tm
       (CuratedPassSetSpec ol sl uaat slc uiwt) ->
         return $ LL.CuratedPassSetSpec ol sl uaat slc uiwt tli

withModule :: (MonadBaseControl IO m)
              => ModuleBuilder -> (ModuleIO -> L b m a) -> L b m a
withModule builder f =
  do ctx <- getContext
     case builder of
       MBAST ast ->
         liftBaseOp
         (catchCallerError 'LL.withModuleFromAST $ 
          LL.withModuleFromAST ctx ast)
         (\moduleIO -> do liftBase $ errorToIO 'LL.verify $ LL.verify moduleIO
                          f moduleIO)
       MBBitcode s bs ->
         liftBaseOp
         (catchCallerError 'LL.withModuleFromBitcode $ 
          LL.withModuleFromBitcode ctx (s,bs))
         (\moduleIO -> do liftBase $ errorToIO 'LL.verify $ LL.verify moduleIO
                          f moduleIO)
       MBVerifiedBitcode bs ->
         liftBaseOp
         (catchCallerError 'LL.withModuleFromBitcode $ 
          LL.withModuleFromBitcode ctx ("",bs)) f
       MBAssembly asm ->
         liftBaseOp
         (catchCallerDiag 'LL.withModuleFromLLVMAssembly $ 
          LL.withModuleFromLLVMAssembly ctx asm)
         (\moduleIO -> do liftBase $ errorToIO 'LL.verify $ LL.verify moduleIO
                          f moduleIO)
       MBAssemblyFile file ->
         liftBaseOp
         (catchCallerDiag 'LL.withModuleFromLLVMAssembly $
          LL.withModuleFromLLVMAssembly ctx (LL.File file))
         (\moduleIO -> do liftBase $ errorToIO 'LL.verify $ LL.verify moduleIO
                          f moduleIO)
       MBBitcodeFile file ->
         liftBaseOp
         (catchCallerError 'LL.withModuleFromBitcode $
          LL.withModuleFromBitcode ctx (LL.File file))
         (\moduleIO -> do liftBase $ errorToIO 'LL.verify $ LL.verify moduleIO
                          f moduleIO)
       MBLink mb1 mb2 ->
         withModule mb1 
         (\mod1 ->
           withModule mb2
           (\mod2 ->
             do liftBase $ errorToIO 'LL.linkModules $ 
                  LL.linkModules False mod1 mod2
                f mod1))
       MBTransform spec mb ->
         withModule mb
         (\mod1 -> 
           do spec' <- pss spec
              liftBaseOp
                (catchCaller 'LL.withPassManager $ LL.withPassManager spec') 
                (\pm ->
                  do _ <- catchInternal 'LL.runPassManager $
                          liftBase $ LL.runPassManager pm mod1
                     f mod1))

studyModule :: (MonadBaseControl IO m)
               => ModuleBuilder -> L b m ModuleBuilder
studyModule builder =
  withModule builder (liftBase . liftM MBVerifiedBitcode . LL.moduleBitcode)
  
moduleAST :: (MonadBaseControl IO m) => ModuleBuilder -> L b m ModuleAST
moduleAST builder = withModule builder (liftBase . LL.moduleAST)

moduleBitcode :: (MonadBaseControl IO m) => ModuleBuilder -> L b m ByteString
moduleBitcode builder = withModule builder (liftBase . LL.moduleBitcode)

moduleTargetAssembly :: (MonadBaseControl IO m, IsBackendVoid b ~ False)
                        => ModuleBuilder -> L b m String
moduleTargetAssembly builder =
  do tm <- getTargetMachine
     withModule builder $
       liftBase . errorToIO 'LL.moduleTargetAssembly . 
       LL.moduleTargetAssembly tm

moduleObject :: (MonadBaseControl IO m, IsBackendVoid b ~ False)
                => ModuleBuilder -> L b m ByteString
moduleObject builder =
  do tm <- getTargetMachine
     withModule builder $
       liftBase . errorToIO 'LL.moduleObject . LL.moduleObject tm

writeLLVMAssemblyToFile :: (MonadBaseControl IO m)
                           => IO.FilePath -> ModuleBuilder -> L b m ()
writeLLVMAssemblyToFile path builder =
  withModule builder $
  liftBase . errorToIO 'LL.writeLLVMAssemblyToFile . 
  LL.writeLLVMAssemblyToFile (LL.File path)
  
writeBitcodeToFile :: (MonadBaseControl IO m)
                      => IO.FilePath -> ModuleBuilder -> L b m ()
writeBitcodeToFile path builder = 
  withModule builder $
  liftBase . errorToIO 'LL.writeBitcodeToFile . 
  LL.writeBitcodeToFile (LL.File path)

writeTargetAssemblyToFile :: (MonadBaseControl IO m, IsBackendVoid b ~ False)
                             => IO.FilePath -> ModuleBuilder -> L b m ()
writeTargetAssemblyToFile path builder =
  do tm <- getTargetMachine
     withModule builder $
       liftBase . errorToIO 'LL.writeTargetAssemblyToFile . 
       LL.writeTargetAssemblyToFile tm (LL.File path)
  
writeObjectToFile :: (MonadBaseControl IO m, IsBackendVoid b ~ False)
                     => IO.FilePath -> ModuleBuilder -> L b m ()
writeObjectToFile path builder =
  do tm <- getTargetMachine
     withModule builder $
       liftBase . errorToIO 'LL.writeObjectToFile . 
       LL.writeObjectToFile tm (LL.File path)
  
newtype ExecutableModule = ExecutableModule (LL.ExecutableModule LL.MCJIT)

optNum :: OptimizationLevel -> Word
-- Using llvm-general original constructors rather than
-- llvm-general-util's preferred synonyms in order to prevent a spurious
-- warning about inexhaustive patterns. See GHC ticket #8779.
optNum CG.None = 0
optNum CG.Less = 1
optNum CG.Default = 2
optNum CG.Aggressive = 3

jitCompile :: (MonadBaseControl IO m)
              => ModuleBuilder -> (ExecutableModule -> L b m a) -> L b m a
jitCompile mb f =
  do maybeSettings <- getSettingsMaybe
     context <- getContext
     let (optLevel, model, noFpe, enableFis) =
           case maybeSettings of
             Just settings ->
               (Just $ optNum $ settingsOptimizationLevel settings,
                Just $ settingsCodeModel settings,
                Just $ noFramePointerElimination $ settingsOptions settings,
                Just $ enableFastInstructionSelection $ 
                settingsOptions settings)
             Nothing -> (Nothing, Nothing, Nothing, Nothing)
     withModule mb
       (\mod1 ->
         liftBaseOp
         (catchCaller 'LL.withMCJIT $ 
          LL.withMCJIT context optLevel model noFpe enableFis)
         (\mcjit ->
           liftBaseOp
           (catchCaller 'LL.withModuleInEngine $ 
            LL.withModuleInEngine mcjit mod1)
           (f . ExecutableModule)))
         
jitGetFunction :: (MonadBaseControl IO m) 
                  => ExecutableModule -> AST.Name -> L b m (Maybe (FunPtr ()))
jitGetFunction (ExecutableModule e) name = 
  liftBase $ catchInternal 'LL.getFunction $ LL.getFunction e name
