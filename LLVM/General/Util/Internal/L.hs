{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeFamilies,
    RankNTypes, FlexibleInstances, MultiParamTypeClasses,
    UndecidableInstances, FlexibleContexts, PatternSynonyms,
    NoImplicitPrelude, TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
module LLVM.General.Util.Internal.L where

import ClassyPrelude
import Control.Monad.Base(MonadBase(..), liftBaseDefault)
import Control.Monad.Cont.Class(MonadCont(..))
import Control.Monad.Error.Class(MonadError(..))
import Control.Monad.Morph(
  MonadTrans(..), MFunctor(..), MMonad(..))
import Control.Monad.Reader.Class(MonadReader(..))
import Control.Monad.RWS.Class(MonadRWS)
import Control.Monad.State.Class(MonadState(..))
import Control.Monad.Trans.Control(
  MonadTransControl(..), MonadBaseControl(..),
  liftBaseOp, ComposeSt, defaultRestoreM, defaultLiftBaseWith)
import Control.Monad.Writer.Class(MonadWriter(..))
import Data.Void(Void,absurd)
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.DataLayout as AST
import qualified LLVM.General.Context as LL
import qualified LLVM.General.Target as LL
import LLVM.General.Util.Internal.Diagnostic
import LLVM.General.Util.Platform
import LLVM.General.Util.Settings

type family DiscriminateBool (a :: Bool) (b :: Bool) where
  DiscriminateBool True True = Void
  DiscriminateBool False False = Void
  DiscriminateBool True False = ()
  DiscriminateBool False True = ()

data BackendMaybe (b :: Backend) a where
  BackendJust :: a -> BackendMaybe b a
  BackendNothing :: DiscriminateBool (IsBackendVoid b) False
                    -> BackendMaybe b a

fromBackendJust :: (IsBackendVoid b ~ False) => BackendMaybe b a -> a
fromBackendJust (BackendJust x) = x
fromBackendJust (BackendNothing x) = absurd x

fromBackendMaybe :: BackendMaybe b a -> Maybe a
fromBackendMaybe (BackendJust x) = Just x
fromBackendMaybe (BackendNothing _) = Nothing

backendNothing :: BackendMaybe BackendVoid a
backendNothing = BackendNothing ()

data C b =
  C { lPlatform :: BackendMaybe b (Platform b),
      lSettings :: BackendMaybe b Settings,
      lDataLayout :: BackendMaybe b AST.DataLayout,
      lContext :: LL.Context,
      lTargetMachine :: BackendMaybe b LL.TargetMachine,
      lLibraryInfo :: LL.TargetLibraryInfo,
      lCounter :: MVar Word }

newtype L b m a = L { unL :: (C b -> m a) }

instance (Functor m) => Functor (L b m) where
  f `fmap` (L l) = L (\c -> f `fmap` (l c))

instance (Applicative m) => Applicative (L b m) where
  pure = L . const . pure
  (L l) <*> (L l') = L (\c -> l c <*> l' c)

instance (Monad m) => Monad (L b m) where
  return = L . const . return
  (L l) >>= f = L (\c -> l c >>= flip (unL . f) c)
  fail = L . const . fail

instance MonadTrans (L b) where
  lift = L . const

instance (MonadIO m) => MonadIO (L b m) where
  liftIO = lift . liftIO

instance (MonadBase base m) => MonadBase base (L b m) where
  liftBase = liftBaseDefault

instance (MonadTransControl (L b)) where
  newtype StT (L b) a = StLT { unStLT :: a }
  liftWith f = L $ \c -> f $ \t -> liftM StLT $ unL t c
  restoreT = L . const . liftM unStLT

instance (MonadBaseControl base m) => MonadBaseControl base (L b m) where
  data StM (L b m) a = StLM { unStLM :: ComposeSt (L b) m a }
  restoreM = defaultRestoreM unStLM
  liftBaseWith = defaultLiftBaseWith StLM

instance (MFunctor (L b)) where
  hoist f (L l) = L (f . l)

instance (MMonad (L b)) where
  embed f (L l) = L (\c -> unL (f (l c)) c)

instance (MonadReader r m) => MonadReader r (L b m) where
  ask = lift ask
  local f m = hoist (local f) m

instance (MonadWriter w m) => MonadWriter w (L b m) where
  writer = lift . writer
  tell = lift . tell
  listen (L l) = L (listen . l)
  pass (L l) = L (pass . l)

instance (MonadState s m) => MonadState s (L b m) where
  get = lift get
  put = lift . put

instance (MonadRWS r w s m) => MonadRWS r w s (L b m)

instance (MonadError e m) => MonadError e (L b m) where
  throwError = lift . throwError
  catchError (L l) f =
    L (\c -> catchError (l c) (\e -> unL (f e) c))

-- This is fairly useless since there's no MonadControlBase instance for
-- for ContT.
instance (MonadCont m) => MonadCont (L b m) where
  callCC f = L (\c -> callCC $ (\cont -> unL (f (L . const . cont)) c))

getC :: (Monad m) => (L b m (C b))
getC = L return

getPlatform :: (Monad m, IsBackendVoid b ~ False) => L b m (Platform b)
getPlatform = L (return . fromBackendJust . lPlatform)

getSettings :: (Monad m, IsBackendVoid b ~ False) => L b m Settings
getSettings = L (return . fromBackendJust . lSettings)

getSettingsMaybe :: (Monad m) => L b m (Maybe Settings)
getSettingsMaybe = L (return . fromBackendMaybe . lSettings)

getDataLayout :: (Monad m, IsBackendVoid b ~ False) => L b m AST.DataLayout
getDataLayout = L (return . fromBackendJust . lDataLayout)

getDataLayoutMaybe :: (Monad m) => L b m (Maybe AST.DataLayout)
getDataLayoutMaybe = L (return . fromBackendMaybe . lDataLayout)

getSettingsOrDefault :: (Monad m) => L b m Settings
getSettingsOrDefault =
  do maybeSettings <- getSettingsMaybe
     case maybeSettings of
       Just x -> return x
       Nothing -> return defaultSettings

getContext :: (Monad m) => (L b m LL.Context)
getContext = L (return . lContext)

getTargetMachine :: (Monad m, IsBackendVoid b ~ False)
                    => (L b m LL.TargetMachine)
getTargetMachine = L (return . fromBackendJust . lTargetMachine)

getTargetMachineMaybe :: (Monad m)
                         => (L b m (Maybe LL.TargetMachine))
getTargetMachineMaybe = L (return . fromBackendMaybe . lTargetMachine)

getLibraryInfo :: (Monad m) => (L b m LL.TargetLibraryInfo)
getLibraryInfo = L (return . lLibraryInfo)

newName :: (MonadBaseControl IO m) => L b m AST.Name
newName = L (\c -> liftBase $ modifyMVar (lCounter c) $
                   (\w -> return (w+1, AST.UnName w)))

runL :: (MonadBaseControl IO m)
        => Platform b -> Settings -> L b m a -> m a
runL platform settings (L l) =
  do let Platform subtarget triple = platform
     let Settings relocModel codeModel optLevel options = settings
     counter <- liftBase $ newMVar 0
     (target, tripleString') <-
       liftBase $ errorToIO 'LL.lookupTarget $
       LL.lookupTarget
       (Just $ showTarget . Just $ untagTarget $ subtargetTarget $
        platformSubtarget platform)
       (showTriple triple)
     let platform' = platform { platformTriple = parseTriple tripleString' }
     liftBaseOp (catchCaller 'LL.withTargetOptions LL.withTargetOptions)
       (\targetOptions ->
         do liftBase $ catchInternal 'LL.pokeTargetOptions $
              LL.pokeTargetOptions options targetOptions
            liftBaseOp
              (catchCaller 'LL.withTargetMachine $
               LL.withTargetMachine target tripleString'
               (subtargetCpuString subtarget)
               mempty -- XXX see llvm-general issue #105
               targetOptions relocModel codeModel optLevel)
              (\targetMachine ->
                do dataLayout <-
                     liftBase $
                     catchInternal 'LL.getTargetMachineDataLayout $
                     LL.getTargetMachineDataLayout targetMachine
                   liftBaseOp (catchCaller 'LL.withContext LL.withContext)
                     (\ctx ->
                       liftBaseOp
                       (catchCaller 'LL.withTargetLibraryInfo $
                        LL.withTargetLibraryInfo tripleString')
                       (\libraryInfo ->
                           let c = C (BackendJust platform')
                                   (BackendJust settings)
                                   (BackendJust dataLayout) ctx
                                   (BackendJust targetMachine)
                                   libraryInfo counter in
                           l c))))

runAnyL :: (MonadBaseControl IO m)
           => AnyPlatform -> Settings
           -> (forall b. (IsBackendVoid b ~ False) => L b m a)
           -> m a
runAnyL (AnyPlatform platform) settings m =
  runL platform settings m

runDefaultL :: (MonadBaseControl IO m)
               => (forall b. (IsBackendVoid b ~ False) => L b m a)
               -> m a
runDefaultL m =
  do platform <- liftBase getDefaultPlatform
     runAnyL platform defaultSettings m

runNativeL :: (MonadBaseControl IO m)
              => (forall b. (IsBackendVoid b ~ False) => L b m a)
              -> m a
runNativeL m =
  do platform <- liftBase getNativePlatform
     runAnyL platform defaultSettings m

runVoidL :: (MonadBaseControl IO m) => L BackendVoid m a -> m a
runVoidL (L l) =
  do counter <- liftBase $ newMVar 0
     catchCaller 'LL.withContext (liftBaseOp LL.withContext)
       (\ctx ->
         liftBaseOp
         (catchCaller 'LL.withTargetLibraryInfo $ LL.withTargetLibraryInfo "")
         (\libraryInfo ->
           let c = C backendNothing backendNothing backendNothing ctx
                   backendNothing libraryInfo counter in
           l c))

runInL :: (MonadBaseControl IO m)
          => Platform b -> Settings -> L b m a -> L b' m a
runInL platform settings (L l) =
  L (\c ->
      do let Platform subtarget triple = platform
         let Settings relocModel codeModel optLevel options = settings
         let counter = lCounter c
         let ctx = lContext c
         (target, tripleString') <-
           liftBase $ errorToIO 'LL.lookupTarget $ LL.lookupTarget
           (Just $ showTarget . Just $ untagTarget $ subtargetTarget $
            platformSubtarget platform)
           (showTriple triple)
         let platform' =
               platform { platformTriple = parseTriple tripleString' }
         liftBaseOp (catchCaller 'LL.withTargetOptions LL.withTargetOptions) $
           (\targetOptions ->
             do liftBase $ catchInternal 'LL.pokeTargetOptions $
                  LL.pokeTargetOptions options targetOptions
                liftBaseOp
                  (catchCaller 'LL.withTargetMachine $
                   LL.withTargetMachine target tripleString'
                   (subtargetCpuString subtarget)
                   mempty -- XXX see llvm-general issue #105
                   targetOptions relocModel codeModel optLevel)
                  (\targetMachine ->
                    do dataLayout <-
                         liftBase $
                         catchInternal 'LL.getTargetMachineDataLayout $
                         LL.getTargetMachineDataLayout targetMachine
                       liftBaseOp
                         (catchCaller 'LL.withTargetLibraryInfo $
                          LL.withTargetLibraryInfo tripleString')
                         (\libraryInfo ->
                           let c' = C (BackendJust platform')
                                    (BackendJust settings)
                                    (BackendJust dataLayout) ctx
                                    (BackendJust targetMachine)
                                    libraryInfo counter in
                           l c'))))

runAnyInL :: (MonadBaseControl IO m)
             => AnyPlatform -> Settings
             -> (forall b. (IsBackendVoid b ~ False) => L b m a)
             -> L b' m a
runAnyInL (AnyPlatform platform) settings m =
  runInL platform settings m

runVoidInL :: (MonadBaseControl IO m)
              => L BackendVoid m a -> L b m a
runVoidInL (L l) =
  liftBaseOp
  (catchCaller 'LL.withTargetLibraryInfo $ LL.withTargetLibraryInfo "")
  (\libraryInfo ->
    L (\c -> l $ c { lPlatform = backendNothing,
                     lSettings = backendNothing,
                     lDataLayout = backendNothing,
                     lTargetMachine = backendNothing,
                     lLibraryInfo = libraryInfo }))

