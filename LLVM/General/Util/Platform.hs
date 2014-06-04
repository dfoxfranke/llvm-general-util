{-# LANGUAGE DeriveDataTypeable, DataKinds, KindSignatures, GADTs, RankNTypes, TypeFamilies, TemplateHaskell, FlexibleContexts, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall #-}
module LLVM.General.Util.Platform (
  -- * Backends, targets and subtargets
  Backend(..), 
  
  -- ** Targets
  Target(..),
  parseTarget, showTarget,
  
  TaggedTarget(..), untagTarget, targetToBackend,
  
  -- ** CPUs
  BackendCpu, 
  
  -- ** Features
  BackendFeature, parseFeatures, showFeatures,
  
  -- ** Subtargets
  Subtarget(..),
  subtargetTarget, subtargetCpu, subtargetFeatures,
  subtargetCpuString, subtargetFeatureStrings,
  
  AnySubtarget(..),
  parseAnySubtarget, anySubtargetTarget, anySubtargetCpu, anySubtargetFeatures,
  
  -- ** Specific backends
  -- *** AArch64 backend
  Aarch64Cpu(..), parseAarch64Cpu, showAarch64Cpu,
  Aarch64Feature(..), parseAarch64Feature, showAarch64Feature,
  
  -- *** ARM backend
  ArmCpu(..), parseArmCpu, showArmCpu,
  ArmFeature(..), parseArmFeature, showArmFeature,
  
  -- *** CPP backend
  -- | The CPP backend ignores @-mcpu=@ and @-mattr@, so accordingly
  -- there is no @CppCpu@ or @CppFeature@ type.
  
  -- *** Hexagon backend
  HexagonCpu(..), parseHexagonCpu, showHexagonCpu,
  HexagonFeature(..), parseHexagonFeature, showHexagonFeature,
                                           
  -- *** MIPS backend
  MipsCpu(..), parseMipsCpu, showMipsCpu,
  MipsFeature(..), parseMipsFeature, showMipsFeature,
  
  -- *** MSP430 backend
  Msp430Cpu(..), parseMsp430Cpu, showMsp430Cpu,
  Msp430Feature(..), parseMsp430Feature, showMsp430Feature,
  
  -- *** NVptx backend
  NvptxCpu(..), parseNvptxCpu, showNvptxCpu,
  NvptxFeature(..), parseNvptxFeature, showNvptxFeature,
  
  -- *** PPC backend
  PpcCpu(..), parsePpcCpu, showPpcCpu,
  PpcFeature(..), parsePpcFeature, showPpcFeature,
  
  -- *** R600 backend
  R600Cpu(..), parseR600Cpu, showR600Cpu,
  R600Feature(..), parseR600Feature, showR600Feature,
  
  -- *** SPARC backend
  SparcCpu(..), parseSparcCpu, showSparcCpu,
  SparcFeature(..), parseSparcFeature, showSparcFeature,
  
  -- *** SystemZ backend
  SystemzCpu(..), parseSystemzCpu, showSystemzCpu,
  SystemzFeature(..), parseSystemzFeature, showSystemzFeature,
  
  -- *** x86 backend
  X86Cpu(..), parseX86Cpu, showX86Cpu,
  X86Feature(..), parseX86Feature, showX86Feature,
  
  -- *** XCore backend
  XcoreCpu(..), parseXcoreCpu, showXcoreCpu,
  
  -- * Triples  
  Triple(..), parseTriple, showTriple,
                           
  -- ** Architectures
  Architecture(..), parseArchitecture, showArchitecture,
  architectureToTarget,
  
  -- ** Vendors
  Vendor(..), parseVendor, showVendor,
                                       
  -- ** OSs
  OS(..), parseOS, showOS,
  
  -- ** Environments
  Environment(..), parseEnvironment, showEnvironment,
  
  -- ** Object formats
  Format(..), parseFormat, showFormat,
                           
  -- * Platforms
  Platform(..), AnyPlatform(..),
  anyPlatformSubtarget, anyPlatformTriple, mkAnyPlatform,
  mkAnyPlatformFromTriple
  
  ) where

import LLVM.General.Util.TH(genStrEnum)

import Data.Functor((<$>))
import Data.List(isPrefixOf, isSuffixOf,intercalate)
import Data.List.Split(splitOn)
import Data.Typeable(Typeable())
import Data.Char(toLower)
import Data.Data(Data())
import Data.Map(Map)
import qualified Data.Map as M
import Data.Maybe(isJust,fromJust)
import Data.Void(Void)

data Backend = BackendAarch64
             | BackendArm
             | BackendCpp
             | BackendHexagon
             | BackendMips
             | BackendMsp430
             | BackendNvptx
             | BackendPpc
             | BackendR600
             | BackendSparc
             | BackendSystemz
             | BackendX86
             | BackendXcore
               
$(genStrEnum "Target" "Target"
  ["aarch64", "arm", "cpp", "hexagon", "mips", "mips64", "mips64el",
   "mipsel", "msp430", "nvptx", "nvptx64", "ppc32", "ppc64", "ppc64le",
   "r600", "sparc", "sparcv9", "systemz", "thumb", "x86", "x86-64",
   "xcore"] "" True)

$(genStrEnum "Aarch64Cpu" "Aarch64Cpu" ["generic"] "" True)
$(genStrEnum "Aarch64Feature" "Aarch64Feature" 
  ["crypto", "fp-armv8", "neon"] "" True)
  
$(genStrEnum "ArmCpu" "ArmCpu"
  ["arm1020t", "arm1022e", "arm10e", "arm10tdmi", "arm1136j-s", "arm1136jf-s",
   "arm1156t2-s", "arm1156t2f-s", "arm1176jz-s", "arm1176jzf-s", "arm710t",
   "arm720t", "arm7tdmi", "arm7tdmi-s", "arm8", "arm810", "arm9", "arm920",
   "arm920t", "arm922t", "arm926ej-s", "arm940t", "arm946e-s", "arm966e-s",
   "arm968e-s", "arm9e", "arm9tdmi", "cortex-a15", "cortex-a5", "cortex-a53",
   "cortex-a57", "cortex-a8", "cortex-a9", "cortex-a9-mp", "cortex-m0", 
   "cortex-m3", "cortex-m4", "cortex-r5", "ep9312", "generic", "iwmmxt", 
   "mpcore", "mpcorenovfp", "strongarm", "strongarm110", "strongarm1100",
   "strongarm1110", "swift", "xscale"] "" True)
$(genStrEnum "ArmFeature" "ArmFeature"
  ["32bit", "a15", "a5", "a53", "a57", "a8", "a9", "aclass", "avoid-movs-shop", 
   "avoid-partial-cpsr", "crc", "crypto", "d16", "db", "fp-armv8", 
   "fp-only-sp", "fp16", "hwdiv", "hwdiv-arm", "mclass", "mp", "nacl-trap",
   "neon", "neonfp", "noarm", "perfmon", "r5", "ras", "rclass", "slow-fp-brcc", 
   "slowfpvmlx", "swift", "t2dsp", "t2xtpk", "thumb-mode", "thumb2",
   "trustzone", "v4t", "v5t", "v5te", "v6", "v6m", "v6t2", "v7", "v8", "vfp2",
   "vfp3", "vfp4", "virtualization", "vmlx-forwarding"] "" True)

$(genStrEnum "HexagonCpu" "HexagonCpu"
  ["hexagonv2", "hexagonv3", "hexagonv4", "hexagonv5"] "" True)
$(genStrEnum "HexagonFeature" "HexagonFeature"
  ["v2", "v3", "v4", "v5"] "" True)

$(genStrEnum "MipsCpu" "MipsCpu"
  ["mips16", "mips32", "mips32r2", "mips64", "mips64r2"] "" True)
$(genStrEnum "MipsFeature" "MipsFeature"
  ["FPIdx", "bitcount", "condmov", "dsp", "dspr2", "eabi", "fp64", 
   "gp64", "micromips", "mips16", "mips32", "mips32r2", "mips64", "mips64r2",
   "msa", "n32", "n64", "o32", "seinreg", "single-float", "swap", "vfpu"]
   "" True)

$(genStrEnum "Msp430Cpu" "Msp430Cpu" ["generic"] "" True)
$(genStrEnum "Msp430Feature" "Msp430Feature" ["ext"] "" True)

$(genStrEnum "NvptxCpu" "NvptxCpu"
  ["sm_20", "sm_21", "sm_30", "sm_35"] "" True)
$(genStrEnum "NvptxFeature" "NvptxFeature"
  ["ptx30", "ptx31", "sm_20", "sm_21", "sm_30", "sm_35"] "" True)

$(genStrEnum "PpcCpu" "PpcCpu"
  ["440", "450", "601", "602", "603", "603e", "603ev", "604", "604e", "620", 
   "7400", "7450", "750", "970", "a2", "a2q", "e500mc", "e5500", "g3", "g4",
   "g4+", "g5", "generic", "ppc", "ppc64", "ppc64le", "pwr3", "pwr4", "pwr5", 
   "pwr5x", "pwr6", "pwr6x", "pwr7"] "" True)
$(genStrEnum "PpcFeature" "PpcFeature"
  ["64bit", "64bitregs", "altivec", "booke", "fcpsgn", "fpcvt", "fprnd", "fre",
   "fres", "frsqrte", "frsqrtes", "fsqrt", "isel", "ldbrx", "lfiwax", "mfocrf",
   "popcntd", "qpx", "recipprec", "stfiwx", "vsx"] "" True)

$(genStrEnum "R600Cpu" "R600Cpu"
  ["SI", "barts", "bonaire", "caicos", "cayman", "cedar", "cypress",
   "hainan", "hawaii", "juniper", "kabini", "kaveri", "oland", "pitcairn",
   "r600", "redwood", "rs880", "rv670", "rv710", "rv730", "rv770", "sumo",
   "tahiti", "turks", "verde"] "" True)
$(genStrEnum "R600Feature" "R600Feature"
  ["64BitPtr", "64on32BitPtr", "DumpCode", "EVERGREEN", "HasVertexCache",
   "NORTHERN_ISLANDS", "R600", "R600ALUInst", "R700", "SEA_ISLANDS",
   "SOUTHERN_ISLANDS", "caymanISA", "disable-ifcvt",
   "disable-irstructurizer", "fetch16", "fetch8", "fp64"] "" True)
  
$(genStrEnum "SparcCpu" "SparcCpu"
  ["f934", "generic", "hypersparc", "sparclet", "sparclite", "sparclite86x",
   "supersparc", "tsc701", "ultrasparc", "ultrasparc3", "ultrasparc3-vis",
   "v8", "v9"] "" True)
$(genStrEnum "SparcFeature" "SparcFeature"
  ["deprecated-v8", "hard-quad-float", "v9", "vis"] "" True)

$(genStrEnum "SystemzCpu" "SystemzCpu"
  ["generic", "z10", "z196", "zEC12"] "" True)
$(genStrEnum "SystemzFeature" "SystemzFeature"
  ["distinct-ops", "fp-extension", "high-word", "load-store-on-cond"]
  "" True)

$(genStrEnum "X86Cpu" "X86Cpu"
  ["amdfam10", "athlon", "athlon-4", "athlon-fx", "athlon-mp", "athlon-tbird",
   "athlon-xp", "athlon64", "athlon64-sse3", "atom", "bdver1", "bdver2",
   "bdver3", "btver1", "btver2", "c3", "c3-2", "core-avx-i", "core-avx2",
   "core2", "corei7", "corei7-avx", "generic", "geode", "i386", "i486", "i586",
   "i686", "k6", "k6-2", "k6-3", "k8", "k8-sse3", "knl", "nehalem", "nocona",
   "opteron", "opteron-sse3", "penryn", "pentium", "pentium-m", "pentium-mmx",
   "pentium2", "pentium3", "pentium3m", "pentium4", "pentium4m", "pentiumpro",
   "prescott", "slm", "westmere", "winchip-c6", "winchip2", "x86-64", "yonah"]
  "" True)
$(genStrEnum "X86Feature" "X86Feature"
  ["3dnow", "3dnowa", "64bit", "64bit-mode", "adx", "aes", "atom", "avx",
   "avx2", "avx512cd", "avx512er", "avx512f", "avx512pf", "bmi", "bmi2",
   "call-reg-indirect", "cmov", "cx16", "f16c", "fast-unaligned-mem", "fma",
   "fma4", "fsgsbase", "hle", "idiv-to-divb", "lea-sp", "lea-uses-ag",
   "lzcnt", "mmx", "movbe", "pad-short-functions", "pclmul", "popcnt",
   "prfchw", "rdrnd", "rdseed", "rtm", "sha", "slm", "slow-bt-mem", "sse",
   "sse2", "sse3", "sse4.1", "sse4.2", "sse4a", "ssse3", "tbm",
   "vector-unaligned-mem", "xop"] "" True)

$(genStrEnum "XcoreCpu" "XcoreCpu" ["generic"] "" True)
                       
data TaggedTarget :: Backend -> * where
  TTargetAarch64 :: TaggedTarget BackendAarch64
  TTargetArm :: TaggedTarget BackendArm
  TTargetCpp :: TaggedTarget BackendCpp
  TTargetHexagon :: TaggedTarget BackendHexagon
  TTargetMips :: TaggedTarget BackendMips
  TTargetMips64 :: TaggedTarget BackendMips
  TTargetMips64el :: TaggedTarget BackendMips
  TTargetMipsel :: TaggedTarget BackendMips
  TTargetMsp430 :: TaggedTarget BackendMsp430
  TTargetNvptx :: TaggedTarget BackendNvptx
  TTargetNvptx64 :: TaggedTarget BackendNvptx
  TTargetPpc32 :: TaggedTarget BackendPpc
  TTargetPpc64 :: TaggedTarget BackendPpc
  TTargetPpc64le :: TaggedTarget BackendPpc
  TTargetR600 :: TaggedTarget BackendR600
  TTargetSparc :: TaggedTarget BackendSparc
  TTargetSparcv9 :: TaggedTarget BackendSparc
  TTargetSystemz :: TaggedTarget BackendSystemz
  TTargetThumb :: TaggedTarget BackendArm
  TTargetX86 :: TaggedTarget BackendX86
  TTargetX86_64 :: TaggedTarget BackendX86
  TTargetXcore :: TaggedTarget BackendXcore
  deriving (Typeable)

instance Eq (TaggedTarget a) where
  x == y = untagTarget x == untagTarget y

instance Ord (TaggedTarget a) where
  compare x y = compare (untagTarget x) (untagTarget y)
  
instance Show (TaggedTarget a) where
  show x = "T" ++ show (untagTarget x)

untagTarget :: TaggedTarget a -> Target
untagTarget TTargetAarch64 = TargetAarch64
untagTarget TTargetArm = TargetArm
untagTarget TTargetCpp = TargetCpp
untagTarget TTargetHexagon = TargetHexagon
untagTarget TTargetMips = TargetMips
untagTarget TTargetMips64 = TargetMips64
untagTarget TTargetMips64el = TargetMips64el
untagTarget TTargetMipsel = TargetMipsel
untagTarget TTargetMsp430 = TargetMsp430
untagTarget TTargetNvptx = TargetNvptx
untagTarget TTargetNvptx64 = TargetNvptx64
untagTarget TTargetPpc32 = TargetPpc32
untagTarget TTargetPpc64 = TargetPpc64
untagTarget TTargetPpc64le = TargetPpc64le
untagTarget TTargetR600 = TargetR600
untagTarget TTargetSparc = TargetSparc
untagTarget TTargetSparcv9 = TargetSparcv9
untagTarget TTargetSystemz = TargetSystemz
untagTarget TTargetThumb = TargetThumb
untagTarget TTargetX86 = TargetX86
untagTarget TTargetX86_64 = TargetX86_64
untagTarget TTargetXcore = TargetXcore

targetToBackend :: Target -> Backend
targetToBackend TargetAarch64 = BackendAarch64
targetToBackend TargetArm = BackendArm
targetToBackend TargetCpp = BackendCpp
targetToBackend TargetHexagon = BackendHexagon
targetToBackend TargetMips = BackendMips
targetToBackend TargetMips64 = BackendMips
targetToBackend TargetMips64el = BackendMips
targetToBackend TargetMipsel = BackendMips
targetToBackend TargetMsp430 = BackendMsp430
targetToBackend TargetNvptx = BackendNvptx
targetToBackend TargetNvptx64 = BackendNvptx
targetToBackend TargetPpc32 = BackendPpc
targetToBackend TargetPpc64 = BackendPpc
targetToBackend TargetPpc64le = BackendPpc
targetToBackend TargetR600 = BackendR600
targetToBackend TargetSparc = BackendSparc
targetToBackend TargetSparcv9 = BackendSparc
targetToBackend TargetSystemz = BackendSystemz
targetToBackend TargetThumb = BackendArm
targetToBackend TargetX86 = BackendX86
targetToBackend TargetX86_64 = BackendX86
targetToBackend TargetXcore = BackendXcore

type family BackendCpu (a :: Backend) where
  BackendCpu BackendAarch64 = Aarch64Cpu
  BackendCpu BackendArm = ArmCpu
  BackendCpu BackendCpp = Void
  BackendCpu BackendHexagon = HexagonCpu
  BackendCpu BackendMips = MipsCpu
  BackendCpu BackendMsp430 = Msp430Cpu
  BackendCpu BackendNvptx = NvptxCpu
  BackendCpu BackendPpc = PpcCpu
  BackendCpu BackendR600 = R600Cpu
  BackendCpu BackendSparc = SparcCpu
  BackendCpu BackendSystemz = SystemzCpu
  BackendCpu BackendX86 = X86Cpu
  BackendCpu BackendXcore = XcoreCpu
  
type family BackendFeature (a :: Backend) where
  BackendFeature BackendAarch64 = Aarch64Feature
  BackendFeature BackendArm = ArmFeature
  BackendFeature BackendCpp = Void
  BackendFeature BackendHexagon = HexagonFeature
  BackendFeature BackendMips = MipsFeature
  BackendFeature BackendMsp430 = Msp430Feature
  BackendFeature BackendNvptx = NvptxFeature
  BackendFeature BackendPpc = PpcFeature
  BackendFeature BackendR600 = R600Feature
  BackendFeature BackendSparc = SparcFeature
  BackendFeature BackendSystemz = SystemzFeature
  BackendFeature BackendX86 = X86Feature
  BackendFeature BackendXcore = Void
  
parseFeatures :: String -> Map String Bool
parseFeatures features = 
  M.fromList $ classify <$>
  splitOn "," features
  where
    classify ('+':s) = (s,True)
    classify ('-':s) = (s,False)
    classify s = (s,True)
    
showFeatures :: Map String Bool -> String
showFeatures features =
  intercalate "," $ showFeature <$> M.toList features
  where showFeature (feature, True) = '+' : feature
        showFeature (feature, False) = '-' : feature

data Subtarget :: Backend -> * where
  SubtargetAarch64 :: TaggedTarget BackendAarch64
                      -> Maybe Aarch64Cpu 
                      -> Map Aarch64Feature Bool
                      -> Subtarget BackendAarch64
  SubtargetArm :: TaggedTarget BackendArm
                  -> Maybe ArmCpu 
                  -> Map ArmFeature Bool
                  -> Subtarget BackendArm
  SubtargetCpp :: TaggedTarget BackendCpp
                  -> Subtarget BackendCpp
  SubtargetHexagon :: TaggedTarget BackendHexagon
                      -> Maybe HexagonCpu
                      -> Map HexagonFeature Bool
                      -> Subtarget BackendHexagon
  SubtargetMips :: TaggedTarget BackendMips
                   -> Maybe MipsCpu
                   -> Map MipsFeature Bool
                   -> Subtarget BackendMips
  SubtargetMsp430 :: TaggedTarget BackendMsp430
                     -> Maybe Msp430Cpu
                     -> Map Msp430Feature Bool
                     -> Subtarget BackendMsp430
  SubtargetNvptx :: TaggedTarget BackendNvptx
                    -> Maybe NvptxCpu
                    -> Map NvptxFeature Bool
                    -> Subtarget BackendNvptx
  SubtargetR600 :: TaggedTarget BackendR600
                   -> Maybe R600Cpu
                   -> Map R600Feature Bool
                   -> Subtarget BackendR600  
  SubtargetPpc :: TaggedTarget BackendPpc
                  -> Maybe PpcCpu
                  -> Map PpcFeature Bool
                  -> Subtarget BackendPpc
  SubtargetSparc :: TaggedTarget BackendSparc
                    -> Maybe SparcCpu
                    -> Map SparcFeature Bool
                    -> Subtarget BackendSparc
  SubtargetSystemz :: TaggedTarget BackendSystemz
                      -> Maybe SystemzCpu
                      -> Map SystemzFeature Bool
                      -> Subtarget BackendSystemz
  SubtargetX86 :: TaggedTarget BackendX86
                  -> Maybe X86Cpu
                  -> Map X86Feature Bool
                  -> Subtarget BackendX86
  SubtargetXcore :: TaggedTarget BackendXcore
                    -> Maybe XcoreCpu
                    -> Subtarget BackendXcore
                  
deriving instance (Eq (Subtarget a))
deriving instance (Show (Subtarget a))
deriving instance (Typeable Subtarget)

subtargetTarget :: Subtarget a -> TaggedTarget a
subtargetTarget (SubtargetAarch64 t _ _) = t
subtargetTarget (SubtargetArm t _ _) = t
subtargetTarget (SubtargetCpp t) = t
subtargetTarget (SubtargetHexagon t _ _) = t
subtargetTarget (SubtargetMips t _ _) = t
subtargetTarget (SubtargetMsp430 t _ _) = t
subtargetTarget (SubtargetNvptx t _ _) = t
subtargetTarget (SubtargetR600 t _ _) = t
subtargetTarget (SubtargetPpc t _ _) = t
subtargetTarget (SubtargetSparc t _ _) = t
subtargetTarget (SubtargetSystemz t _ _) = t
subtargetTarget (SubtargetX86 t _ _) = t
subtargetTarget (SubtargetXcore t _) = t

subtargetCpu :: Subtarget a -> Maybe (BackendCpu a)
subtargetCpu (SubtargetAarch64 _ c _) = c
subtargetCpu (SubtargetArm _ c _) = c
subtargetCpu (SubtargetCpp _) = Nothing
subtargetCpu (SubtargetHexagon _ c _) = c
subtargetCpu (SubtargetMips _ c _) = c
subtargetCpu (SubtargetMsp430 _ c _) = c
subtargetCpu (SubtargetNvptx _ c _) = c
subtargetCpu (SubtargetR600 _ c _) = c
subtargetCpu (SubtargetPpc _ c _) = c
subtargetCpu (SubtargetSparc _ c _) = c
subtargetCpu (SubtargetSystemz _ c _) = c
subtargetCpu (SubtargetX86 _ c _) = c
subtargetCpu (SubtargetXcore _ c) = c

subtargetCpuString :: Subtarget a -> String
subtargetCpuString (SubtargetAarch64 _ c _) = showAarch64Cpu c
subtargetCpuString (SubtargetArm _ c _) = showArmCpu c
subtargetCpuString (SubtargetCpp _) = ""
subtargetCpuString (SubtargetHexagon _ c _) = showHexagonCpu c
subtargetCpuString (SubtargetMips _ c _) = showMipsCpu c
subtargetCpuString (SubtargetMsp430 _ c _) = showMsp430Cpu c
subtargetCpuString (SubtargetNvptx _ c _) = showNvptxCpu c
subtargetCpuString (SubtargetR600 _ c _) = showR600Cpu c
subtargetCpuString (SubtargetPpc _ c _) = showPpcCpu c
subtargetCpuString (SubtargetSparc _ c _) = showSparcCpu c
subtargetCpuString (SubtargetSystemz _ c _) = showSystemzCpu c
subtargetCpuString (SubtargetX86 _ c _) = showX86Cpu c
subtargetCpuString (SubtargetXcore _ c) = showXcoreCpu c

subtargetFeatures :: Subtarget a -> Map (BackendFeature a) Bool
subtargetFeatures (SubtargetAarch64 _ _ f) = f
subtargetFeatures (SubtargetArm _ _ f) = f
subtargetFeatures (SubtargetCpp _) = M.empty
subtargetFeatures (SubtargetHexagon _ _ f) = f
subtargetFeatures (SubtargetMips _ _ f) = f
subtargetFeatures (SubtargetMsp430 _ _ f) = f
subtargetFeatures (SubtargetNvptx _ _ f) = f
subtargetFeatures (SubtargetR600 _ _ f) = f
subtargetFeatures (SubtargetPpc _ _ f) = f
subtargetFeatures (SubtargetSparc _ _ f) = f
subtargetFeatures (SubtargetSystemz _ _ f) = f
subtargetFeatures (SubtargetX86 _ _ f) = f
subtargetFeatures (SubtargetXcore _ _) = M.empty

subtargetFeatureStrings :: Subtarget a -> Map String Bool
subtargetFeatureStrings (SubtargetAarch64 _ _ f) = 
  M.mapKeys (showAarch64Feature . Just) f
subtargetFeatureStrings (SubtargetArm _ _ f) = 
  M.mapKeys (showArmFeature . Just) f
subtargetFeatureStrings (SubtargetCpp _ ) = 
  M.empty
subtargetFeatureStrings (SubtargetHexagon _ _ f) = 
  M.mapKeys (showHexagonFeature . Just) f
subtargetFeatureStrings (SubtargetMips _ _ f) = 
  M.mapKeys (showMipsFeature . Just) f
subtargetFeatureStrings (SubtargetMsp430 _ _ f) = 
  M.mapKeys (showMsp430Feature . Just) f
subtargetFeatureStrings (SubtargetNvptx _ _ f) = 
  M.mapKeys (showNvptxFeature . Just) f
subtargetFeatureStrings (SubtargetR600 _ _ f) = 
  M.mapKeys (showR600Feature . Just) f
subtargetFeatureStrings (SubtargetPpc _ _ f) = 
  M.mapKeys (showPpcFeature . Just) f
subtargetFeatureStrings (SubtargetSparc _ _ f) = 
  M.mapKeys (showSparcFeature . Just) f
subtargetFeatureStrings (SubtargetSystemz _ _ f) = 
  M.mapKeys (showSystemzFeature . Just) f
subtargetFeatureStrings (SubtargetX86 _ _ f) = 
  M.mapKeys (showX86Feature . Just) f
subtargetFeatureStrings (SubtargetXcore _ _) = M.empty

data AnySubtarget = forall (a :: Backend). AnySubtarget (Subtarget a)
 
mapKeysMaybe :: (Ord k1, Ord k2) => (k1 -> Maybe k2) -> Map k1 v -> Map k2 v
mapKeysMaybe f x = 
  M.mapKeys fromJust $
  M.filterWithKey (\k _ -> isJust k) $
  M.mapKeys f x

parseAnySubtarget :: String -> String -> Map String Bool -> Maybe AnySubtarget
parseAnySubtarget targetStr cpuStr featureStrs =
  case parseTarget targetStr of
    Just TargetAarch64 ->
      Just $ AnySubtarget $
      SubtargetAarch64 TTargetAarch64 (parseAarch64Cpu cpuStr)
      (mapKeysMaybe parseAarch64Feature featureStrs)
    Just TargetArm ->
      Just $ AnySubtarget $
      SubtargetArm TTargetArm (parseArmCpu cpuStr)
      (mapKeysMaybe parseArmFeature featureStrs)
    Just TargetCpp ->
      Just $ AnySubtarget $
      SubtargetCpp TTargetCpp
    Just TargetHexagon ->
      Just $ AnySubtarget $
      SubtargetHexagon TTargetHexagon (parseHexagonCpu cpuStr)
      (mapKeysMaybe parseHexagonFeature featureStrs)
    Just TargetMips ->
      Just $ AnySubtarget $
      SubtargetMips TTargetMips (parseMipsCpu cpuStr)
      (mapKeysMaybe parseMipsFeature featureStrs)
    Just TargetMips64 ->
      Just $ AnySubtarget $
      SubtargetMips TTargetMips64 (parseMipsCpu cpuStr)
      (mapKeysMaybe parseMipsFeature featureStrs)
    Just TargetMips64el ->
      Just $ AnySubtarget $
      SubtargetMips TTargetMips64el (parseMipsCpu cpuStr)
      (mapKeysMaybe parseMipsFeature featureStrs)
    Just TargetMipsel ->
      Just $ AnySubtarget $
      SubtargetMips TTargetMipsel (parseMipsCpu cpuStr)
      (mapKeysMaybe parseMipsFeature featureStrs)
    Just TargetMsp430 ->
      Just $ AnySubtarget $
      SubtargetMsp430 TTargetMsp430 (parseMsp430Cpu cpuStr)
      (mapKeysMaybe parseMsp430Feature featureStrs)
    Just TargetNvptx ->
      Just $ AnySubtarget $
      SubtargetNvptx TTargetNvptx (parseNvptxCpu cpuStr)
      (mapKeysMaybe parseNvptxFeature featureStrs)
    Just TargetNvptx64 ->
      Just $ AnySubtarget $
      SubtargetNvptx TTargetNvptx64 (parseNvptxCpu cpuStr)
      (mapKeysMaybe parseNvptxFeature featureStrs)
    Just TargetPpc32 ->
      Just $ AnySubtarget $
      SubtargetPpc TTargetPpc32 (parsePpcCpu cpuStr)
      (mapKeysMaybe parsePpcFeature featureStrs)
    Just TargetPpc64 ->
      Just $ AnySubtarget $
      SubtargetPpc TTargetPpc64 (parsePpcCpu cpuStr)
      (mapKeysMaybe parsePpcFeature featureStrs)
    Just TargetPpc64le ->
      Just $ AnySubtarget $
      SubtargetPpc TTargetPpc64le (parsePpcCpu cpuStr)
      (mapKeysMaybe parsePpcFeature featureStrs)
    Just TargetR600 ->
      Just $ AnySubtarget $
      SubtargetR600 TTargetR600 (parseR600Cpu cpuStr)
      (mapKeysMaybe parseR600Feature featureStrs)
    Just TargetSparc ->
      Just $ AnySubtarget $
      SubtargetSparc TTargetSparc (parseSparcCpu cpuStr)
      (mapKeysMaybe parseSparcFeature featureStrs)
    Just TargetSparcv9 ->
      Just $ AnySubtarget $
      SubtargetSparc TTargetSparcv9 (parseSparcCpu cpuStr)
      (mapKeysMaybe parseSparcFeature featureStrs)
    Just TargetSystemz ->
      Just $ AnySubtarget $
      SubtargetSystemz TTargetSystemz (parseSystemzCpu cpuStr)
      (mapKeysMaybe parseSystemzFeature featureStrs)
    Just TargetThumb ->
      Just $ AnySubtarget $
      SubtargetArm TTargetThumb (parseArmCpu cpuStr)
      (mapKeysMaybe parseArmFeature featureStrs)
    Just TargetX86 ->
      Just $ AnySubtarget $
      SubtargetX86 TTargetX86 (parseX86Cpu cpuStr)
      (mapKeysMaybe parseX86Feature featureStrs)
    Just TargetX86_64 ->
      Just $ AnySubtarget $
      SubtargetX86 TTargetX86_64 (parseX86Cpu cpuStr)
      (mapKeysMaybe parseX86Feature featureStrs)
    Just TargetXcore ->
      Just $ AnySubtarget $
      SubtargetXcore TTargetXcore (parseXcoreCpu cpuStr)
    Nothing ->
      Just $ AnySubtarget $ SubtargetCpp TTargetCpp

anySubtargetTarget :: AnySubtarget -> Target
anySubtargetTarget (AnySubtarget t) = untagTarget (subtargetTarget t)

anySubtargetCpu :: AnySubtarget -> String
anySubtargetCpu (AnySubtarget t) = subtargetCpuString t

anySubtargetFeatures :: AnySubtarget -> Map String Bool
anySubtargetFeatures (AnySubtarget t) = subtargetFeatureStrings t

$(genStrEnum "Architecture" "Arch"
  ["aarch64", "arm", "hexagon", "mips", "mipsel", "mips64",  "mips64el",
   "msp430", "ppc64", "ppc64le", "ppc", "r600", "sparc", "sparcv9",
   "s390x", "tce", "thumb", "i386", "x86_64", "xcore", "nvptx",
   "nvptx64", "le32", "amdil", "spir", "spir64"] "unknown" False)

parseArchitecture :: String -> Maybe Architecture
parseArchitecture arch
  | x `elem` ["i386","i486","i586","i686","i786","i886","i986"] 
                                              = Just ArchI386
  | x `elem` ["amd64","x86_64","x86_64h"]     = Just ArchX86_64
  | x == "powerpc"                            = Just ArchPpc
  | x `elem` ["powerpc64","ppu"]              = Just ArchPpc64
  | x == "powerpc64le"                        = Just ArchPpc64le
  | x == "aarch64"                            = Just ArchAarch64
--  | x == "aarch64_be"                         = Just ArchAarch64_be
  | x `elem` ["arm","xscale"]                 = Just ArchArm
  | "armv" `isPrefixOf` x                     = Just ArchArm
--  | x == "armeb"                              = Just ArchArmeb
  | "thumbv" `isPrefixOf` x                   = Just ArchThumb
--  | x == "thumeb"                             = Just ArchThumbeb
--  | "thumbebv" `isPrefixOf` x                 = Just ArchThumbeb
--  | x == "arm64"                              = Just ArchArm64
--  | x == "arm64_be"                           = Just ArchArm64_be
  | x == "msp430"                             = Just ArchMsp430
  | x `elem` ["mips","mipseb","mipsallegrex"] = Just ArchMips
  | x `elem` ["mipsel","mipsallegrexel"]      = Just ArchMipsel
  | x `elem` ["mips64","mips64eb"]            = Just ArchMips64
  | x == "mips64el"                           = Just ArchMips64el
  | x == "r600"                               = Just ArchR600
  | x == "hexagon"                            = Just ArchHexagon
  | x == "s390x"                              = Just ArchS390x
  | x == "sparc"                              = Just ArchSparc
  | x `elem` ["sparcv9","sparc64"]            = Just ArchSparcv9
  | x == "tce"                                = Just ArchTce
  | x == "xcore"                              = Just ArchXcore
  | x == "nvptx"                              = Just ArchNvptx
  | x == "nvptx64"                            = Just ArchNvptx64
  | x == "le32"                               = Just ArchLe32
  | x == "amdil"                              = Just ArchAmdil
  | x == "spir"                               = Just ArchSpir
  | x == "spir64"                             = Just ArchSpir64
  | otherwise                                 = Nothing
  where x = map toLower arch

  -- ["aarch64", "arm", "cpp", "hexagon", "mips", "mips64", "mips64el",
  --  "mipsel", "msp430", "nvptx", "nvptx64", "ppc32", "ppc64", "ppc64le",
  --  "r600", "sparc", "sparcv9", "systemz", "thumb", "x86", "x86-64",
  --  "xcore"] "" True)

architectureToTarget :: Maybe Architecture -> Target
architectureToTarget (Just ArchAarch64) = TargetAarch64
architectureToTarget (Just ArchArm) = TargetArm
architectureToTarget (Just ArchHexagon) = TargetHexagon
architectureToTarget (Just ArchMips) = TargetMips
architectureToTarget (Just ArchMipsel) = TargetMipsel
architectureToTarget (Just ArchMips64) = TargetMips64
architectureToTarget (Just ArchMips64el) = TargetMips64el
architectureToTarget (Just ArchMsp430) = TargetMsp430
architectureToTarget (Just ArchPpc64) = TargetPpc64
architectureToTarget (Just ArchPpc64le) = TargetPpc64le
architectureToTarget (Just ArchPpc) = TargetPpc32
architectureToTarget (Just ArchR600) = TargetR600
architectureToTarget (Just ArchSparc) = TargetSparc
architectureToTarget (Just ArchSparcv9) = TargetSparcv9
architectureToTarget (Just ArchS390x) = TargetSystemz
architectureToTarget (Just ArchTce) = TargetThumb
architectureToTarget (Just ArchThumb) = TargetThumb
architectureToTarget (Just ArchI386) = TargetX86
architectureToTarget (Just ArchX86_64) = TargetX86_64
architectureToTarget (Just ArchXcore) = TargetXcore
architectureToTarget (Just ArchNvptx) = TargetNvptx
architectureToTarget (Just ArchNvptx64) = TargetNvptx64
architectureToTarget (Just ArchLe32) = TargetCpp
architectureToTarget (Just ArchAmdil) = TargetCpp
architectureToTarget (Just ArchSpir) = TargetCpp
architectureToTarget (Just ArchSpir64) = TargetCpp
architectureToTarget Nothing = TargetCpp

$(genStrEnum "Vendor" "Vendor"
  ["apple", "pc", "scei", "bgp", "bgq", "freescale", "ibm", "nvidia"]
  "unknown" True)
  
data OS = OSAuroraux
        | OSCygwin
        | OSDarwin
        | OSDragonfly
        | OSFreebsd
        | OSIos
        | OSKfreebsd
        | OSLinux
        | OSLv2
        | OSMacosx
        | OSMingw32
        | OSNetbsd
        | OSOpenbsd
        | OSSolaris
        | OSWin32
        | OSHaiku
        | OSMinix
        | OSRtems
        | OSNacl
        | OSCnk
        | OSBitrig
        | OSAix
        | OSCuda
        | OSNvcl
        deriving (Eq,Ord,Show,Bounded,Enum,Typeable,Data)
                 
showOS :: Maybe (OS,String) -> String
showOS Nothing = "unknown"
showOS (Just (OSAuroraux,x)) = "auroraux" ++ x
showOS (Just (OSCygwin,x)) = "cygwin" ++ x
showOS (Just (OSDarwin,x)) = "darwin" ++ x
showOS (Just (OSDragonfly,x)) = "dragonfly" ++ x
showOS (Just (OSFreebsd,x)) = "freebsd" ++ x
showOS (Just (OSIos,x)) = "ios" ++ x
showOS (Just (OSKfreebsd,x)) = "kfreebsd" ++ x
showOS (Just (OSLinux,x)) = "linux" ++ x
showOS (Just (OSLv2,x)) = "lv2" ++ x
showOS (Just (OSMacosx,x)) = "macosx" ++ x
showOS (Just (OSMingw32,x)) = "mingw32" ++ x
showOS (Just (OSNetbsd,x)) = "netbsd" ++ x
showOS (Just (OSOpenbsd,x)) = "openbsd" ++ x
showOS (Just (OSSolaris,x)) = "solaris" ++ x
showOS (Just (OSWin32,x)) = "win32" ++ x
showOS (Just (OSHaiku,x)) = "haiku" ++ x
showOS (Just (OSMinix,x)) = "minix" ++ x
showOS (Just (OSRtems,x)) = "rtems" ++ x
showOS (Just (OSNacl,x)) = "nacl" ++ x
showOS (Just (OSCnk,x)) = "cnk" ++ x
showOS (Just (OSBitrig,x)) = "bitrig" ++ x
showOS (Just (OSAix,x)) = "aix" ++ x
showOS (Just (OSCuda,x)) = "cuda" ++ x
showOS (Just (OSNvcl,x)) = "nvcl" ++ x

parseOS :: String -> Maybe (OS,String)
parseOS os 
  | "auroraux" `isPrefixOf` x  = Just (OSAuroraux, drop 8 x)
  | "cygwin" `isPrefixOf` x    = Just (OSCygwin, drop 6 x)
  | "darwin" `isPrefixOf` x    = Just (OSDarwin, drop 6 x)
  | "dragonfly" `isPrefixOf` x = Just (OSDragonfly, drop 9 x)
  | "freebsd" `isPrefixOf` x   = Just (OSFreebsd, drop 7 x)
  | "ios" `isPrefixOf` x       = Just (OSIos, drop 3 x)
  | "kfreebsd" `isPrefixOf` x  = Just (OSKfreebsd, drop 8 x)
  | "linux" `isPrefixOf` x     = Just (OSLinux, drop 5 x)
  | "lv2" `isPrefixOf` x       = Just (OSLv2, drop 3 x)
  | "macosx" `isPrefixOf` x    = Just (OSMacosx, drop 6 x)
  | "mingw32" `isPrefixOf` x   = Just (OSMingw32, drop 7 x)
  | "netbsd" `isPrefixOf` x    = Just (OSNetbsd, drop 6 x)
  | "openbsd" `isPrefixOf` x   = Just (OSOpenbsd, drop 7 x)
  | "solaris" `isPrefixOf` x   = Just (OSSolaris, drop 7 x)
  | "win32" `isPrefixOf` x     = Just (OSWin32, drop 5 x)
  | "haiku" `isPrefixOf` x     = Just (OSHaiku, drop 5 x)
  | "minix" `isPrefixOf` x     = Just (OSMinix, drop 5 x)
  | "rtems" `isPrefixOf` x     = Just (OSRtems, drop 5 x)
  | "nacl" `isPrefixOf` x      = Just (OSNacl, drop 4 x)
  | "cnk" `isPrefixOf` x       = Just (OSCnk, drop 3 x)
  | "bitrig" `isPrefixOf` x    = Just (OSBitrig, drop 6 x)
  | "aix" `isPrefixOf` x       = Just (OSAix, drop 3 x)
  | "cuda" `isPrefixOf` x      = Just (OSCuda, drop 4 x)
  | "nvcl" `isPrefixOf` x      = Just (OSNvcl, drop 4 x)
  | otherwise        = Nothing
  where x = map toLower os

$(genStrEnum "Environment" "Environment"
  ["gnu", "gnueabihf", "gnueabi", "gnux32", "code16", "eabi", "eabihf",
   "android", "msvc", "itanium", "cygnus"] "unknown" False)

parseEnvironment :: String -> Maybe Environment
parseEnvironment env
  | "gnueabihf" `isPrefixOf` x = Just EnvironmentGnueabihf
  | "gnueabi" `isPrefixOf` x   = Just EnvironmentGnueabi
  | "gnux32" `isPrefixOf` x    = Just EnvironmentGnux32
  | "gnu" `isPrefixOf` x       = Just EnvironmentGnu
  | "eabihf" `isPrefixOf` x    = Just EnvironmentEabihf
  | "code16" `isPrefixOf` x    = Just EnvironmentCode16
  | "eabi" `isPrefixOf` x      = Just EnvironmentEabi
  | "android" `isPrefixOf` x   = Just EnvironmentAndroid
  | "msvc" `isPrefixOf` x      = Just EnvironmentMsvc
  | "itanium" `isPrefixOf` x   = Just EnvironmentItanium
  | "cygnus" `isPrefixOf` x    = Just EnvironmentCygnus
  | otherwise                  = Nothing
  where x = map toLower env
                          
$(genStrEnum "Format" "Format" ["elf", "coff", "macho"] "unknown" False)

parseFormat :: String -> Maybe Format
parseFormat format
  | "elf" `isSuffixOf` x   = Just FormatElf
  | "coff" `isSuffixOf` x  = Just FormatCoff
  | "macho" `isSuffixOf` x = Just FormatMacho
  | otherwise              = Nothing
  where x = map toLower format

defaultFormat :: Maybe (OS,String) -> Maybe Format
defaultFormat Nothing = Nothing
defaultFormat (Just (OSCygwin,_))  = Just FormatCoff
defaultFormat (Just (OSMingw32,_)) = Just FormatCoff
defaultFormat (Just (OSWin32,_))   = Just FormatCoff
defaultFormat (Just (OSDarwin,_))  = Just FormatMacho
defaultFormat (Just (OSIos,_))     = Just FormatMacho
defaultFormat (Just (OSMacosx,_))  = Just FormatMacho
defaultFormat _                    = Just FormatElf

data Triple = Triple { tripleArchitecture :: Maybe Architecture,
                       tripleVendor :: Maybe Vendor,
                       tripleOS :: Maybe (OS, String),
                       tripleEnvironment :: Maybe Environment, 
                       tripleFormat :: Maybe Format }
              deriving (Eq,Ord,Show,Typeable,Data)

parseTriple :: String -> Triple
parseTriple x = 
  let triple = parseTriple' (splitOn "-" x) in
  if (tripleFormat triple) == Nothing
  then triple { tripleFormat = defaultFormat (tripleOS triple) }
  else triple
  where parseTriple' [] = Triple Nothing Nothing Nothing Nothing Nothing
        parseTriple' [a] = 
          Triple (parseArchitecture a) Nothing Nothing Nothing Nothing
        parseTriple' [a,b]
          | map toLower b == "unknown" || isJust (parseVendor b) =
              Triple (parseArchitecture a) (parseVendor b) 
              Nothing Nothing Nothing
          | isJust (parseOS b) =
              Triple (parseArchitecture a) Nothing (parseOS b)
              Nothing Nothing
          | otherwise = 
              Triple (parseArchitecture a) Nothing Nothing Nothing Nothing
        parseTriple' [a,b,c]
          | map toLower b == "unknown" || isJust (parseVendor b) =
              Triple (parseArchitecture a) (parseVendor b) (parseOS c) 
              Nothing Nothing
          | isJust (parseOS b) && isJust (parseFormat c) =
              Triple (parseArchitecture a) Nothing (parseOS b) Nothing
              (parseFormat c)
          | isJust (parseOS b) =
              Triple (parseArchitecture a) Nothing (parseOS b)
              (parseEnvironment c) (defaultFormat (parseOS b))
          | otherwise = Triple (parseArchitecture a) Nothing (parseOS c) 
                        Nothing Nothing
        parseTriple' [a,b,c,d]
          | map toLower b == "unknown" || isJust (parseVendor b) =
              Triple (parseArchitecture a) (parseVendor b) (parseOS c)
              (parseEnvironment d) Nothing
          | isJust (parseOS b) =
              Triple (parseArchitecture a) Nothing (parseOS b)  
              (parseEnvironment c) (parseFormat d)
          | otherwise = 
              Triple (parseArchitecture a) Nothing (parseOS c)
                     (parseEnvironment d) Nothing
        parseTriple' [a,b,c,d,e] = 
          Triple (parseArchitecture a) (parseVendor b) (parseOS c)
                 (parseEnvironment d) (parseFormat e)
        parseTriple' x' = parseTriple' (take 5 x')

showTriple :: Triple -> String
showTriple t
  | tripleEnvironment t == Nothing &&
    tripleFormat t == defaultFormat (tripleOS t) =
      showArchitecture (tripleArchitecture t) ++ "-" ++ 
      showVendor (tripleVendor t) ++ "-" ++ 
      showOS (tripleOS t)
  | tripleFormat t == defaultFormat (tripleOS t) =
    showArchitecture (tripleArchitecture t) ++ "-" ++ 
    showVendor (tripleVendor t) ++ "-" ++ 
    showOS (tripleOS t) ++ "-" ++
    showEnvironment (tripleEnvironment t)
  | otherwise =
    showArchitecture (tripleArchitecture t) ++ "-" ++ 
    showVendor (tripleVendor t) ++ "-" ++ 
    showOS (tripleOS t) ++ "-" ++
    showEnvironment (tripleEnvironment t) ++ "-" ++
    showFormat (tripleFormat t)

data Platform (a :: Backend) = 
  Platform { 
    platformSubtarget :: (Subtarget a), 
    platformTriple :: Triple }

data AnyPlatform = forall (a :: Backend). AnyPlatform (Platform a)

anyPlatformSubtarget :: AnyPlatform -> AnySubtarget
anyPlatformSubtarget (AnyPlatform p) = AnySubtarget (platformSubtarget p)

anyPlatformTriple :: AnyPlatform -> Triple
anyPlatformTriple (AnyPlatform p) = platformTriple p

mkAnyPlatform :: AnySubtarget -> Triple -> AnyPlatform
mkAnyPlatform (AnySubtarget s) t = AnyPlatform (Platform s t)

mkAnyPlatformFromTriple :: Triple -> AnyPlatform
mkAnyPlatformFromTriple t =
  case architectureToTarget (tripleArchitecture t) of
    TargetAarch64 -> 
      AnyPlatform $ 
      Platform (SubtargetAarch64 TTargetAarch64 Nothing M.empty) t
    TargetArm ->
      AnyPlatform $
      Platform (SubtargetArm TTargetArm Nothing M.empty) t
    TargetCpp ->
      AnyPlatform $
      Platform (SubtargetCpp TTargetCpp) t
    TargetHexagon ->
      AnyPlatform $
      Platform (SubtargetHexagon TTargetHexagon Nothing M.empty) t
    TargetMips ->
      AnyPlatform $
      Platform (SubtargetMips TTargetMips Nothing M.empty) t
    TargetMips64 ->
      AnyPlatform $
      Platform (SubtargetMips TTargetMips64 Nothing M.empty) t
    TargetMips64el ->
      AnyPlatform $
      Platform (SubtargetMips TTargetMips64el Nothing M.empty) t
    TargetMipsel ->
      AnyPlatform $
      Platform (SubtargetMips TTargetMipsel Nothing M.empty) t
    TargetMsp430 ->
      AnyPlatform $
      Platform (SubtargetMsp430 TTargetMsp430 Nothing M.empty) t
    TargetNvptx ->
      AnyPlatform $
      Platform (SubtargetNvptx TTargetNvptx Nothing M.empty) t
    TargetNvptx64 ->
      AnyPlatform $
      Platform (SubtargetNvptx TTargetNvptx64 Nothing M.empty) t
    TargetPpc32 ->
      AnyPlatform $
      Platform (SubtargetPpc TTargetPpc32 Nothing M.empty) t
    TargetPpc64 ->
      AnyPlatform $
      Platform (SubtargetPpc TTargetPpc64 Nothing M.empty) t
    TargetPpc64le ->
      AnyPlatform $
      Platform (SubtargetPpc TTargetPpc64le Nothing M.empty) t
    TargetR600 ->
      AnyPlatform $
      Platform (SubtargetR600 TTargetR600 Nothing M.empty) t
    TargetSparc ->
      AnyPlatform $
      Platform (SubtargetSparc TTargetSparc Nothing M.empty) t
    TargetSparcv9 ->
      AnyPlatform $
      Platform (SubtargetSparc TTargetSparcv9 Nothing M.empty) t
    TargetSystemz ->
      AnyPlatform $
      Platform (SubtargetSystemz TTargetSystemz Nothing M.empty) t
    TargetThumb ->
      AnyPlatform $
      Platform (SubtargetArm TTargetThumb Nothing M.empty) t
    TargetX86 ->
      AnyPlatform $
      Platform (SubtargetX86 TTargetX86 Nothing M.empty) t
    TargetX86_64 ->
      AnyPlatform $
      Platform (SubtargetX86 TTargetX86_64 Nothing M.empty) t
    TargetXcore ->
      AnyPlatform $
      Platform (SubtargetXcore TTargetXcore Nothing) t