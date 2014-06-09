module Main where

import LLVM.General.Target(initializeAllTargets)
import LLVM.General.Util.Test.Platform.SmallCheck

import Test.Framework
import qualified Test.Framework.Providers.SmallCheck as SC

main :: IO ()
main = do initializeAllTargets
          defaultMain tests

tests = 
  [testGroup "Triples" 
   [
     SC.testProperty "noOSPrefixes" noOSPrefixes,
     SC.withDepth 3 $ SC.testProperty "allTriplesPreserved" allTriplesPreserved,
     SC.withDepth 3 $ SC.testProperty "allTriplesPreservedSettingArch" allTriplesPreservedSettingArch
   ]
  ]
      