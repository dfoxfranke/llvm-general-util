{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, DeriveGeneric #-}
module LLVM.General.Util.Test.Platform.SmallCheck where

import Data.Maybe
import Data.List
import Data.String
import Control.Monad.Trans.Error
import LLVM.General.Util.Platform
import LLVM.General.Target
import Test.SmallCheck
import Test.SmallCheck.Series

instance (Monad m) => Serial m Architecture
instance (Monad m) => Serial m Vendor
instance (Monad m) => Serial m OS
instance (Monad m) => Serial m Environment
instance (Monad m) => Serial m Format

newtype OSSuffixChar = OSSuffixChar { unOSSuffixChar :: Char }

instance (Monad m) => Serial m OSSuffixChar where
  series = foldr1 (\/) [ cons0 (OSSuffixChar c)
                       | c <- ['!'..','] ++ ['.'..'~']]

instance (Monad m) => Serial m OSSuffix where
  series = cons0 (fromString "") \/
           cons2 (\x xs -> fromString $ unOSSuffixChar x : showOSSuffix xs)
  
instance (Monad m) => Serial m Triple

roundTripPreservesTriple :: Bool -> Triple -> IO (Either Reason Reason)
roundTripPreservesTriple setArch triple =
  do let tripleStr = showTriple triple
     let archStr = Just $ showTarget $ Just $ architectureToTarget $
           tripleArchitecture triple
     result <- runErrorT $ lookupTarget 
               (if setArch then archStr else Nothing) 
               tripleStr
     return $ case result of
       Left str -> Left $ "lookupTarget errored with " ++ show str
       Right (_,tripleStr') ->
         let triple' = parseTriple tripleStr' in
         if triple == triple' ||
            tripleFormat triple == Nothing &&
            triple { tripleFormat = defaultFormat (tripleOS triple) } == triple'
         then Right "triples match"
         else Left $ "triples don't match: put in (" ++ show triple ++
              "), got out (" ++ show triple' ++ ")"

-- | No OS name is a prefix of any other OS name. This is important because
-- otherwise parsing is ambiguous.
noOSPrefixes :: (Monad m) => Property m
noOSPrefixes = 
  forAll (\(x,y) -> 
           x == y || 
           not (showOS (Just (x,"")) `isPrefixOf` showOS (Just (y,""))))

-- | Printing a triple, looking up a target by that triple-string, and
-- reparsing the triple-string that LLVM returns gives back the
-- original triple.
allTriplesPreserved :: Property IO
allTriplesPreserved = 
  forAll (\x -> monadic (roundTripPreservesTriple False x))

-- | Like 'allTriplesPreserved', but also use 'architectToTarget' to specify
-- an explicit target to LLVM.
allTriplesPreservedSettingArch :: Property IO
allTriplesPreservedSettingArch = 
  forAll (\x -> monadic (roundTripPreservesTriple True x))
  