{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric, ParallelListComp #-}

module LLVM.General.Util.Internal.TH(genStrEnum) where

import Data.Char
import Data.Data(Data)
import Data.Functor
import Data.Maybe
import Data.Typeable(Typeable)
import GHC.Generics
import Language.Haskell.TH
mkDconName :: String -> String -> Name
mkDconName prefix [] = mkName prefix
mkDconName prefix (x:xs) =
  mkName $ prefix ++ normalizeHead x : (normalizeTail <$> xs)
  where normalizeHead x
          | isAlpha x || isDigit x = toUpper x
          | otherwise = '_'
        normalizeTail x
          | isAlpha x || isDigit x = toLower x
          | otherwise = '_'

genStrEnum :: String -> String -> [String] -> String -> Bool -> Q [Dec]
genStrEnum tycon dconPrefix strs defaultStr wantParse =
  do lowerStr <- newName "lowerStr"
     inputStr <- newName "inputStr"
     let tyconName = mkName tycon
     let parseName = mkName $ "parse" ++ tycon
     let showName  = mkName $ "show" ++ tycon
     let dconNames = mkDconName dconPrefix <$> strs
     dataDecl <-
       dataD (cxt []) tyconName []
       (flip normalC [] <$> dconNames)
       [''Eq, ''Ord, ''Show, ''Enum, ''Bounded, ''Typeable, ''Data, ''Generic]
     parseDecl <- sigD parseName [t|String -> Maybe $(conT tyconName)|]
     parseDef <-
       funD parseName
       [clause
        [varP inputStr]
        (guardedB $
         [normalGE [|$(varE lowerStr) == $(litE $ stringL (map toLower str))|]
                   [|Just $(conE dconName)|]
          | str <- strs | dconName <- dconNames] ++
         [normalGE [|otherwise|] [|Nothing|]])
         [head <$> [d|$(varP lowerStr) = map toLower $(varE inputStr)|]]]
     showDecl <- sigD showName [t|Maybe $(conT tyconName) -> String|]
     showDef <-
       funD showName $
       [clause [conP 'Just [conP dconName []]]
               (normalB (litE (stringL str))) []
        | str <- strs | dconName <- dconNames] ++
       [clause [conP 'Nothing []] (normalB (litE (stringL defaultStr))) []]
     return $ if wantParse then [dataDecl,parseDecl,parseDef,showDecl,showDef]
              else [dataDecl,showDecl,showDef]
