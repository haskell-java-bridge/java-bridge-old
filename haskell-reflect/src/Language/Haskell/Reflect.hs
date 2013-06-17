{-# LANGUAGE Haskell2010
    , TupleSections
 #-}
{-# OPTIONS
    -Wall
    -fno-warn-name-shadowing
 #-}

module Language.Haskell.Reflect where

import Prelude

import Data.Either
--import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad
import qualified Control.Monad.CatchIO as C
import qualified Control.Monad.Error as E

import Language.Haskell.Interpreter
import Language.Haskell.Reflect.Types

--import Text.Parsec

{-
readTypeFromSignature :: String -> [HType]
readTypeFromSignature signature = runParserT parse () "-" signature >>= either [] id
  where
    pTypes = sepBy pType (string " -> ")
    pType = pParens <|> pVar
    pParens = do
        char '('
        types <- pTypes
        char ')'
        return types
    pVar = do
-}      

readContextFromSignature :: String -> a
readContextFromSignature _context = undefined

readKindFromSignature :: String -> a
readKindFromSignature _kind = undefined


checkModuleAvailability :: (Functor m, C.MonadCatchIO m)
                        => [String] -> m [(String, Bool)]
-- ^ Checks whether the given modules are installed in the
-- package database or not.
checkModuleAvailability names = do
    result <- runInterpreter $ do
        set [ installedModulesInScope := True
            , languageExtensions := [TemplateHaskell, MagicHash] ]
        let check name = E.catchError
                (getModuleExports name >>= return . const True)
                (return . const False)
        mapM check names >>= return . zip names
    either C.throw return result
        

reflectModules :: (Functor m, C.MonadCatchIO m)
               => [String] -> m (Map String InterpreterError, Map String HModule)
reflectModules names = do
    result <- runInterpreter $ do
        set [ installedModulesInScope := True
            , languageExtensions := [TemplateHaskell, MagicHash] ]

        let browseModule name = E.catchError
                (getModuleExports name >>= return . Right . (name,))
                (return . Left . (name,))

            emptyModule name = HModule {
                moduleName = name,
                moduleFunctions = Map.empty,
                moduleTypeclasses = Map.empty,
                moduleDatatypes = Map.empty
              }

            reflectModule (modName, elems) = do
                reflectedElems <- foldM reflectElems (emptyModule modName) elems
                return (modName, reflectedElems)
              where
                reflectElems mod elem = case elem of
                    Fun name -> do
                        func <- reflectFun name
                        return $ mod {
                            moduleFunctions = (Map.insert name func (moduleFunctions mod))
                          }
                    Class name members -> do
                        clazz <- reflectClass name members
                        return $ mod {
                            moduleTypeclasses = (Map.insert name clazz (moduleTypeclasses mod))
                          }
                    Data name constructors -> do
                        data_ <- reflectData name constructors
                        return $ mod {
                            moduleDatatypes = (Map.insert name data_ (moduleDatatypes mod))
                          }

                reflectFun name = do
                    signature <- typeOf $ modName ++ "." ++ name
                    return $ HFunction {
                        functionName = name,
                        functionSignature = signature,
                        functionType = [], --readTypeFromSignature signature,
                        functionContext = []
                      }

                reflectClass name _members = do
                    return $ HTypeclass {
                        typeclassName = name,
                        typeclassMembers = [],
                        typeclassInstances = []
                      }

                reflectData name _constructors = do
                    kind <- kindOf $ modName ++ "." ++ name
                    return $ HDatatype {
                        datatypeName = name,
                        datatypeKind = kind,
                        datatypeConstructors = Map.empty
                      }
                    
                
        modules <- mapM browseModule names
        reflectedModules <- mapM reflectModule $ rights modules
        return (Map.fromList $ lefts modules, Map.fromList $ reflectedModules)
        
    either C.throw return result


reflectTypeclasses :: (Functor m, C.MonadCatchIO m)
                   => [String] -> m [(String, Maybe HTypeclass)]
reflectTypeclasses names = do
    result <- runInterpreter $ do
        set [ installedModulesInScope := True
            , languageExtensions := [TemplateHaskell, MagicHash] ]
        setImports ["Prelude",
                    "Language.Haskell.Reflect.Types",
                    "Language.Haskell.Reflect.Utils",
                    "Language.Haskell.TH",
                    "Language.Haskell.TH.Quote"]
        let query = \name -> "$(reify ''" ++ name
                        ++ " >>= typeclassInfo >>= dataToExpQ (const Nothing))"
            doIt q = E.catchError (interpret q (as :: HTypeclass) >>= return . Just)
                                  (return . const Nothing)
        mapM (doIt . query) names >>= return . zip names
    either C.throw return result


