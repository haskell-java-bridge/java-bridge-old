{-# LANGUAGE Haskell2010
    , DeriveDataTypeable
    , GeneralizedNewtypeDeriving
 #-}
{-# OPTIONS -Wall #-}

module Language.Haskell.Reflect.Types where

import Data.Char
import Data.Data
import Data.Map
import Data.String

data HName = TyVar String | QName String
    deriving (Data, Typeable, Show, Read, Eq, Ord)

instance IsString HName where
    fromString n@(x:_)
        | isUpper x = QName n
        | otherwise = TyVar n
    fromString "" = error "empty name"
    fromString _  = error "not a name"


data HModule = HModule {
    moduleName :: String,
    moduleFunctions :: Map String HFunction,
    moduleTypeclasses :: Map String HTypeclass,
    moduleDatatypes :: Map String HDatatype
  } deriving (Data, Typeable, Show, Read, Eq)

data HType = HType {
    typeName :: HName,
    typeArgs :: [HType]
  } deriving (Data, Typeable, Show, Read, Eq)

data HFunction = HFunction {
    functionName :: String,
    functionType :: [HType],
    functionSignature :: String,
    functionContext :: [HType]
  } deriving (Data, Typeable, Show, Read, Eq)

data HTypeclass = HTypeclass {
    typeclassName :: String,
    typeclassMembers :: [HFunction],
    typeclassInstances :: [HInstance]
  } deriving (Data, Typeable, Show, Read, Eq)

data HInstance = HInstance {
  } deriving (Data, Typeable, Show, Read, Eq)

data HDatatype = HDatatype {
    datatypeName :: String,
    datatypeKind :: String,
    datatypeConstructors :: Map String HFunction
  } deriving (Data, Typeable, Show, Read, Eq)



