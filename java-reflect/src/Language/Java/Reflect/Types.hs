{-# LANGUAGE Haskell2010
    , DeriveDataTypeable
 #-}
{-# OPTIONS -Wall #-}

-- |
-- Module       : Foreign.Java.Bindings.JavaTypes
-- Copyright    : (c) Julian Fleischer 2013
-- License      : MIT (See LICENSE file in cabal package)
--
-- Maintainer   : julian.fleischer@fu-berlin.de
-- Stability    : provisional
-- Portability  : non-portable (DeriveDataTypeable)
--
-- Data structures that describe the interface of
-- Java structures such as classes, generic types,
-- their methods, etc.
--
-- All types are instances of 'Data' and 'Typeable' and
-- can therefor be used with /Scrap Your Boilerplate/
-- combinators (see "Data.Generics").
module Language.Java.Reflect.Types where

import Foreign.Java.Utils
import Data.Int
import Data.Data

-- type JavaClassCache = Map (String, [JavaType]) JavaClass

-- | A JavaType is either a Primitive Type, an Array, or an Object.
data JavaType
    = JBoolean
    | JChar
    | JByte
    | JShort
    | JInt
    | JLong
    | JFloat
    | JDouble
    | JObj {
        typeName :: String
      }
    | JArr {
        componentType :: JavaType
      }
  deriving (Eq, Ord, Show, Read, Data, Typeable)


printJavaType :: JavaType -> String
printJavaType t = case t of
    JBoolean -> "boolean"
    JChar -> "char"
    JByte -> "byte"
    JShort -> "short"
    JInt -> "int"
    JLong -> "long"
    JFloat -> "float"
    JDouble -> "double"
    JObj n -> n
    JArr c -> printJavaType c ++ "[]"

-- | The interface of a Java class.
data JavaClass = JavaClass {
    className :: String,
    classPackage :: Maybe String,
    classModName :: String,
    classParents :: [String],
    classIfaces :: [String],
    classConstructors :: [JavaConstructor],
    classMethods :: [JavaMethod],
    classFields :: [JavaField],
    classTypeParams :: [JavaTypeParam],
    classEnum :: Bool,
    classEnumConstants :: [(Int32, String)],
    classIface :: Bool,
    classAnnotation :: Bool,
    classAbstract :: Bool,
    classFinal :: Bool
  } deriving (Eq, Show, Read, Data, Typeable)

data JavaClassType = Annotation | Interface | Enum | Class | Exception | Error
    deriving (Eq, Ord, Show, Read, Data, Typeable)


classType :: JavaClass -> JavaClassType
-- ^ Determines the 'JavaClassType' of a 'JavaClass'.
classType clazz
    | "java.lang.Error"     `elem` classParents clazz = Error
    | "java.lang.Exception" `elem` classParents clazz = Exception
    | classAnnotation clazz = Annotation
    | classIface clazz      = Interface
    | classEnum clazz       = Enum
    | otherwise             = Class


classDependencies :: JavaClass -> [String]
-- ^ Calculate all classes that are referenced in any way by this class.
classDependencies clazz = concat [
    classParents clazz,
    classIfaces clazz,
    concatMap fieldDependencies (classFields clazz),
    concatMap methodDependencies (classMethods clazz),
    concatMap constructorDependencies (classConstructors clazz) ]

classFullName :: JavaClass -> String
-- ^ Derive the full name from a class definition. See also 'makeName'.
classFullName clazz = makeName (classPackage clazz) (className clazz)
    

-- | A Type variable declaration.
data JavaTypeParam = JavaTypeParam {
    paramName :: TyVar,
    paramBounds :: [JavaGenericType]
  } deriving (Eq, Ord, Show, Read, Data, Typeable)

-- | A Type variable. This is merely a name.
newtype TyVar = TyVar { tyVarName :: String }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

data JavaGenericType
    = -- | @<? super X>@, @<? extends X>@
      Wildcard {
        jgtBounds :: [JavaGenericType],
        jgtLowerBounds :: [JavaGenericType]
    }
    | -- | @java.util.List<X>@
      Parameterized {
        -- | The full name of the base type, e.g. @java.lang.Class@.
        jgtBasetype :: String,
        -- | The parameters.
        jgtParameters :: [JavaGenericType]
    }
    | -- | @X[]@
      GenericArray {
        -- | The base type of the generic array, e.g. @java.lang.Number@.
        jgtComponentType :: JavaGenericType
    }
    | -- | @<X>@
      TypeVarReference {
        -- | The name of the type variable, e.g. @E@ or @X@.
        jgtName :: TyVar
    }
    | -- java.lang.Object
      NotSoGeneric {
        -- | A plain type, full name. Used for example in a 'Parameterized'
        -- type, which may be parameterized by a plain type (like
        -- @Class<Number>@.
        jgtBasetype :: String
    } deriving (Eq, Ord, Show, Read, Data, Typeable)

-- | The type of a generic type.
data JavaGenericTypeType = WildcardT
                         | ParameterizedT
                         | GenericArrayT
                         | TypeVarReferenceT
                         | NotSoGenericT

jgtType :: JavaGenericType -> JavaGenericTypeType
-- ^ Get the type of a generic type.
jgtType t = case t of
    (Wildcard _ _)       -> WildcardT
    (Parameterized _ _)  -> ParameterizedT
    (GenericArray _)     -> GenericArrayT
    (TypeVarReference _) -> TypeVarReferenceT
    (NotSoGeneric _)     -> NotSoGenericT

-- | The interface to a field in the Java language.
data JavaField = JavaField {
    fieldName :: String,
    fieldType :: (JavaType, JavaGenericType),
    fieldFinal :: Bool,
    fieldStatic :: Bool
  } deriving (Eq, Show, Read, Data, Typeable)

fieldDependencies :: JavaField -> [String]
fieldDependencies field = case fst (fieldType field) of
    JObj n -> [n]
    _ -> []

-- | The interface to a method in the Java language.
data JavaMethod = JavaMethod {
    methodName :: String,
    methodName' :: String,
    methodParams :: [(JavaType, JavaGenericType)],
    methodReturnType :: (Maybe JavaType, JavaGenericType),
    methodExceptions :: [String],
    methodTypeParams :: [JavaTypeParam],
    methodStatic :: Bool,
    methodAbstract :: Bool,
    methodFinal :: Bool,
    methodNative :: Bool,
    methodSynchronized :: Bool
  } deriving (Eq, Show, Read, Data, Typeable)

methodDependencies :: JavaMethod -> [String]
-- ^ Return the full names of all classes that this method
-- references in its definition.
methodDependencies method = foldl classReference [] referenced
  where
    referenced = maybe paramTypes (: paramTypes) (fst $ methodReturnType method)
    paramTypes = map fst $ methodParams method
    classReference r c = case c of
        JObj n -> n : r
        _ -> r

-- | A Constructor in the Java language.
data JavaConstructor = JavaConstructor {
    constructorParams :: [(JavaType, JavaGenericType)],
    constructorExceptions :: [String],
    constructorTypeParams :: [JavaTypeParam]
  } deriving (Eq, Show, Read, Data, Typeable)

constructorDependencies :: JavaConstructor -> [String]
-- ^ Retrieve all classes that this constructor definition
-- references in its parameters or generic declaration.
constructorDependencies constr = foldl classReference []
                              $ map fst $ constructorParams constr
  where
    classReference r c = case c of
        JObj n -> n : r
        _ -> r


