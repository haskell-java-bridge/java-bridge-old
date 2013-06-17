{-# LANGUAGE Haskell2010 #-}
{-# OPTIONS
    -Wall
    -fno-warn-missing-signatures
    -fno-warn-name-shadowing
 #-}

-- |
-- Module       : Foreign.Java.Bindings.ReflectJava
-- Copyright    : (c) Julian Fleischer 2013
-- License      : MIT (See LICENSE file in cabal package)
--
-- Maintainer   : julian.fleischer@fu-berlin.de
-- Stability    : provisional
--
-- Methods for reflecting Java classes using a JVM as source of information.
module Language.Java.Reflect (
    findClasses,
    reflectClasses,

    module Language.Java.Reflect.Types

  ) where


import Language.Java.Reflect.Types

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Set as Set
import Data.Set (Set)

import Data.Int
import Data.Word
import Data.Maybe (fromJust)

import Foreign.Java
import Foreign.Java.Utils


definitely = maybe (error "The impossible happened! Please report this as a bug.")


-- (
--  [Methods = [types] ],
--  (Maybe Parentclass, [Interfaces])
-- )
type Class = ([String], (Maybe String, [String]))


findClasses :: Word32 -> [String] -> Java [String] --Java (Map String Class)
findClasses maxDepth classNamesToFind = do

    (Just clazz)   <- getClass "java.lang.Class"
    (Just cMethod) <- getClass "java.lang.reflect.Method"
    (Just cConstr) <- getClass "java.lang.reflect.Constructor"
    (Just cField)  <- getClass "java.lang.reflect.Field"

    getMethods <- clazz `bindMethod` "getMethods"
        ::= array (object "java.lang.reflect.Method")
    getConstructors <- clazz `bindMethod` "getConstructors"
        ::= array (object "java.lang.reflect.Constructor")
    getFields <- clazz `bindMethod` "getFields"
        ::= array (object "java.lang.reflect.Field")

    getFieldType <- cField `bindMethod` "getType"
        ::= object "java.lang.Class"
    getReturnType <- cMethod `bindMethod` "getReturnType"
        ::= object "java.lang.Class"
    getClassName <- clazz `bindMethod` "getName"
        ::= string
    getParametersM <- cMethod `bindMethod` "getParameterTypes"
        ::= array (object "java.lang.Class")
    getParametersC <- cConstr `bindMethod` "getParameterTypes"
        ::= array (object "java.lang.Class")
    getSuperclass <- clazz `bindMethod` "getSuperclass"
        ::= object "java.lang.Class"
    getInterfaces <- clazz `bindMethod` "getInterfaces"
        ::= array (object "java.lang.Class")

    (Just classLoader) <- getClass "java.lang.ClassLoader"
    getSystemClassLoader <- classLoader `bindStaticMethod` "getSystemClassLoader"
        ::= object "java.lang.ClassLoader"
    (Just systemClassLoader) <- getSystemClassLoader

    (Just loadClass) <- classLoader `getMethod` "loadClass"
        ::= string --> boolean --> object "java.lang.Class"

    let getClass' :: String -> Java (Maybe JObject)
        getClass' clazz = callMethodE loadClass systemClassLoader clazz False
            >>= return . either (const Nothing) id

        readClass :: JObject -> Java Class
        readClass clazz = do
            constructors <- getConstructors clazz
                >>= toList . fromJust
                >>= mapM (readConstructor . fromJust)
                >>= return . concat
            methods <- getMethods clazz
                >>= toList . fromJust
                >>= mapM (readMethod . fromJust)
                >>= return . concat
            fields <- getFields clazz
                >>= toList . fromJust
                >>= mapM (readField . fromJust)
            superclass <- getSuperclass clazz
                >>= maybe (return Nothing) getClassName
            interfaces <- getInterfaces clazz
                >>= toList . fromJust
                >>= mapM (getClassName . fromJust)
                >>= return . map fromJust
            return (constructors ++ fields ++ methods, (superclass, interfaces))

        readField :: JObject -> Java String
        readField field = do
            (Just fieldType) <- getFieldType field >>= getClassName . fromJust

            return fieldType

        readMethod :: JObject -> Java [String]
        readMethod method = do
            args <- readMethodParameters method
--          excs <- readMethodExceptions method
            (Just returnType) <- getReturnType method >>= getClassName . fromJust
            
            return (returnType : args)

        readConstructor :: JObject -> Java [String]
        readConstructor constr = do
            args <- readConstructorParameters constr
--          excs <- readConstructorExceptions constr
            
            return args

        readMethodParameters :: JObject -> Java [String]
        readMethodParameters method = getParametersM method
            >>= toList . fromJust
            >>= mapM (getClassName . fromJust)
            >>= return . map fromJust

        readConstructorParameters :: JObject -> Java [String]
        readConstructorParameters method = getParametersC method
            >>= toList . fromJust
            >>= mapM (getClassName . fromJust)
            >>= return . map fromJust

        readReferences :: Class -> [String]
        readReferences (refs1, (super, ifaces)) =
            let refs2 = maybe ifaces (: ifaces) super
            in refs2 ++ refs1

        isPrimitive x = head x == '[' || x `elem` ["byte", "short", "int", "long",
                                                   "float", "double", "boolean", "char",
                                                   "void"]

        resolve :: Map String Class -> Set String -> [String]
                -> Java (Map String Class, Set String)
        resolve map set css = case css of
            (x:xs) -> if x `Map.member` map
                then do resolve map set xs
                else getClass' x >>= maybe (resolve map set xs) (\clazz -> do
                        clazz' <- readClass clazz
                        let newClasses = Set.filter (`Map.notMember` map)
                                            $ Set.fromList (readReferences clazz')
                        resolve (Map.insert x clazz' map)
                                (newClasses `Set.union` set)
                                xs)
            [] -> do return (map, Set.filter (not . isPrimitive) set)

        findAll :: Word32 -> Map String Class -> [String] -> Java (Map String Class)
        findAll 0 classes _xs = return classes
        findAll limit classes xs = do
            (classes', new) <- resolve classes Set.empty xs
            if Set.null new
                then return classes'
                else findAll (pred limit) classes' (Set.toList new)
    
    Map.keys `fmap` findAll maxDepth Map.empty classNamesToFind

reflectClasses :: [String] -> Java [JavaClass]
reflectClasses classes = do

    (Just clazz)     <- getClass "java.lang.Class"
    (Just cField)    <- getClass "java.lang.reflect.Field"
    (Just cMethod)   <- getClass "java.lang.reflect.Method"
    (Just cConstr)   <- getClass "java.lang.reflect.Constructor"
    (Just cTypeV)    <- getClass "java.lang.reflect.TypeVariable"
    (Just cParamT)   <- getClass "java.lang.reflect.ParameterizedType"
    (Just cWildT)    <- getClass "java.lang.reflect.WildcardType"
    (Just cGenArrT)  <- getClass "java.lang.reflect.GenericArrayType"
    (Just cEnum)     <- getClass "java.lang.Enum"
    (Just modifiers) <- getClass "java.lang.reflect.Modifier"

    getMethods <- clazz `bindMethod` "getMethods"
        ::= array (object "java.lang.reflect.Method")
    getFields <- clazz `bindMethod` "getFields"
        ::= array (object "java.lang.reflect.Field")
    getReturnType <- cMethod `bindMethod` "getReturnType"
        ::= object "java.lang.Class"
    getName <- clazz `bindMethod` "getName"
        ::= string
    getMethodName <- cMethod `bindMethod` "getName"
        ::= string
    getFieldName <- cField `bindMethod` "getName"
        ::= string
    getFieldType <- cField `bindMethod` "getType"
        ::= object "java.lang.Class"
    getGenericType <- cField `bindMethod` "getGenericType"
        ::= object "java.lang.reflect.Type"
    getParameters <- cMethod `bindMethod` "getParameterTypes"
        ::= array (object "java.lang.Class")
    getParameters' <- cConstr `bindMethod` "getParameterTypes"
        ::= array (object "java.lang.Class")
    getGenericParametersM <- cMethod `bindMethod` "getGenericParameterTypes"
        ::= array (object "java.lang.reflect.Type")
    getGenericParametersC <- cConstr `bindMethod` "getGenericParameterTypes"
        ::= array (object "java.lang.reflect.Type")
    getGenericReturnType <- cMethod `bindMethod` "getGenericReturnType"
        ::= object "java.lang.reflect.Type"
    getSuperclass <- clazz `bindMethod` "getSuperclass"
        ::= object "java.lang.Class"
    getInterfaces <- clazz `bindMethod` "getInterfaces"
        ::= array (object "java.lang.Class")
    getModifiersC <- clazz `bindMethod` "getModifiers"
        ::= int
    getModifiersM <- cMethod `bindMethod` "getModifiers"
        ::= int
    getModifiersF <- cField `bindMethod` "getModifiers"
        ::= int
    getConstructors <- clazz `bindMethod` "getConstructors"
        ::= array (object "java.lang.reflect.Constructor")
    getComponentType <- clazz `bindMethod` "getComponentType"
        ::= object "java.lang.Class"
    getEnumConstants <- clazz `bindMethod` "getEnumConstants"
        ::= array (object "java.lang.Object")
    getTypeParameters <- clazz `bindMethod` "getTypeParameters"
        ::= array (object "java.lang.reflect.TypeVariable")
    getTypeParametersC <- cConstr `bindMethod` "getTypeParameters"
        ::= array (object "java.lang.reflect.TypeVariable")
    getTypeParametersM <- cMethod `bindMethod` "getTypeParameters"
        ::= array (object "java.lang.reflect.TypeVariable")

    -- reflect TypeVariables
    getTypeVariableName <- cTypeV `bindMethod` "getName"
        ::= string
    getTypeVariableBounds <- cTypeV `bindMethod` "getBounds"
        ::= array (object "java.lang.reflect.Type")

    -- reflect ParameterizedTypes
    getActualTypeArguments <- cParamT `bindMethod` "getActualTypeArguments"
        ::= array (object "java.lang.reflect.Type")
    getRawType <- cParamT `bindMethod` "getRawType"
        ::= object "java.lang.reflect.Type"
    
    -- reflect WildcardTypes
    getLowerBounds <- cWildT `bindMethod` "getLowerBounds"
        ::= array (object "java.lang.reflect.Type")
    getUpperBounds <- cWildT `bindMethod` "getUpperBounds"
        ::= array (object "java.lang.reflect.Type")

    -- reflect GenericArrayTypes
    getGenericComponentType <- cGenArrT `bindMethod` "getGenericComponentType"
        ::= object "java.lang.reflect.Type"

    -- reflect Enum types
    getEnumName <- cEnum `bindMethod` "name"
        ::= string
    getEnumOrdinal <- cEnum `bindMethod` "ordinal"
        ::= int

    isAnnotation <- clazz `bindMethod` "isAnnotation"
        ::= boolean
    isArray <- clazz `bindMethod` "isArray"
        ::= boolean
    isEnum <- clazz `bindMethod` "isEnum"
        ::= boolean
    isInterface <- clazz `bindMethod` "isInterface"
        ::= boolean
    isPrimitive <- clazz `bindMethod` "isPrimitive"
        ::= boolean

    isStatic <- modifiers `bindStaticMethod` "isStatic"
        ::= int --> boolean
    isAbstract <- modifiers `bindStaticMethod` "isAbstract"
        ::= int --> boolean
    isFinal <- modifiers `bindStaticMethod` "isFinal"
        ::= int --> boolean
    isNative <- modifiers `bindStaticMethod` "isNative"
        ::= int --> boolean
    isSynchronized <- modifiers `bindStaticMethod` "isSynchronized"
        ::= int --> boolean

    (Just classLoader) <- getClass "java.lang.ClassLoader"
    getSystemClassLoader <- classLoader `bindStaticMethod` "getSystemClassLoader"
        ::= object "java.lang.ClassLoader"
    (Just systemClassLoader) <- getSystemClassLoader
    (Just loadClass) <- classLoader `getMethod` "loadClass"
        ::= string --> boolean --> object "java.lang.Class"

    let findParentClasses :: JObject -- of type java.lang.Class
                          -> Java [JObject]
        findParentClasses clazz = do
            parent <- getSuperclass clazz
            case parent of
                (Just parent) -> do
                    parents <- findParentClasses parent
                    return $ parent : parents
                _ -> return []
        
        reflectType :: JObject -- of type java.lang.Class
                    -> Java (Maybe JavaType)
        reflectType javatype = do

            isActuallyPrimitive <- isPrimitive javatype
            isActuallyAnArray   <- isArray javatype
            
            case undefined of
                _ | isActuallyPrimitive -> do
                    stringRepresentation <- toString javatype
                    return $ case stringRepresentation of
                        "boolean" -> Just JBoolean 
                        "char"    -> Just JChar
                        "byte"    -> Just JByte
                        "short"   -> Just JShort
                        "int"     -> Just JInt
                        "long"    -> Just JLong
                        "float"   -> Just JFloat
                        "double"  -> Just JDouble
                        "void"    -> Nothing
                        x -> error ("can't be: " ++ show x)  -- these are all primitive types

                _ | isActuallyAnArray -> do
                    (Just componentType) <- getComponentType javatype
                    reflectType componentType >>= return . Just . JArr . fromJust

                _otherwiseItsAnObjectType -> do
                    (Just name) <- getName javatype

                    return $ Just $ JObj {
                        typeName = name
                    }

        reflectTypeParameter :: JObject -- of type java.lang.reflect.TypeVariable<?>
                             -> Java JavaTypeParam
        reflectTypeParameter typeParameter = do
            
            (Just name) <- getTypeVariableName typeParameter
            (Just bounds) <- getTypeVariableBounds typeParameter

            bounds' <- toList bounds
                >>= mapM (reflectGenericType . fromJust)

            return $ JavaTypeParam {
                paramName = (TyVar name),
                paramBounds = bounds'
            }

        reflectGenericType :: JObject -- of type java.lang.reflect.Type
                           -> Java JavaGenericType
        reflectGenericType genericType = do

            isGenericArrayType  <- genericType `isInstanceOf` cGenArrT
            isParameterizedType <- genericType `isInstanceOf` cParamT
            isTypeVariable      <- genericType `isInstanceOf` cTypeV
            isWildcardType      <- genericType `isInstanceOf` cWildT

            case undefined of
                _ | isGenericArrayType -> do
                    (Just basetype) <- getGenericComponentType genericType
                    reflectedType <- reflectGenericType basetype

                    return $ GenericArray {
                        jgtComponentType = reflectedType
                    }

                _ | isParameterizedType -> do
                    (Just typeargs) <- getActualTypeArguments genericType
                    (Just basetype) <- getRawType genericType
                    isClass <- basetype `isInstanceOf` clazz
                    basetypeName <- if isClass
                        then getName basetype >>= return . fromJust
                        else fail "Type parameterized on a type variable. That was not possible <= Java 8."

                    parameters <- toList typeargs >>= mapM (reflectGenericType . fromJust)

                    return $ Parameterized {
                        jgtBasetype = basetypeName,
                        jgtParameters = parameters
                    }

                _ | isTypeVariable -> do
                    (Just name) <- getTypeVariableName genericType
                    
                    return $ TypeVarReference {
                        jgtName = (TyVar name)
                    }

                _ | isWildcardType -> do
                    (Just lowerBounds) <- getLowerBounds genericType
                    (Just bounds) <- getUpperBounds genericType

                    lowerBounds' <- toList lowerBounds
                        >>= mapM (reflectGenericType . fromJust)
                    bounds' <- toList bounds
                        >>= mapM (reflectGenericType . fromJust)
                    
                    return $ Wildcard {
                        jgtBounds = bounds',
                        jgtLowerBounds = lowerBounds'
                    }

                _ -> getName genericType >>= return . NotSoGeneric . fromJust
                    

        reflectEnumConstant :: JObject -- of type java.lang.Enum
                            -> Java (Int32, String)
        reflectEnumConstant enumConstant = do
            (Just name) <- getEnumName enumConstant
            ordinal     <- getEnumOrdinal enumConstant
            return (ordinal, name)

        reflectConstructor :: JObject -- of type java.lang.reflect.Constructor
                           -> Java JavaConstructor
        reflectConstructor constructor = do

            parameters <- getParameters' constructor
                >>= maybe (return []) toList
                >>= mapM (fmap fromJust . reflectType . fromJust)

            genericParams <- getGenericParametersC constructor
                >>= maybe (return []) toList
                >>= mapM (reflectGenericType . fromJust)

            typeParams <- getTypeParametersC constructor
                >>= maybe (return []) toList
                >>= mapM (reflectTypeParameter . fromJust)

            return $ JavaConstructor {
                constructorParams = zip parameters genericParams,
                constructorTypeParams = typeParams,
                constructorExceptions = []
            }

        reflectField :: JObject -- of type java.lang.reflect.Field
                     -> Java JavaField
        reflectField field = do

            (Just name) <- getFieldName field
            modifiers <- getModifiersF field

            fieldType <- getFieldType field
                >>= definitely reflectType
                >>= definitely return

            genericType <- getGenericType field
                >>= definitely reflectGenericType

            isActuallyStatic <- isStatic modifiers
            isActuallyFinal  <- isStatic modifiers

            return $ JavaField {
                fieldName = name,
                fieldType = (fieldType, genericType),
                fieldFinal = isActuallyFinal,
                fieldStatic = isActuallyStatic
            }

        reflectMethod :: JObject -- of type java.lang.reflect.Method
                      -> Java JavaMethod
        reflectMethod method = do

            (Just name) <- getMethodName method
            modifiers <- getModifiersM method

            returnType <- getReturnType method
                >>= maybe (return Nothing) reflectType

            parameters <- getParameters method
                >>= maybe (return []) toList
                >>= mapM (fmap fromJust . reflectType . fromJust)

            genericParams <- getGenericParametersM method
                >>= maybe (return []) toList
                >>= mapM (reflectGenericType . fromJust)

            genericReturnType <- getGenericReturnType method
                >>= reflectGenericType . fromJust
                            
            typeParams <- getTypeParametersM method
                >>= maybe (return []) toList
                >>= mapM (reflectTypeParameter . fromJust)

            isActuallyStatic   <- isStatic modifiers
            isActuallyAbstract <- isAbstract modifiers
            isActuallyFinal    <- isFinal modifiers
            isActuallyNative   <- isNative modifiers
            isActuallySynchronized <- isSynchronized modifiers

            return $ JavaMethod {
                methodName = name,
                methodName' = "", -- filled in later on
                methodParams = zip parameters genericParams,
                methodReturnType = (returnType, genericReturnType),
                methodTypeParams = typeParams,
                methodExceptions = [],
                methodStatic = isActuallyStatic,
                methodAbstract = isActuallyAbstract,
                methodFinal = isActuallyFinal,
                methodNative = isActuallyNative,
                methodSynchronized = isActuallySynchronized
            }

        reflectClass :: String -> Java JavaClass
        reflectClass className = do
            
            (Just clazz) <- callMethod loadClass systemClassLoader className False
            modifiers <- getModifiersC clazz

            isActuallyAbstract <- isAbstract modifiers
            isActuallyFinal <- isFinal modifiers

            let name        = takeClassName   className
                packageName = takePackageName className
            
            parents <- asObject clazz
                >>= findParentClasses
                >>= mapM (fmap fromJust . getName)

            interfaces <- asObject clazz
                >>= getInterfaces
                >>= maybe (return []) toList
                >>= mapM (getName . maybe undefined id)
                >>= mapM (maybe undefined return)

            fields <- asObject clazz
                >>= getFields
                >>= maybe (return []) toList
                >>= mapM (maybe undefined reflectField)

            methods <- asObject clazz
                >>= getMethods
                >>= maybe (return []) toList
                >>= mapM (maybe undefined reflectMethod)

            constructors <- asObject clazz
                >>= getConstructors
                >>= maybe (return []) toList
                >>= mapM (maybe undefined reflectConstructor)

            parameters <- asObject clazz
                >>= getTypeParameters
                >>= maybe (return []) toList
                >>= mapM (maybe undefined reflectTypeParameter)

            enumConstants <- asObject clazz
                >>= getEnumConstants
                >>= maybe (return []) toList
                >>= mapM (maybe undefined reflectEnumConstant)

            isActuallyEnum  <- asObject clazz >>= isEnum
            isActuallyIface <- asObject clazz >>= isInterface
            isActuallyAnn   <- asObject clazz >>= isAnnotation

            return $ JavaClass {
                className = name,
                classPackage = packageName,
                classModName = "", -- is filled in later
                classFields = fields,
                classMethods = methods,
                classParents = parents,
                classIfaces = interfaces,
                classConstructors = constructors,
                classTypeParams = parameters,
                classEnum = isActuallyEnum,
                classEnumConstants = enumConstants,
                classIface = isActuallyIface,
                classAnnotation = isActuallyAnn,
                classAbstract = isActuallyAbstract,
                classFinal = isActuallyFinal
            }

    mapM reflectClass classes

