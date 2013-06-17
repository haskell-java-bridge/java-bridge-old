{-# LANGUAGE Haskell2010
    , GeneralizedNewtypeDeriving
    , DeriveDataTypeable
 #-}
{-# OPTIONS
    -Wall
    -fno-warn-name-shadowing
 #-}

-- | INTERNAL module is used to infer Haskell types from Java types.
module Typomatic (
    runTypomatic,
    typomatic,
    ArgInfo (..),

    dataTName,
    dataCName,
    newtTName,
    newtCName,
    tyclTName
) where

import Language.Java.Reflect

import Control.Monad.State hiding (void)
import qualified Control.Monad.State as State
import Data.Functor.Identity
import Data.Functor ((<$>))

import Data.Generics

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.List as List

dataTName, dataCName, newtTName, newtCName, tyclTName :: String -> String

dataTName = (++ "''")
dataCName = id -- (++ "")
newtTName = (++ "'")
newtCName = (++ "'")
tyclTName = id -- (++ "")

-- | This is the information which is ultimately
-- gathered by the use of this module.
data ArgInfo = ArgInfo {
    fSignature :: String,
    fArguments :: [TVar],
    fReturnType :: TVar,
    fArgNames :: [String],
    fJavaSignature :: String,
    fJavaReturnType :: String,
    fJniSignature :: String
  }

tr :: Eq a => a -> a -> [a] -> [a]
tr a b (x:xs)
    | a == x    = b : tr a b xs
    | otherwise = x : tr a b xs
tr _ _ [] = []

breakLast :: [a] -> ([a], a)
breakLast [a] = ([], a)
breakLast (a:as) =
    let (init', last') = breakLast as
    in  (a:init', last')
breakLast _ = error "Foreign.Java.Util.breakLast: empty list"


--------------------
-- Type variables --
--------------------

data TVar = TVar String | TVars [TVar]
    deriving (Eq, Ord, Show, Data, Typeable)

printTVar :: TVar -> String
printTVar var = case var of
    (TVar v) -> v
    (TVars vs) -> concat ["(", tail (concatMap ((' ':) . printTVar) vs), ")"]


---------------
-- Utilities --
---------------

printJniType :: JavaType -> String
printJniType t = case t of
    JBoolean -> "JNI.boolean"
    JChar    -> "JNI.char"
    JByte    -> "JNI.byte"
    JShort   -> "JNI.short"
    JInt     -> "JNI.int"
    JLong    -> "JNI.long"
    JFloat   -> "JNI.float"
    JDouble  -> "JNI.double"
    JObj n   -> "JNI.object \"" ++ n ++ "\""
    -- arrays are treated as objects:
    JArr c   -> "JNIS.object' \"[" ++ printJniRawType c ++ "\""


printJniRawType :: JavaType -> String
printJniRawType t = case t of
    JBoolean -> "Z"
    JChar    -> "C"
    JByte    -> "B"
    JShort   -> "S"
    JInt     -> "I"
    JLong    -> "J"
    JFloat   -> "F"
    JDouble  -> "D"
    JObj n   -> 'L' : tr '.' '/' n ++ ";"
    JArr c   -> '[' : printJniRawType c


printJniSignature :: JavaMethod -> String
printJniSignature method = show name ++ " JNI.::= " ++ args ++ ret
  where
    name = methodName method
    args = concatMap ((++ " --> ") . printJniType) (map fst $ methodParams method)
    ret  = maybe "JNI.void" printJniType (fst $ methodReturnType method)



typomatic :: JavaClass -> JavaMethod -> Typomatic ArgInfo
typomatic clazz method_ = do

    let className = classFullName clazz
        classParams = classTypeParams clazz

        -- sanitize tyVars tied to the method by distinguishing
        -- from tyVars tied to the class by adding an apostrophe
        -- to the name of the tyVar if it is tied to the method.
        methodTypeVars = map paramName (methodTypeParams method_)
        safe var@(TyVar str)
            | var `elem` methodTypeVars = TyVar (str ++ "'")
            | otherwise = var
        method = everywhere (mkT safe) method_

        -- the following three functions create the list of
        -- argument parameters, including @this@ (if the method
        -- is not static) and the return type. The return type
        -- is separated later on again.
        thisParam = if null classParams
            then NotSoGeneric className
            else Parameterized {
                    jgtBasetype = className,
                    jgtParameters = (map (TypeVarReference . paramName) classParams)
                  }
        params = (if methodStatic method then [] else [thisParam])
            ++ map snd (methodParams method)
            ++ [maybe (TypeVarReference (TyVar "()"))
                      (const $ snd $ methodReturnType method)
                      (fst $ methodReturnType method)]
        jtypes = (if methodStatic method then [] else [JObj className])
            ++ map fst (methodParams method)

        -- turns a JavaGenericType definition into type variables.
        -- The names are taken from the monad via 'newVar'.
        tvar param = case jgtType param of
            WildcardT -> do
                name <- newVar
                return $ TVar name
            ParameterizedT -> do
                name <- newVar
                params <- mapM tvar (jgtParameters param)
                return $ TVars $ TVar name : params
            GenericArrayT -> do
                name <- newVar
                return $ TVar name
            TypeVarReferenceT -> do
                let name = (tyVarName (jgtName param))
                return $ TVar name
            NotSoGenericT -> do
                name <- newVar
                return $ TVar name

        -- creates a haskell signature (-> String)
        signature typeVars returnVar = do
            -- retrieve the context and turn each variable into a String.
            contexts <- getContext >>= mapM (\(tvar, context) -> do
                return $ context ++ " " ++ printTVar tvar)

            let argTypes = concatMap ((++ " -> ") . printTVar) typeVars
                -- the final type is wrapped in the Java monad
                returnType = "JNI.Java " ++ printTVar returnVar
                -- finally assemble the conetext.
                context = if null contexts then "" else
                    "(" ++ concat (List.intersperse ", " contexts) ++ ") => "
            
            -- return the full signature, consisting of the context,
            -- the type of the arguments, and the return type.
            return $ concat [context, argTypes, returnType]

        -- create a java signature (-> String)
        javaSignature = do
            let name = methodName method
                args = map printJavaType $ map fst $ methodParams method
            return $ name ++ "(" ++ concat (List.intersperse ", " args) ++ ")"
            

    -- if this is not a static method the first argument
    -- is /this/. This merely pushed the name into the list
    -- of type variable names in the monad.
    when (not $ methodStatic method) (pushVar "this")

    -- get type variables for all arguments, including the
    -- return type (as the return type may be the same as
    -- one of the argument types).
    --
    -- Split the result into arguments and return var again,
    -- since the return variable will get special treatment
    -- henceforth.
    (typeVars, returnVar_) <- breakLast <$> mapM tvar params

    -- augment the return type variable, i.e. if it is not
    -- a type variable at all, replace the variable name by
    -- a constant reference to a specific type.
    --
    -- This is only the case with parameterized type variables
    -- and not-so-generic ones.
    returnVar <- case fst (methodReturnType method) of
        Just (JObj typeName) -> case jgtType (snd $ methodReturnType method) of
            ParameterizedT -> do
                clazz <- getClass typeName
                let (TVars (TVar _ : ts)) = returnVar_
                return $ TVars (TVar (newtTName (classModName clazz)) : ts)
            NotSoGenericT -> do
                clazz <- getClass typeName
                return $ TVar (newtTName (classModName clazz))
            _ -> return returnVar_
        _ -> return returnVar_

    -- Add contexts for all arguments and augment array types
    let makeContext typeVar jtype = case jtype of
            JObj name -> do
                clazz <- getClass name
                addContext typeVar $ tyclTName $ classModName clazz
                return typeVar
            JArr componentType -> do
                addContext typeVar "JNIS.Array"
                return typeVar
            JBoolean -> do
                addContext typeVar "JNIS.JBoolean"
                return typeVar
            JChar -> do
                addContext typeVar "JNIS.JChar"
                return typeVar
            JByte -> do
                addContext typeVar "JNIS.JByte"
                return typeVar
            JShort -> do
                addContext typeVar "JNIS.JShort"
                return typeVar
            JInt -> do
                addContext typeVar "JNIS.JInt"
                return typeVar
            JLong -> do
                addContext typeVar "JNIS.JLong"
                return typeVar
            JFloat -> do
                addContext typeVar "JNIS.JFloat"
                return typeVar
            JDouble -> do
                addContext typeVar "JNIS.JDouble"
                return typeVar

    -- Here makeContexts is applied (see above). In the same pass a new
    -- set of typeVars (typeVars') is generated, since makeContexts
    -- migth further investigate array and create type variables for
    -- their component types.
    typeVars' <- mapM (uncurry makeContext) (zip typeVars jtypes)

    -- Create contexts for the ultimate return type.
    returnVar' <- case fst (methodReturnType method) of
            Nothing -> do
                let tvar = TVar "void"
                addContext tvar "JNIS.VoidResult"
                return tvar
            Just t -> case t of
                JObj _   -> do
                    let tvar = TVars [TVar "object", returnVar]
                    addContext tvar "JNIS.ObjectResult"
                    return tvar
                JArr _   -> do
                    let tvar = TVar "array"
                    addContext tvar "JNIS.ArrayResult"
                    return tvar
                JBoolean -> do
                    let tvar = TVar "boolean"
                    addContext tvar "JNIS.BooleanResult"
                    return tvar
                JChar    -> do
                    let tvar = TVar "char"
                    addContext tvar "JNIS.CharResult"
                    return tvar
                JByte    -> do
                    let tvar = TVar "byte"
                    addContext tvar "JNIS.ByteResult"
                    return tvar
                JShort   -> do
                    let tvar = TVar "short"
                    addContext tvar "JNIS.ShortResult"
                    return tvar
                JInt     -> do
                    let tvar = TVar "int"
                    addContext tvar "JNIS.IntResult"
                    return tvar
                JLong    -> do
                    let tvar = TVar "long"
                    addContext tvar "JNIS.LongResult"
                    return tvar
                JFloat   -> do
                    let tvar = TVar "float"
                    addContext tvar "JNIS.FloatResult"
                    return tvar
                JDouble  -> do
                    let tvar = TVar "double"
                    addContext tvar "JNIS.DoubleResult"
                    return tvar

    -- generate the Haskell signature (a String)
    sig <- signature typeVars' returnVar'

    -- generate the Java signature (a String).
    -- This is used for documentation purposed later on
    -- (i.e. inserted as haddock docstring).
    jsig <- javaSignature

    -- generate the names of the arguments
    let argNames = (if methodStatic method then id else ("this":) . init)
            $ zipWith (\_ i -> 'a' : show i) typeVars [(1 :: Integer)..]

    -- assemble and return all the calculated information
    return $ ArgInfo {
        fArguments  = typeVars,
        fReturnType = returnVar',
        fArgNames   = argNames,
        fSignature  = sig,
        fJavaSignature = jsig,
        fJavaReturnType = maybe "void" printJavaType (fst $ methodReturnType method),
        fJniSignature = printJniSignature method
      }

--------------------------------------------
-- The following are utilities for the monad
--------------------------------------------

-- | The state of the monad.
data TypomaticState = TypomaticState {
    tVars :: [String],
    tContext :: Set (TVar, String),
    tParams :: [String],
    tClasses :: Map String JavaClass
  }

-- | Retrieve the definition of a class.
--
-- The monad has an internal store of class names
-- and their definitions. See 'tClasses'.
getClass :: String -> Typomatic JavaClass
getClass name = do
    state <- State.get
    return ((tClasses state) Map.! name)

-- | Get the current context as a list.
getContext :: Typomatic [(TVar, String)]
getContext = State.get >>= return . Set.toList . tContext

-- | Add a context for a specific type variable.
addContext :: TVar -> String -> Typomatic ()
addContext tvar string = do
    state <- State.get
    State.put (state {tContext = ((tvar, string) `Set.insert` tContext state)})

-- | Introduce a new name.
--
-- This simply takes the next element in the infinite
-- 'tVars' list and stores the tail back in the monad.
newVar :: Typomatic String
newVar = do
    state <- State.get
    let (v:vs) = tVars state
    State.put (state { tVars = vs})
    return v

-- | Push a new name in the front of the available names.
pushVar :: String -> Typomatic ()
pushVar name = do
    state <- State.get
    State.put (state {tVars = (name : tVars state)})

-- | The monad.
newtype Typomatic a = Typomatic { _runTypomatic :: StateT TypomaticState Identity a }
    deriving (Monad, MonadState TypomaticState, Functor)

-- | Run a computation in the monad.
runTypomatic :: Map String JavaClass -> Typomatic a -> a
runTypomatic classes =
    let state = TypomaticState { -- initial state
        tVars = map (('v':) . show) [(1 :: Integer)..],
        tContext = Set.empty,
        tParams = [],
        tClasses = classes
    } in fst . runIdentity . flip runStateT state . _runTypomatic




