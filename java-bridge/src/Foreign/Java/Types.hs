{-# LANGUAGE Haskell2010
    , MagicHash
    , FlexibleInstances
    , FlexibleContexts
    , MultiParamTypeClasses
    , FunctionalDependencies
 #-}
{-# OPTIONS
    -Wall
    -fno-warn-missing-signatures
    -fno-warn-name-shadowing
 #-}

-- | This module contains mostly boiler plate code that is needed
-- for method discovery.
module Foreign.Java.Types (

        -- These are human readable short hands for
        -- Z, X, C, ... below
        boolean, char, byte, short, int, long,
        float, double, string, object, array, void,

        -- These types are used to describe methods with the
        -- same vocabulary as the JNI does.
        --
        -- Notable additions: A, X, and Q are not defined in
        -- the JNI. A and X are used for convenience and
        -- resemble Arrays and Strings (S is already taken by
        -- Short, therefor X). Q is special and stands for
        -- objects in much the same way as L does, but Q will
        -- carry it's own low level signature (for example
        -- [Ljava.lang.String; ).
        Z (Z), C (C), B (B), S (S), I (I), J (J),
        D (D), F (F), L (L), V (V), A (A), X (X),

        -- P is used to apply the above descriptors.
        -- (-->) does the same thing, but is an infix operator.
        P (P),

        (-->), MethodDescriptor (..),

        -- The famous Q. See above.
        object', Q (Q),

        constructorSignature,
        methodSignature,

        Constructor,
        Method,

        Param (..),

        JArg (jarg)

    ) where

import Data.Int
import Data.Word

import Foreign.Java.JNI.Types hiding (JArg)
import qualified Foreign.Java.JNI.Types as Core
import Foreign.Java.Util


boolean = Z
char    = C
byte    = B
short   = S
int     = I
long    = J
float   = F
double  = D
object  = L
array   = A
string  = X
void    = V

object' = Q

data P a x = P a x deriving Show


class Param a where
    fieldSignature :: a -> String

-- These are the translations of descriptors to
-- JNI signatures.

instance Param Z where fieldSignature _ = "Z"
instance Param C where fieldSignature _ = "C"
instance Param B where fieldSignature _ = "B"
instance Param S where fieldSignature _ = "S"
instance Param I where fieldSignature _ = "I"
instance Param J where fieldSignature _ = "J"
instance Param F where fieldSignature _ = "F"
instance Param D where fieldSignature _ = "D"
instance Param L where fieldSignature (L x) = 'L' : tr '.' '/' x ++ ";"
instance Param X where fieldSignature _ = "Ljava/lang/String;"
instance Param x => Param (A x) where fieldSignature (A x) = '[' : fieldSignature x

instance Param Q where fieldSignature (Q s) = s


class JArg a b | a -> b where
    jarg :: a -> b -> Core.JArg

-- These are the known argument types.

instance JArg Z Bool    where jarg _ = BooleanA
instance JArg C Word16  where jarg _ = CharA
instance JArg B Int8    where jarg _ = ByteA
instance JArg S Int16   where jarg _ = ShortA
instance JArg I Int32   where jarg _ = IntA
instance JArg J Int64   where jarg _ = LongA
instance JArg F Float   where jarg _ = FloatA
instance JArg D Double  where jarg _ = DoubleA
instance JArg L (Maybe JObject) where jarg _ = ObjectA
instance JArg (A e) (Maybe (JArray e)) where jarg _ = ArrayA
instance JArg X String  where jarg _ = StringA

instance JArg Q (Maybe JObject) where jarg _ = ObjectA


-- (-->), (::=) are infix operators for convenience.

infixr 9 -->
infixl 8 ::=

(-->) :: a -> x -> P a x
a --> x = P a x

-- A MethodDescriptor is what is given to
-- 'getMethod', 'getConstructor', and friends.

data MethodDescriptor p = String ::= p
    deriving Show


---------------
-- Constructors
--
-- The signatures for looking up constructors are forged here.

constructorSignature :: Constructor p => p
constructorSignature = _constructorSignature "("

class Constructor p where
    _constructorSignature :: String -> p

instance Constructor String where
    _constructorSignature _ = "()V"

instance (Constructor (t -> r), Param a) => Constructor (P a t -> r) where
    _constructorSignature sig (P a t) = _constructorSignature (sig ++ fieldSignature a) t

instance Constructor (Z -> String) where
    _constructorSignature sig a = sig ++ fieldSignature a ++ ")V"
instance Constructor (C -> String) where
    _constructorSignature sig a = sig ++ fieldSignature a ++ ")V"
instance Constructor (B -> String) where
    _constructorSignature sig a = sig ++ fieldSignature a ++ ")V"
instance Constructor (S -> String) where
    _constructorSignature sig a = sig ++ fieldSignature a ++ ")V"
instance Constructor (I -> String) where
    _constructorSignature sig a = sig ++ fieldSignature a ++ ")V"
instance Constructor (J -> String) where
    _constructorSignature sig a = sig ++ fieldSignature a ++ ")V"
instance Constructor (F -> String) where
    _constructorSignature sig a = sig ++ fieldSignature a ++ ")V"
instance Constructor (D -> String) where
    _constructorSignature sig a = sig ++ fieldSignature a ++ ")V"
instance Constructor (L -> String) where
    _constructorSignature sig a = sig ++ fieldSignature a ++ ")V"
instance Param a => Constructor (A a -> String) where
    _constructorSignature sig a = sig ++ fieldSignature a ++ ")V"
instance Constructor (X -> String) where
    _constructorSignature sig X = sig ++ "Ljava/lang/String;" ++ ")V"

instance Constructor (Q -> String) where
    _constructorSignature sig a = sig ++ fieldSignature a ++ ")V"


----------
-- Methods
--
-- The signatures for looking up methods (both static and virtual,
-- that distinction has no meaning on this level) are forged here.

methodSignature :: Method p => p
methodSignature = _methodSignature "("

class Method p where
    _methodSignature :: String -> p

instance (Param a, Method r, Method (P b x -> r)) => Method (P a (P b x) -> r) where
    _methodSignature sig (P a x) = _methodSignature (sig ++ fieldSignature a) x

instance Param a => Method (P a Z -> String) where
    _methodSignature sig (P a _) = _methodSignature (sig ++ fieldSignature a ++ ")Z")
instance Param a => Method (P a C -> String) where
    _methodSignature sig (P a _) = _methodSignature (sig ++ fieldSignature a ++ ")C")
instance Param a => Method (P a B -> String) where
    _methodSignature sig (P a _) = _methodSignature (sig ++ fieldSignature a ++ ")B")
instance Param a => Method (P a S -> String) where
    _methodSignature sig (P a _) = _methodSignature (sig ++ fieldSignature a ++ ")S")
instance Param a => Method (P a I -> String) where
    _methodSignature sig (P a _) = _methodSignature (sig ++ fieldSignature a ++ ")I")
instance Param a => Method (P a J -> String) where
    _methodSignature sig (P a _) = _methodSignature (sig ++ fieldSignature a ++ ")J")
instance Param a => Method (P a F -> String) where
    _methodSignature sig (P a _) = _methodSignature (sig ++ fieldSignature a ++ ")F")
instance Param a => Method (P a D -> String) where
    _methodSignature sig (P a _) = _methodSignature (sig ++ fieldSignature a ++ ")D")
instance Param a => Method (P a L -> String) where
    _methodSignature sig (P a t) = _methodSignature (sig ++ fieldSignature a ++ ")" ++ fieldSignature t)
instance Param a => Method (P a V -> String) where
    _methodSignature sig (P a _) = _methodSignature (sig ++ fieldSignature a ++ ")V")
instance Param a => Method (P a X -> String) where
    _methodSignature sig (P a _) = _methodSignature (sig ++ fieldSignature a ++ ")Ljava/lang/String;")
instance (Param a, Param e) => Method (P a (A e) -> String) where
    _methodSignature sig (P a t) = _methodSignature (sig ++ fieldSignature a ++ ")" ++ fieldSignature t)

instance Param a => Method (P a Q -> String) where
    _methodSignature sig (P a t) = _methodSignature (sig ++ fieldSignature a ++ ")" ++ fieldSignature t)


instance Method (Z -> String) where
    _methodSignature sig _ = sig ++ ")Z"
instance Method (C -> String) where
    _methodSignature sig _ = sig ++ ")C"
instance Method (B -> String) where
    _methodSignature sig _ = sig ++ ")B"
instance Method (S -> String) where
    _methodSignature sig _ = sig ++ ")S"
instance Method (I -> String) where
    _methodSignature sig _ = sig ++ ")I"
instance Method (J -> String) where
    _methodSignature sig _ = sig ++ ")J"
instance Method (F -> String) where
    _methodSignature sig _ = sig ++ ")F"
instance Method (D -> String) where
    _methodSignature sig _ = sig ++ ")D"
instance Method (L -> String) where
    _methodSignature sig s = sig ++ ")" ++ fieldSignature s
instance Method (V -> String) where
    _methodSignature sig _ = sig ++ ")V"
instance Method (X -> String) where
    _methodSignature sig _ = sig ++ ")Ljava/lang/String;"
instance (Param e, Method (e -> String)) => Method (A e -> String) where
    _methodSignature sig s = sig ++ ")" ++ fieldSignature s

instance Method (Q -> String) where
    _methodSignature sig s = sig ++ ")" ++ fieldSignature s


instance Method String where
    _methodSignature sig = sig


