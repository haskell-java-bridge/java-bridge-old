{-# LANGUAGE Haskell2010
    , CPP #-}
{-# OPTIONS -Wall #-}

-- | This module contains information about the java bridge on your
-- system. For the low level interface use "Foreign.Java.JNI.Safe" or
-- "Foreign.Java.JNI.Unsafe", for the medium level interface use
-- "Foreign.Java".
--
-- For creating high level bindings between Haskell and Java use
-- "Foreign.Java.Bindings".
module Foreign.Java.JNI where

data JniFlag
    = ONLY_CORE     -- ^ The java bridge was compiled with only the
                    --   core modules (low level interface).
    | DEBUG         -- ^ The java bridge was compiled with debug
                    --   symbols.
    | OSX_GUI       -- ^ The java bridge was compiled with special
                    --   support for Cocoa on Mac OS X.
    | OSX_FRAMEWORK -- ^ The java bridge was linked with the Java
                    --   framework on OS X. Otherwise @libjvm@ is
                    --   loaded dynamically.
   deriving (Show, Eq)

jniFlags :: [JniFlag]
-- ^ Returns a list of flags which the java bridge was compiled with.
jniFlags =

#ifdef FFIJNI_ONLY_CORE
    ONLY_CORE :
#endif

#ifdef FFIJNI_DEBUG
    DEBUG :
#endif

#ifdef FFIJNI_OSX_GUI
    OSX_GUI :
#endif

#ifdef FFIJNI_OSX_FRAMEWORK
    OSX_FRAMEWORK :
#endif

    []

javaBridgeVersion :: String
-- ^ The version of the java bridge.
javaBridgeVersion = FFIJNI_BRIDGE_VERSION


