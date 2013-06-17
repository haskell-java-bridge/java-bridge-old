{-# LANGUAGE Haskell2010
    , GeneralizedNewtypeDeriving
    , DeriveDataTypeable
    , CPP
 #-}
{-# OPTIONS
    -Wall
    -fno-warn-missing-signatures
    -fno-warn-name-shadowing
 #-}

-- | INTERNAL module:
-- The Java Monad Transformer. Most of its API is re-exported by "Foreign.Java"
module Foreign.Java.JavaMonad where


import Control.Monad.State hiding (void)
import qualified Control.Monad.State as State

import Data.Int
import Data.Word

import qualified Foreign.Java.JNI.Safe   as JNI
import qualified Foreign.Java.JNI.Types  as Core

import Foreign.Java.JNI.Types (
    JObject (..),
    JThrowable (..)
  )

import Foreign hiding (void)
import Foreign.C.String

import Foreign.Java.Util

import Control.Concurrent
import Control.Exception
import Data.Typeable


io :: IO a -> Java a
-- ^ Short for 'liftIO' and restricted to the 'Java' monad.
io = liftIO


-- | An exception in either the Java Virtual Machine or during
-- instantiating the Virtual Machine.
data JavaException =
        -- | An exception that occurred during the initialization
        -- of the virtual machine. Thrown by 'runJava', 'runJava'',
        -- or 'initJava'.
        JvmException String [String]

        -- | An exception that occurred inside the virtual machine.
        -- Thrown by those functions ending with a capital @E@.
      | JavaException String JThrowable
    deriving Typeable

instance Show JavaException where
    show (JvmException jvmlibPath args) =
        "JvmException: jvmlibPath = " ++ jvmlibPath
            ++ ", arguments = " ++ show args
    show (JavaException strMessage _throwable) =
        "JavaException: " ++ strMessage

instance Exception JavaException


-- | A reference to an instance of a Java Virtual Machine.
newtype JVM = JVM (Ptr Core.JVM)
    deriving Show


-- | The State of a virtual machine, running in the Java
-- Monad (which is a State Monad wrapped around the IO
-- Monad with JVMState as additional State).
--
-- All the accessor functions are INTERNAL.
data JVMState = JVMState {

    -- | INTERNAL The actual pointer to the virtual machine.
    jvmPtr :: Ptr Core.JVM,

    -- | INTERNAL Whether this virtual machine instance should
    -- be talked to using safe or unsafe calls.
    --
    -- See also 'setSafe' and 'getSafe'.
    jvmSafe :: Bool,

    -- | INTERNAL The cached methodID of Object.toString
    jvmToString :: Maybe (JObject -> Java (Maybe String)),

    -- | INTERNAL The cached methodID of Object.hashCode
    jvmHashCode :: Maybe (JObject -> Java Int32),

    jvmGetC :: Maybe (Maybe JObject -> Int32 -> Java Word16),
    jvmGetB :: Maybe (Maybe JObject -> Int32 -> Java Int8),
    jvmGetS :: Maybe (Maybe JObject -> Int32 -> Java Int16),
    jvmGetI :: Maybe (Maybe JObject -> Int32 -> Java Int32),
    jvmGetJ :: Maybe (Maybe JObject -> Int32 -> Java Int64),
    jvmGetF :: Maybe (Maybe JObject -> Int32 -> Java Float),
    jvmGetD :: Maybe (Maybe JObject -> Int32 -> Java Double),
    jvmGetZ :: Maybe (Maybe JObject -> Int32 -> Java Bool),
    jvmGetL :: Maybe (Maybe JObject -> Int32 -> Java (Maybe JObject)),

    jvmSetC :: Maybe (Maybe JObject -> Int32 -> Word16 -> Java ()),
    jvmSetB :: Maybe (Maybe JObject -> Int32 -> Int8 -> Java ()),
    jvmSetS :: Maybe (Maybe JObject -> Int32 -> Int16 -> Java ()),
    jvmSetI :: Maybe (Maybe JObject -> Int32 -> Int32 -> Java ()),
    jvmSetJ :: Maybe (Maybe JObject -> Int32 -> Int64 -> Java ()),
    jvmSetF :: Maybe (Maybe JObject -> Int32 -> Float -> Java ()),
    jvmSetD :: Maybe (Maybe JObject -> Int32 -> Double -> Java ()),
    jvmSetZ :: Maybe (Maybe JObject -> Int32 -> Bool -> Java ()),
    jvmSetL :: Maybe (Maybe JObject -> Int32 -> (Maybe JObject) -> Java ())
  }

-- | Creates a JVMState and initializes it with sane default values.
-- A Pointer to the virtual machine is required in any case.
newJVMState vm = JVMState {

    jvmPtr = vm,
    jvmSafe = True,
    jvmToString = Nothing,
    jvmHashCode = Nothing,

    jvmGetC = Nothing, jvmSetC = Nothing,
    jvmGetB = Nothing, jvmSetB = Nothing,
    jvmGetS = Nothing, jvmSetS = Nothing,
    jvmGetI = Nothing, jvmSetI = Nothing,
    jvmGetJ = Nothing, jvmSetJ = Nothing,
    jvmGetF = Nothing, jvmSetF = Nothing,
    jvmGetD = Nothing, jvmSetD = Nothing,
    jvmGetZ = Nothing, jvmSetZ = Nothing,
    jvmGetL = Nothing, jvmSetL = Nothing
  }


-- | Every computation in the Java Virtual Machine happens inside the
-- Java monad. The Java monad is mightier than the IO monad, i.e.
-- IO operations can be performed in both the IO monad as well as in
-- the Java monad, but Java operations can be performed in the Java
-- monad only and not in the IO monad.
--
-- Use one of 'runJava' or 'runJava'' to perform operations in the
-- Java monad.
newtype Java a = Java { _runJava :: StateT JVMState IO a }
    deriving (Monad, MonadState JVMState, Functor, MonadIO)

-- | INTERNAL Retrieve the 'jvmPtr' from this Java Monads
-- State.
getVM :: Java (Ptr Core.JVM)
getVM   = State.get $> jvmPtr

-- | INTERNAL Retrieve 'jvmSafe' from this Java Monads Sate.
getSafe :: Java Bool
getSafe = State.get $> jvmSafe

-- | By default java methods are invoked via the FFI using
-- safe calls. Safe calls are slower than unsafe calls. This
-- function controls whether safe or unsafe calls are being
-- used to communicate with the JVM.
--
-- If your application does not invoke the JVM concurrently
-- it is mostly safe to use unsafe calls.
--
-- > runJava (setUnsafe True >> doSomething)
--
-- will perform 'doSomething' using unsafe calls.
setUnsafe mode = do
    state <- State.get
    State.put (state { jvmSafe = not mode })



newtype JavaThreadId a = JavaThreadId (MVar (Either SomeException a))

forkJava :: Java a -> Java (JavaThreadId a)
-- ^ A utility function for forking an OS thread which runs in the
-- Java Monad. It will return a 'JavaThreadId' which you can wait on
-- using 'waitJava'.
forkJava t = io $ do
    lock <- newEmptyMVar
    _ <- forkOS $ do
        result <- try $ runJava t
        putMVar lock result
    return $ JavaThreadId lock


waitJava :: JavaThreadId a -> Java (Either SomeException a)
-- ^ Wait for a Java Thread to exit. If the thread exits abnormally
-- (that is, if an exception occurred), this function will return
-- @Left SomeException@. Otherwise it will return the result of the
-- computation as @Right a@.
waitJava (JavaThreadId mvar) = io $ takeMVar mvar


runJava :: Java a -> IO a
-- ^ Run a computation with support by a Java Virtual Machine.
runJava = runJava' []


runJava' :: [String] -> Java a -> IO a
-- ^ Run a computation with support by a Java Virtual Machine,
-- initialized with the given parameters.
--
-- This function may be used only once. If you intend to call
-- it multiple times, you need to initialize the Java subsystem
-- once before. If you fail to do so, this function will tear
-- down the virtual machine once it is done.
--
-- By using 'initJava' the virtual machine will be alive during
-- the whole lifetime of your process and 'runJava'' will never
-- tear down the machine.
--
-- /NOTE: According to the Java Native Interface specification it may be possible to create multiple virtual machines within a single process. However, no implementation of the JNI seems to be capable of doing so./
--
-- This function can be used to set for example the classpath
-- of the virtual machine:
--
-- > runJava' ["-Djava.class.path=java-library-dir"] $ do
-- >     doSomething
--
-- /NOTE: java.class.path does support relative paths./
runJava' opts f = do

    str <- mapM newCString (augmentOpts opts)
    ptr <- newArray str
    vm  <- JNI.createVM' (fromIntegral $ length str) ptr

    mapM_ free str >> free ptr

    if vm == nullPtr then do
                            libjvmPath <- JNI.getLibjvmPath >>= peekCString
                            throw $ JvmException libjvmPath opts
                     else return ()
     
    (result, _) <- finally (runStateT (_runJava f) (newJVMState vm))
                           (JNI.destroyVM vm)

    return result

#ifdef FFIJNI_DEBUG
augmentOpts = ("-Xcheck:jni" :)
#else
augmentOpts = id
#endif

runJavaGui :: Java a -> IO ()
-- ^ Short hand for @runJavaGui' []@.
runJavaGui = runJavaGui' []

runJavaGui' :: [String] -> Java a -> IO ()
-- ^ Mac OS X needs some special treatment for initializing
-- graphical applications, namely a Cocoa Runloop needs to be present
-- on the main thread. Since the main thread is the application
-- that the JVM was invoked from this has two consequences:
-- (1) A runloop needs to be created on the main thread
-- manually and (2) the main thread is not usable for your application.
--
-- On Mac OS X this function will fork an os thread using 'forkJava'
-- and start the Cocoa main event loop. This means that this function
-- must be called on the main thread and that it will never terminate
-- (since the cocoa event queue will be running there forever).
--
-- Note that this implies that you link your application with
-- the threaded runtime (`-threaded` in GHC).
--
-- Typically your application should look like this:
--
-- > main = runJavaGui $ do
-- >     stuffYourApplicationDoes
--
-- On all other platforms this is exactly the same as 'runJava''
-- (minus the fact that it returns @()@).
#if defined(FFIJNI_MACOSX) && defined(FFIJNI_OSX_GUI)
runJavaGui' opts java = runJava' opts $ do
        _ <- forkJava java
        io JNI.runCocoaMain
#else
runJavaGui' opts javaGui = runJava' opts javaGui >> return ()
#endif

initJava :: [String] -> IO ()
-- ^ Initializes the Java Virtual Machine so that it can
-- be used by subsequent invocations of 'runJava'. Note that
-- once you start the virtual machine it will be runing throughout
-- the whole lifetime of the main thread of your application.
initJava opts = runJava' opts persistVM

persistVM :: Java ()
persistVM = do
    vm <- getVM
    liftIO $ JNI.persistVM vm
    return ()


