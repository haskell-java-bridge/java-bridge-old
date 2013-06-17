{-# LANGUAGE Haskell2010
    , TypeSynonymInstances
    , FlexibleInstances
 #-}

type Java = IO

class VoidResult a where
    toVoidResult :: () -> a

instance VoidResult () where
    toVoidResult = id

class Runnable a where
    run :: VoidResult void => a -> Java void

instance Runnable (Java ()) where
    run x = x


        



