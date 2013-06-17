{-# LANGUAGE Haskell2010
    , TypeSynonymInstances
    , FlexibleInstances
    , UndecidableInstances
    , OverlappingInstances
 #-}
{-# OPTIONS -Wall #-}

-- | 
-- Module       : Foreign.Java.IO
-- Copyright    : (c) Julian Fleischer 2013
-- License      : MIT (See LICENSE file in cabal package)
--
-- Maintainer   : julian.fleischer@fu-berlin.de
-- Stability    : provisional
-- Portability  : non-portable (UndecidableInstances, OverlappingInstances)
--
-- Utilities to ease IO operations in the Java monad.
module Foreign.Java.IO where

import Prelude hiding (print)
import Control.Monad.IO.Class
import System.IO hiding (print)


class PrintLn a where
    -- | Like @'putStrLn' . 'show'@, but with a specialized
    -- version for Strings plus it can be used within any
    -- 'MonadIO' monad (such as IO and Java).
    println :: MonadIO m => a -> m ()

    -- | Like @'putStr' . 'show'@, but with a specialized
    -- version for Strings plus it can be used within any
    -- 'MonadIO' monad (such as IO and Java).
    print :: MonadIO m => a -> m ()

instance  PrintLn String where
    println = liftIO . putStrLn
    print x = liftIO (putStr x >> hFlush stdout)

instance Show a => PrintLn a where
    println = println . show
    print = print . show

