{-# LANGUAGE Haskell2010 #-}
{-# OPTIONS
    -Wall
    -fno-warn-orphans
 #-}

-- |
-- Module       : Foreign.Java.Maybe
-- Copyright    : (c) Julian Fleischer 2013
-- License      : MIT (See LICENSE file in cabal package)
--
-- Maintainer   : julian.fleischer@fu-berlin.de
-- Stability    : provisional
-- Portability  : portable (Haskell2010)
--
-- Every java methods returns in priniciple either Nothing or
-- Just a value. This is quite cumbersome to work with. This module
-- contains utility functions for working with Maybe values in the
-- java monad.
--
-- This module offers the orphan instance (!)
--
-- > instance JavaObject a => JavaObject (Maybe a)
--
-- This instance allows you to apply 'toString' and the like
-- without unwrapping a 'Maybe' value.
--
-- Note that the 'asObject' function provided by this instance
-- is undefined (since @null@ is not an object). This is also the
-- reason why this instance is not included in "Foreign.Java" by
-- default. Invoking it will call @error "NullPointerException"@.
-- In other words: This fine module will bring back all the joy of
-- Java you might miss in Haskell :-)
--
-- 'toString' will return @null@ for @Nothing@.
--
-- 'hashCode' will return @0@ for @Nothing@.
--
-- 'classOf' will return the class for @java.lang.Void@ on @Nothing@,
-- as this is a class for which there gare no object instances. This is
-- only a stopgap and slightly incorrect, as null is not an object,
-- and does therefor not have a class.
module Foreign.Java.Maybe where


import Foreign.Java


instance JavaObject a => JavaObject (Maybe a) where

    toString m = case m of
        Just obj -> toString obj
        Nothing -> return "null"

    hashCode m = case m of
        Just obj -> hashCode obj
        Nothing -> return 0

    asObject m = case m of
        Just obj -> asObject obj
        Nothing -> error "NullPointerException"

    classOf m = case m of
        Just obj -> classOf obj
        Nothing -> getClass "java.lang.Void" >>= classOf


