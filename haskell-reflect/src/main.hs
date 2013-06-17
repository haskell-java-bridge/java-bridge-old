{-# LANGUAGE Haskell2010
 #-}
{-# OPTIONS
    -Wall
 #-}

module Main where

import Prelude
import Language.Haskell.Reflect

import System.Environment


main :: IO ()
main = getArgs >>= reflectModules >>= print





