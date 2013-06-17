{-# LANGUAGE Haskell2010 #-}
{-# OPTIONS -Wall #-}

module Language.Haskell.Reflect.Utils where

import Data.Map

import Language.Haskell.TH
import Language.Haskell.Reflect.Types

typeclassInfo :: Info -> Q HTypeclass
typeclassInfo _ = do
    return $ HTypeclass {
        typeclassName = "",
        typeclassInstances = []
      }

instanceInfo :: Info -> Q HInstance
instanceInfo _ = do
    return $ HInstance




