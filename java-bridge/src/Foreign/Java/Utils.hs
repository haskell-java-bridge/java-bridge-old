{-# LANGUAGE Haskell2010 #-}
{-# OPTIONS -Wall #-}

-- |
-- Module       : Foreign.Java.Utils
-- Copyright    : (c) Julian Fleischer 2013
-- License      : MIT (See LICENSE file in cabal package)
--
-- Maintainer   : julian.fleischer@fu-berlin.de
-- Stability    : provisional
-- Portability  : portable (Haskell2010)
--
-- Utilities for dealing with Class, Package, and Module names in
-- the Java and Haskell languages.
module Foreign.Java.Utils where

import Data.Strings

javaKeywords, haskellKeywords :: [String]

javaKeywords = ["abstract",
                "assert",
                "boolean",
                "break",
                "byte",
                "case",
                "catch",
                "char",
                "class",
                "const",
                "continue",
                "default",
                "do",
                "double",
                "else",
                "enum",
                "extends",
                "final",
                "finally",
                "float",
                "for",
                "goto",
                "if",
                "implements",
                "import",
                "instanceof",
                "int",
                "interface",
                "long",
                "native",
                "new",
                "package",
                "private",
                "protected",
                "public",
                "return",
                "short",
                "static",
                "staticfp",
                "super",
                "switch",
                "synchronized",
                "this",
                "throw",
                "throws",
                "transient",
                "try",
                "void",
                "volatile",
                "while",

                -- strictly speaking these three are /reserved words/...
                "null", "true", "false"]

haskellKeywords = ["as",
                   "case",
                   "of",
                   "class",
                   "data",
                   "default",
                   "deriving",
                   "do",
                   "foreign",
                   "hiding",
                   "if",
                   "then",
                   "else",
                   "import",
                   "infixl",
                   "infixr",
                   "instance",
                   "let",
                   "in",
                   "module",
                   "newtype",
                   "qualified",
                   "type",
                   "where",

                   -- either extension words or other reserved words
                   "forall", -- several extensions
                   "mdo", -- ... -fglasgow-exts ... (deprecated?)
                   "rec", -- XDoRec
                   "proc", -- arrow notation
                   "family"] -- type families, to be sure

makeName :: Maybe String -- The name of the package
         -> String -- The name of the class
         -> String -- The simple name of the class, including the package.
-- ^ Build the name of a class based on maybe a package and a class name.
makeName pkg clazz = case pkg of
    (Just package) -> package ++ '.' : clazz
    _ -> clazz

makePackageModuleName :: String -- The name of the package
                      -> String -- The name of the corresponding Haskell module
-- ^ Translates a package name into a module name.
makePackageModuleName name
    | null name = name
    | otherwise = strJoin "." $ map (strCapitalize)
                              $ strSplitAll "." name

makeClassModuleName :: String -- The name of the class
                    -> String -- The name of the corresponding Haskell module
-- ^ Translates a class name into a module name.
makeClassModuleName name = case maybe "" makePackageModuleName (takePackageName name) of
    ""      -> classModuleName
    package -> package ++ '.' : classModuleName
  where classModuleName = strCapitalize
                        $ dropWhile (== '_') $ filter (/= '$')
                        $ takeClassName name

splitClassName :: String -> (String, String)
-- ^ Splits a class name into package name and class name.
--
-- If the name does not contain a package component, the first
-- string is empty.
--
-- See also 'joinClassName'.
splitClassName name = (maybe "" id $ takePackageName name, takeClassName name)

joinClassName :: (String, String) -> String
-- ^ Pendant to 'splitClassName'.
joinClassName (package, clazz) = case package of
    "" -> clazz
    _ -> package ++ '.' : clazz

takePackageName :: String -> Maybe String
-- ^ Retrieve the package name form a simple name of a class.
--
-- >>> takePackageName "java.lang.A$B"
-- Just "java.lang"
--
-- >>> takePackageName "Test"
-- Nothing
takePackageName fullName = if null name then Nothing else Just (init name)
  where name = (reverse . snd . break (== '.') . reverse) fullName

takeClassName :: String -> String
-- ^ Retrieve the class name form a simple name of a class.
-- This also contains the name of the enclosing class(es).
--
-- >>> takeClassName "java.lang.A$B"
-- "A$B"
--
-- >>> takeClassName "Thread$State"
-- "Thread$State"
takeClassName = reverse . fst . break (== '.') . reverse

takeBaseClassName :: String -> String
-- ^ Retrieve the class name form a simple name of a class.
-- This contains only the name of the class itself.
--
-- >>> takeBaseClassName "java.lang.A$B"
-- "B"
takeBaseClassName = reverse . fst . break (== '$') . reverse . takeClassName

takeEnclosingClasses :: String -> [String]
-- ^ Retrieve the names of the enclosing classes from a simple
-- class name.
--
-- >>> takeEnclosingClasses "java.lang.Map$EntrySet"
-- ["java.lang.Map"]
--
-- >>> takeEnclosingClasses "package.A$B$C"
-- ["package.A", "package.A$B"]
takeEnclosingClasses name = map (makeName (takePackageName name)) simpleNames
  where simpleNames = scanl1 (\x y-> x ++ "$" ++ y) $ init $ names $ takeClassName name
        names n = let (a, b) = break (== '$') n
                  in  a : if null b then [] else names (tail b)

