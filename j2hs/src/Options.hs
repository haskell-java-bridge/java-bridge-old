{-# LANGUAGE Haskell2010
    , TemplateHaskell
 #-}

module Options where

import Data.NamedRecord
import Data.Word

-- The options as used in the application later on.
-- This is a named record, see Data.NamedRecord for
-- more information.
record' "Options"
    `has` "optShowHelp"         := False
    `has` "optShowVersion"      := False
    `has` "optPackages"         := ["java.lang"]
    `has` "optClasspath"        := ["."]
    `has` "optSearchDepth"      := (100 :: Word32)
    `has` "optVerbose"          := False
    `has` "optCompleteSE6"      := False
    `has` "optFiltered"         := False
    `has` "optOnlyReflect"      := False
    `has` "optTargetDirectory"  := "bindings"
    `has` "optProjectName"      := "java-bindings"
    `has` "optProjectVersion"   := "1.0"
    `has` "optCabalProject"     := True
    `has` "optSaveReflectDump"  := ""
    `has` "optLoadReflectDump"  := ""

