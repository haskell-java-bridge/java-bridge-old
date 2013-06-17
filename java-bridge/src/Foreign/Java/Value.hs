{-# LANGUAGE Haskell2010 #-}
{-# OPTIONS
    -Wall
    -fno-warn-name-shadowing
 #-}

-- |
-- Module       : Foreign.Java.Value
-- Copyright    : (c) Julian Fleischer 2013
-- License      : MIT (See LICENSE file in cabal package)
--
-- Maintainer   : julian.fleischer@fu-berlin.de
-- Stability    : provisional
-- Portability  : portable (Haskell2010)
--
-- The ternary value type, which can /treither/ hold a value,
-- nothing, or a special value describing an error condition.
module Foreign.Java.Value where

-- | A ternary value type to hold one of two possible value types
-- or none at all.
data Value e a
  = Value a -- ^ An actual value
  | NoValue -- ^ No value
  | Fail e  -- ^ A value describing the error

-- | fold on a 'Value', like 'either' for 'Either' or 'maybe' for 'Maybe'.
value :: b -- ^ default value if neither a value nor a fail value is given
      -> (e -> b) -- ^ function to handle a fail value
      -> (a -> b) -- ^ function to handle an actual value
      -> Value e a -- ^ the value
      -> b -- ^ the final return value
value noValue fail success value = case value of
    (Value a) -> success a
    (Fail e) -> fail e
    _ -> noValue

