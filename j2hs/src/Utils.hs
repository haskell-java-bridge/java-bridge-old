{-# LANGUAGE Haskell2010 #-}
{-# OPTIONS
    -Wall
    -fno-warn-missing-signatures
 #-}

module Utils where

import Data.Strings

import Data.List (foldl')

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Bimap as Bimap


exhaustively :: Eq a => (a -> a) -> a -> a
exhaustively = exhaustivelyBy (==)

exhaustivelyBy :: (a -> a -> Bool) -> (a -> a) -> a -> a
exhaustivelyBy predicate func dat = case predicate dat result of
    True -> result
    False -> exhaustivelyBy predicate func result
  where result = func dat

exhaustivelyByM :: Monad m => (a -> a -> m Bool) -> (a -> m a) -> a -> m a
exhaustivelyByM predicate func dat = do
    result <- func dat
    exhausted <- predicate dat result
    case exhausted of
        True -> return result
        False -> exhaustivelyByM predicate func result

intersections :: Ord a => [Set a] -> Set a
intersections sets = case sets of
    [] -> Set.empty
    [x] -> x
    (x:xs) -> foldl' Set.intersection x xs


-- Following 20 lines: Functions for managing a Bimap
-- such that it works with case insensitive string.
--
-- TODO: Factor this out `plus` use the case-insensitive package.
--       Those functions x... should be made into their own
--       datatype (Something like CISBimap or
--       BimapWithGenericComparisonFunction or ...)

newtype CaseInsensitiveString = CIS String

instance Eq CaseInsensitiveString where
    (CIS a) == (CIS b) = strToLower a == strToLower b

instance Ord CaseInsensitiveString where
    (CIS a) <= (CIS b) = strToLower a <= strToLower b

xToString (CIS s) = s

xEmpty      = Bimap.empty
xInsert k v = Bimap.insert (CIS k) (CIS v)
xMemberR k  = Bimap.memberR (CIS k)
xGet m      = xToString . (Bimap.!) m . CIS
xFromList   = Bimap.fromList . map (\(a, b) -> (CIS a, CIS b))
xToList     = map (\(a, b) -> (xToString a, xToString b)) . Bimap.toList
xKeysR      = map xToString . Bimap.keysR
xKeys       = map xToString . Bimap.keys
xSize       = Bimap.size

-- end todo


