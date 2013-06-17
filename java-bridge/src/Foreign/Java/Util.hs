{-# LANGUAGE Haskell2010 #-}
{-# OPTIONS -Wall #-}

module Foreign.Java.Util (
        tr, ($>), (<$), breakLast, trace, debug
    ) where


import qualified Debug.Trace as Debug


tr :: Eq a => a -> a -> [a] -> [a]
tr a b (x:xs)
    | a == x    = b : tr a b xs
    | otherwise = x : tr a b xs
tr _ _ [] = []


($>) :: Functor f => f a -> (a -> b) -> f b
($>) = flip fmap


(<$) :: Functor f => (a -> b) -> f a -> f b
(<$) = fmap


breakLast :: [a] -> ([a], a)
breakLast [a] = ([], a)
breakLast (a:as) =
    let (init', last') = breakLast as
    in  (a:init', last')
breakLast _ = error "Foreign.Java.Util.breakLast: empty list"


trace :: Show a => a -> a
trace a = Debug.trace (show a) a

debug :: Show b => b -> a -> a
debug b a = Debug.trace (show b) a


