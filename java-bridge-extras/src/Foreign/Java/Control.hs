{-# LANGUAGE Haskell2010 #-}
{-# OPTIONS -Wall #-}

-- | Utilities for controlling actions inside the Java monad.
module Foreign.Java.Control where

import Prelude hiding (pred, until)

when :: Monad m => m Bool -> m () -> m ()
-- ^ Execute an action if the given predicate
-- evaluates to 'True'.
when pred action = pred >>= (\p -> if p then action else return ())

unless :: Monad m => m Bool -> m () -> m ()
-- ^ Execute an action if the given predicate
-- evaluates to 'False'.
unless pred action = pred >>= (\p -> if not p then action else return ())

whether :: Monad m => m Bool -> m a -> m a -> m a
-- ^ Execute either the first or the second action,
-- depending on whether the given predicate evaluates
-- to 'True' or 'False'.
whether pred actionIf actionElse = do
    cond <- pred
    if cond then actionIf else actionElse

while :: Monad m => m Bool -> m () -> m ()
-- ^ Run a computation as long as the given predicate
-- evaluates to 'True'.
while pred action = do
    cond <- pred
    if cond then action >> while pred action
            else return ()

for :: Monad m => a -> (a -> m Bool) -> (a -> m a) -> m a
-- ^ Reiterate a computation on a given value as long
-- as a condition is 'True'.
for i pred action = do
    continue <- pred i
    if continue then action i >>= (\i' -> for i' pred action)
                else return i

until :: Monad m => a -> (a -> m (Bool, a)) -> m a
-- ^ Reiterate a computation on a given value until
-- a condition is 'True'.
until i action = do
    (done, result) <- action i
    if done then return result
             else until result action


