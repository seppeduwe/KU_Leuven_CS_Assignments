module MonadicCombinators where

import Control.Monad

sequence'   :: Monad m => [m a] -> m [a]
mapM'       :: Monad m => (a -> m b) -> [a] -> m [b]
zipWithM'   :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
replicateM' :: Monad m => Int -> m a -> m [a]

sequence' [] = return []
sequence' (m:ms) = do
    a  <- m
    as <- sequence' ms
    return (a : as)
-- OR:
--sequence'         = foldr (liftM2 (:)) $ return []
mapM' f as        = sequence (map f as)
zipWithM' f xs ys = sequence (zipWith f xs ys)
replicateM' n x   = sequence (replicate n x)
