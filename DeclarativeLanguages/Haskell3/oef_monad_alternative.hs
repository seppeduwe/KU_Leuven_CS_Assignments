import Control.Monad (join)

-- fmap, unit and join for the State monad (container approach)
newtype State s a = S { runS :: s -> (a, s) }

fmapState :: (a -> b) -> State s a -> State s b
fmapState f m = S (\s0 -> let (x, s1) = runS m s0
                          in (f x, s1))

unitState :: a -> State s a
unitState x = S (\s -> (x, s))

joinState :: State s (State s a) -> State s a
joinState mm = S (\s0 -> let (m, s1) = runS mm s0
                         in runS m  s1)


-- return and >>= for the list monad (side-effect approach)
returnList :: a -> [a]
returnList x = [x]

bindList :: [a] -> (a -> [b]) -> [b]
bindList xs f = concat $ map f xs

{- OR:
bindList = flip concatMap
-}


-- join, fmap and unit in terms of
--   return :: Monad m => a -> m a
--   (>>=)  :: Monad m => m a -> (a -> m b) -> m b
join' :: Monad m => m (m a) -> m a
join' m = m >>= id

fmap' :: Monad m => (a -> b) -> m a -> m b
fmap' f m = m >>= return . f

unit' :: Monad m => a -> m a
unit' = return

-- return and (>>=) in terms of
--   unit :: Monad m => a -> m a
--   join :: Monad m => m (m a) -> m a
--   fmap :: Monad m => (a -> b) -> m a -> m b
return' :: Monad m => a -> m a
return' = unit'

bind' :: (Functor m, Monad m) => m a -> (a -> m b) -> m b
bind' m f = join (fmap f m)
