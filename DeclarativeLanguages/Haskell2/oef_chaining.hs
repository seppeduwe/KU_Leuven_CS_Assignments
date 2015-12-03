applyTimes :: Int -> (a -> a) -> a -> a
applyTimes 0 _ = id
applyTimes n a = a . applyTimes (n - 1) a

applyAll :: [a -> a] -> a -> a
applyAll = foldl (.) id

applyMultipleFuncs :: a -> [a->b] -> [b]
applyMultipleFuncs arg = map ($ arg)
