unpair :: [(a, b)] -> ([a], [b])
unpair xs = (map fst xs, map snd xs)

{- OR:
unpair xs = ([a | (a, _) <- xs], [b | (_, b) <- xs])
-}

{- OR:
unpair [] = ([], [])
unpair ((a, b) : rest) = (a : as, b : bs)
  where (as, bs) = unpair rest
-}

{- OR:
unpair = foldr (\(x,y) (xs,ys) -> (x:xs,y:ys)) ([],[])
-}
