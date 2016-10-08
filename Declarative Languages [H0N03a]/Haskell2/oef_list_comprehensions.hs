
map' :: (a -> b) -> [a] -> [b]
map' f as = [f a | a <- as]

filter' :: (a -> Bool) -> [a] -> [a]
filter' p as = [a | a <- as, p a]

concat' :: [[a]] -> [a]
concat' ass = [a | as <- ass, a <- as ]




bind :: [a] -> (a -> [b]) -> [b]
bind m f = concat (map f m)

lc1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
lc1 f p = map f . filter p

lc2 :: (a -> b -> c) -> [a] -> (a -> [b]) -> (b -> Bool) -> [c]
lc2 f as bf p = bind as $ \a ->
                bind (filter p (bf a)) $ \b -> [f a b]

lc3 :: (Int -> Int -> Int -> a) -> Int -> [a]
lc3 f n = bind (filter even [1..n]) $ \a ->
          bind [a..n] $ \b ->
          bind (filter (\c -> a ^ 2 + b ^ 2 == c ^ 2) [b..n]) $ \c ->
          [f a b c]
