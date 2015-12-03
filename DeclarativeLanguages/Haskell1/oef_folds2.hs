mySum :: [Int] -> Int
mySum []     = 0
mySum (x:xs) = x + mySum xs

myProduct :: [Int] -> Int
myProduct []     = 1
myProduct (x:xs) = x * myProduct xs


fold :: (Int -> Int -> Int) -> Int -> [Int] -> Int
-- polymorphic: fold :: (a -> a -> a) -> a -> [a] -> a
-- even more polymorphic: fold :: (a -> b -> b) -> b -> [a] -> b
fold _ n []     = n
fold f n (x:xs) = f x (fold f n xs)

-- fold  =  foldr


readInBase :: Int -> [Int] -> Int
readInBase b = foldl (\x y -> x*b+y) 0

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x y -> f x : y) []
