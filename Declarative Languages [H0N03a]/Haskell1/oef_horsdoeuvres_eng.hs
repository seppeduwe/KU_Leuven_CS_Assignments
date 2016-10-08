myLast :: [Int] -> Int
myLast [x]    = x
myLast (_:xs) = myLast xs

-- myLast = last


myRepeat :: Int -> Int -> [Int]
myRepeat n x | n > 0      = x : myRepeat (n - 1) x
              | otherwise  = []

-- myRepeat = replicate


flatten :: [[Int]] -> [Int]
flatten []     = []
flatten (x:xs) = x ++ flatten xs

-- flatten = concat


range :: Int -> Int -> [Int]
range s e | s > e     = []
          | otherwise = s : range (s + 1) e

-- range s e = [s..e]

removeMultiples :: Int -> [Int] -> [Int]
removeMultiples _ [] = []
removeMultiples n (x:xs)
  | x `mod` n == 0 = rest
  | otherwise      = x : rest
  where rest = removeMultiples n xs

-- removeMultiples n = filter (\x -> x `mod` n /= 0)
