removeMultiples :: Int -> [Int] -> [Int]
removeMultiples n l = filter (\x -> mod x n /= 0) l
-- Notice the possibility to perform an eta reduction here:
-- removeMultiples n = filter (\x -> mod x n /= 0)

sieve :: Int -> [Int]
sieve n = sieve2 [2..n] n

-- WITHOUT SQUARE ROOT BOUND:
-- sieve2 :: [Int] -> Int -> [Int]
-- sieve2 []     _ = []
-- sieve2 (x:xs) n = x : sieve2 (removeMultiples x xs) n

-- WITH SQUARE ROOT BOUND:
sieve2 :: [Int] -> Int -> [Int]
sieve2 (x:xs) n | x <= floor_square n = x : sieve2 (removeMultiples x xs) n
                | otherwise           = x : xs

sqrtMono :: Double -> Double
sqrtMono = sqrt

i2d :: Int -> Double
i2d = fromIntegral

floorMono :: Double -> Int
floorMono = floor

floorSquare :: Int -> Int
floorSquare n = floorMono (sqrtMono (i2d n))
-- OR: floorSquare = floorMono . sqrtMono . i2d
