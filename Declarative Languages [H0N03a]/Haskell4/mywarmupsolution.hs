 {-# LANGUAGE FlexibleInstances,UndecidableInstances #-}
import Control.Applicative (Applicative(..))
import Control.Monad (liftM2, join, ap)

myList :: IO [Int]
myList = do n <- readLn
            m <- readLn
            return (myRepeat n m)

myRepeat :: Int -> Int -> [Int]
myRepeat 0 x = []
myRepeat n x = (x : myRepeat (n-1) x)

continuouslyRead :: IO ()
continuouslyRead = do
           n <- getLine
           if null n
            then return()
           else do
              putStrLn (lineReversed n)
              continuouslyRead

lineReversed :: String -> String
lineReversed = reverse 

-- ghci > index [ print " Hello World ", print " Hello Galaxy "] readLn

index :: [IO a] -> IO Int -> IO a
index listA numA = do 
    nr <- numA
    listA !! nr