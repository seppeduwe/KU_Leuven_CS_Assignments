module Factory where
import Data.List (sort)

--Exercise 1
data Station a b

machine :: [(a, Int)] -> b -> Station a b
machine = undefined

combine :: [Station a b] -> Station a b
combine = undefined

-- Exercise 2
trivial :: (Bounded a,Enum a) => Station a a
trivial = undefined

--Exercise 3
data Resources a

startResources :: Resources a
startResources = undefined

amount :: Ord a => Resources a -> a -> Int
amount = undefined

insert :: Ord a => Resources a -> a -> Resources a
insert = undefined

--Exercise 4
run :: Ord a => [(a,Int)] -> b -> Resources a -> [a] -> ([a],[b])
run = undefined

--Exercise 5
runStation :: Ord a => Station a b -> [a] -> ([a],[b])
runStation = undefined
