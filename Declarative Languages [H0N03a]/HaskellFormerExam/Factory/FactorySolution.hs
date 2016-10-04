module FactorySolution where
import Data.List (sort)

--opdracht 1
data Station a b = Machine [(a,Int)]  b | Combiner [Station a b] deriving Show
machine = Machine
combine = Combiner

-- opdracht 2
trivial :: (Bounded a,Enum a) => Station a a
trivial = Combiner $ map (\a -> Machine [(a,1)] a) [minBound..maxBound]

trivial' :: Station Bool Bool
trivial' = combine [machine [(True,1)] True, machine [(False,1)] False]

--opdracht 3
type Resources a = [(a,Int)]

startResources :: Resources a
startResources = []

amount :: Ord a => Resources a -> a -> Int
amount xs var  = head $ [b | (a,b) <- xs , a ==var ] ++ [0]

insert :: Ord a => Resources a -> a -> Resources a
insert xs var = (var,amount xs var + 1):[(a,b) | (a,b) <- xs , a /= var]

--opdracht 4
run :: Ord a => [(a,Int)] -> b -> Resources a -> [a] -> ([a],[b])
run reqs b state (x:xs) 		|sort state == sort reqs = let (as,bs) = run reqs b [] (x:xs) in (as,b:bs)
								|amount state x < amount reqs x = run reqs b (insert state x) xs
								|otherwise = let (as,bs) = run reqs b state xs in (x:as,bs)
run reqs b state [] |sort state == sort reqs = ([],[b])
					|otherwise = ([],[])

--opdracht 5
runStation :: Ord a => Station a b -> [a] -> ([a],[b])
runStation (Machine reqs b) stream = run reqs b startResources stream
runStation (Combiner (x:xs)) stream = (asr,bs1++bsr)
					where
						(as1,bs1) = runStation x stream
						(asr,bsr) = runStation (Combiner xs) as1
runStation (Combiner []) stream = (stream,[])
