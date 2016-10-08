module MOPL where
--Exercise 1
data Statement
data Term

--Exercise 2
assign :: String -> Term -> Statement
assign = undefined

printTerm :: Term -> Statement
printTerm = undefined

intTerm :: Int -> Term
intTerm = undefined

varTerm :: String -> Term 
varTerm = undefined

plus :: Term -> Term -> Term
plus = undefined

times :: Term -> Term -> Term
times = undefined

minus :: Term -> Term -> Term
minus = undefined

--Exercise 3
type State = [(String,Int)]

valueOf :: State -> String -> Int
valueOf = undefined

insertS :: String -> Int -> State -> State
insertS = undefined 

--Exercise 4
evalTerm :: State -> Term -> Int 
evalTerm = undefined

--Exercise 5
execAssign :: String -> Term -> State -> State
execAssign = undefined 

--Exercise 6
type Program = [Statement]

execPure :: State -> Program -> State
execPure = undefined

--Exercise 7

execute :: Program -> IO ()
execute = undefined

