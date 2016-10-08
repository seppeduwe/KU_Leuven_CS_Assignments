module MOPLsol where 

--opdracht 1
data Statement = Assign String Term | Print Term
data Term = VT String | IT Int | BinT (Int -> Int -> Int) Term Term

instance Show Statement where
	show (Assign a b) = a ++ " = " ++ show b
	show (Print a) = "print( " ++ show a ++ " )"

instance Show Term where
	show (VT s) = s
	show (IT i) = show i
	show (BinT a t1 t2) = "BinOp( " ++ show t1 ++ " , " ++ show t2 ++ " )"

--opdracht 2
assign :: String -> Term -> Statement
assign = Assign

printTerm :: Term -> Statement
printTerm = Print

intTerm :: Int -> Term
intTerm = IT

varTerm :: String -> Term 
varTerm = VT

plus :: Term -> Term -> Term
plus = BinT (+)

times :: Term -> Term -> Term
times = BinT (*)

minus :: Term -> Term -> Term
minus = BinT (-)

--opdracht 3
type State = [(String,Int)]

valueOf :: State -> String -> Int
valueOf xs var  = head [b | (a,b) <- xs , a ==var ]

insertS :: String -> Int -> State -> State
insertS var val xs = (var,val):[(a,b) | (a,b) <- xs , a /= var]

--opdracht 4
evalTerm :: State -> Term -> Int 
evalTerm st (IT i) = i 
evalTerm st (VT s) = valueOf st s
evalTerm st (BinT o a b) = o (evalTerm st a) (evalTerm st b)

--opdracht 5
execAssign :: String -> Term -> State -> State
execAssign var t state = insert var (evalTerm state t) state

--opdracht 6
type Program = [Statement]

execPure :: State -> Program -> State
execPure = foldl go
	where
		go st Print{} = st
		go st (Assign a b) = execAssign a b st


--opdracht 7
executeStatement :: State -> Statement -> IO State
executeStatement s (Print x) = do
						putStrLn (show . evalTerm s $ x)
						return s
executeStatement s (Assign a b) = return $ execAssign a b s

execute :: Program -> IO ()
execute xs = go [] xs
	where
		go _ [] = return ()
		go state (x:xs) = do
					state' <- executeStatement state x
					go state' xs


