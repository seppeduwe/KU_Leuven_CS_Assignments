import Control.Applicative (Applicative(..))
import Control.Monad (join, liftM2, ap)

data Exp = Lit Int | Add Exp Exp | Mul Exp Exp | Div Exp Exp
  deriving (Show)

data Log e a = Error e | Result a String
  deriving (Show)

instance Functor (Log e) where
  fmap f (Error e)      = Error e
  fmap f (Result a log) = Result (f a) log

-- Required for GHC version >= 7.10
instance Applicative (Log e) where
    pure = return
    (<*>) = ap

instance Monad (Log e) where
  return x = Result x ""
  Error e      >>= f = Error e
  Result a log >>= f =
    case f a of
      Error e        -> Error e
      Result a' log' -> Result a' (log ++ log')

traceLog :: String -> Log String ()
traceLog s = Result () s

divisionByZero :: Log String a
divisionByZero = Error "Division by zero"

evalLog :: Exp -> Log String Int
evalLog (Lit i) = do
  traceLog "Lit\n"
  return i
evalLog (Add x y) = do
  traceLog "Add\n"
  xv <- evalLog x
  yv <- evalLog y
  return (xv + yv)
evalLog (Mul x y) = do
  traceLog "Add\n"
  xv <- evalLog x
  yv <- evalLog y
  return (xv * yv)
evalLog (Div x y) = do
  traceLog "Div\n"
  xv <- evalLog x
  yv <- evalLog y
  if yv == 0
    then divisionByZero
    else return (div xv yv)

-- liftM2
evalLog2 :: Exp -> Log String Int
evalLog2 (Lit i)   = traceLog "Lit\n" >> return i
evalLog2 (Add x y) = traceLog "Add\n" >> liftM2 (+) (evalLog2 x) (evalLog2 y)
evalLog2 (Mul x y) = traceLog "Mul\n" >> liftM2 (*) (evalLog2 x) (evalLog2 y)
evalLog2 (Div x y) = traceLog "Div\n" >> join (liftM2 fun (evalLog2 x) (evalLog2 y))
  where
    fun x y | y == 0    = divisionByZero
            | otherwise = return (div x y)
