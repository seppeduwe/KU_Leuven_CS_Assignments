import Control.Applicative (Applicative(..))
import Control.Monad (ap)

data Exp = Lit Int | Add Exp Exp | Mul Exp Exp
  deriving (Show)

evalTrace :: Exp -> (Int, String)
evalTrace exp = evalTrace' (exp, "")

evalTrace' :: (Exp, String) -> (Int, String)
evalTrace' (exp, log) =
  case exp of
    Lit x   -> (x, log ++ "Lit\n")
    Add x y -> let newlog     = log ++ "Add\n"
                   (xv, logx) = evalTrace' (x, newlog)
                   (yv, logy) = evalTrace' (y, logx)
               in  (xv + yv, logy)
    Mul x y -> let newlog     = log ++ "Mul\n"
                   (xv, logx) = evalTrace' (x, newlog)
                   (yv, logy) = evalTrace' (y, logx)
               in  (xv * yv, logy)

data Writer a = Writer a String

instance Functor Writer where
  fmap f (Writer a log) = Writer (f a) log

-- Required for GHC version >= 7.10
instance Applicative Writer where
    pure = return
    (<*>) = ap

instance Monad Writer where
  return x = Writer x ""
  (Writer a log) >>= f =
    let Writer x log' = f a
    in  Writer x (log ++ log')

trace :: String -> Writer ()
trace s = Writer () s

evalTraceM :: Exp -> Writer Int
evalTraceM (Lit x) = do
  trace "Lit\n"
  return x
evalTraceM (Add x y) = do
  trace "Add\n"
  xv <- evalTraceM x
  yv <- evalTraceM y
  return (xv + yv)
evalTraceM (Mul x y) = do
  trace "Mul\n"
  xv <- evalTraceM x
  yv <- evalTraceM y
  return (xv * yv)
