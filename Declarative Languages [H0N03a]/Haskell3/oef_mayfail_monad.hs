import Control.Applicative (Applicative(..))
import Control.Monad (liftM2, join, ap)

data MayFail e a = Error e | Result a
  deriving (Show)

safeDiv :: Int -> Int -> MayFail String Int
safeDiv a b
  | b == 0    = Error "Division by zero"
  | otherwise = Result (div a b)

data Exp = Lit Int | Add Exp Exp | Mul Exp Exp | Div Exp Exp
  deriving (Show)

instance Functor (MayFail e) where
  fmap f (Error  e) = Error e
  fmap f (Result a) = Result (f a)

instance Monad (MayFail e) where
  return = Result
  Error e  >>= f = Error e
  Result a >>= f = f a

-- Required for GHC version >= 7.10
instance Applicative (MayFail e) where
    pure = return
    (<*>) = ap

-- No syntactic sugar
eval :: Exp -> MayFail String Int
eval (Lit x)   = return x
eval (Add x y) = eval x >>= \xv ->
                 eval y >>= \yv ->
                 return (xv + yv)
eval (Mul x y) = eval x >>= \xv ->
                 eval y >>= \yv ->
                 return (xv * yv)
eval (Div x y) = eval x >>= \xv ->
                 eval y >>= \yv ->
                 safeDiv xv yv

-- using do notation
eval2 :: Exp -> MayFail String Int
eval2 (Lit x)   = return x
eval2 (Add x y) = do
  xv <- eval2 x
  yv <- eval2 y
  return (xv + yv)
eval2 (Mul x y) = do
  xv <- eval2 x
  yv <- eval2 y
  return (xv * yv)
eval2 (Div x y) = do
  xv <- eval2 x
  yv <- eval2 y
  safeDiv xv yv

-- Using liftM2
eval3 :: Exp -> MayFail String Int
eval3 (Lit x)   = return x
eval3 (Add x y) = liftM2 (+) (eval3 x) (eval3 y)
eval3 (Mul x y) = liftM2 (*) (eval3 x) (eval3 y)
eval3 (Div x y) = join $ liftM2 safeDiv (eval3 x) (eval3 y)
