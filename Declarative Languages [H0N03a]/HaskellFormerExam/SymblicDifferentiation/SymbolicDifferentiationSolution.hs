{- | Symbolic differentiation exam question

     The purpose of this exercise is to implement symbolic differentation for
     a simplistic expression language consisting of constants, variables,
     addition, multiplication, exponentiation and natural logarithms.

     Author: Alexander Vandenbroucke
-}

module SymbolicDifferentiationSolution where
-- | Function type, represents functions f : R -> R (where R is the real
--   numbers), build from multiplication, addition, exponentiation with an
--   real constant and the natural logarithm.
--   DISCUSSION: if we don't provide this datatype, we can still test
--   evaluate, derivative, pretty and simplification independently, provided
--   an Eq instance is defined/derived.
data Function
  = Const Double
    -- ^ Constant
  | X
    -- ^ variable
  | Function :+: Function
    -- ^ addition 
  | Function :*: Function
    -- ^ multiplication
  | Function :^: Double
    -- ^ integer power
  | Ln Function
    -- ^ natural logarithm (really here so evaluate has another partial case)
  deriving (Show, Eq)

infixl 6 :+:
infixl 7 :*:
infixl 8 :^:

instance Num Function where
  fromInteger = Const . fromInteger
  (+) = (:+:)
  (*) = (:*:)
  abs = error "Function.Num.abs: undefined"
  signum = error "Function.Num.signum: undefined"
  negate f = Const (-1) * f

-- | Evaluate a Function.
--   @evaluate f x@ evaluates @f@ by substituting every 'X' with @x@, and
--   doing the obvious thing for 'Const', ':+:', ':*:', ':^:' and 'Ln'.
--   Use the maybe monad!
--   When the exponent < 0, then 0 :^: n is Nothing!
--   The natural logarithm ln(y) is only defined when y > 0.
evaluate :: Function -> Double -> Maybe Double
evaluate (Const c)   _ = return c
evaluate X           x = return x
evaluate (e1 :+: e2) x = do
  y1 <- evaluate e1 x
  y2 <- evaluate e2 x
  return (y1 + y2)
evaluate (e1 :*: e2) x = do
  y1 <- evaluate e1 x
  y2 <- evaluate e2 x
  return (y1 * y2)
evaluate (e :^: n) x = do
  y <- evaluate e x
  if y == 0 && n < 0 then
    Nothing
  else
    return (y ** n)
evaluate (Ln e)  x = do
  y <- evaluate e x
  if y <= 0 then Nothing else return (log y)

-- | Compute the derivative of the function
derivative :: Function -> Function
-- dc/dx = 0 forall c in R
derivative (Const _)   = Const 0
-- dx/dx = 1
derivative X           = Const 1
-- d(f + g)/dx = df/dx + dg/dx
derivative (f :+: g) = derivative f :+: derivative g
-- d(f * g)/dx = (df/dx * g) + (f * dg/dx)
derivative (f :*: g) = (derivative f :*: g) :+: (f :*: derivative g)
-- d(f^n)/dx = n * f^(n-1) * df/dx
derivative (f  :^: n)  = Const n :*: (f :^: (n-1)) :*: derivative f
-- d(ln f)/dx = 1/f * df/dx
derivative (Ln f) = f :^: (-1) :*: derivative f

pretty :: Function -> String
pretty f = pretty' 0 f where
  -- | @pretty' c f@ takes an integer @c@ that is the precedence
  --   of the operator of which @f@ occurs as an argument position (i.e. the
  --   context of @f@).
  --   This means that a function should only be enclosed in parentheses if
  --   it occurs in a context that has higher priority than itself.
  --   =======================
  --    operator    priority
  --   -----------------------
  --    Ln             4
  --    X,Const        3
  --    :^:            2
  --    :*:            1
  --    :+:            0
  --   =======================
pretty' :: Int -> Function -> String
pretty' prio (Const d)   = fillBrackets prio 3 (show d)
pretty' prio (X)         = fillBrackets prio 3 "x"
pretty' prio (f1 :+: f2) = fillBrackets prio 0 ((pretty' 0 f1) ++ " + " ++ (pretty' 0 f2))
pretty' prio (f1 :*: f2) = fillBrackets prio 1 ((pretty' 1 f1) ++ " * " ++ (pretty' 1 f2))
pretty' prio (f1 :^: f2) = fillBrackets prio 2 ((pretty' 2 f1) ++ "^" ++ (show f2))
pretty' prio (Ln f1)     = fillBrackets prio 4 ("ln"++(pretty' 4 f1))

fillBrackets :: Int -> Int -> String -> String
fillBrackets p1 p2 s | p1 > p2 = "("++s++")"
                     | otherwise = s

-- | Mini command-line application to show, evaluate and take the derivative.
--   If you did not complete 'pretty', use 'show' instead
--   If you did not complete 'derivative', re-evaluate the Function instead.
evaluateIO :: Function -> IO ()
evaluateIO f = do
  putStrLn "Please enter a value for x to evaluate the function f(x) on, where:"
  putStr   "  f(x) = " >> print (pretty f)
  putStr   "x = "
  x <- readLn
  putStr   "  f(x) = " >> print (evaluate f x)
  let dfdx = derivative f
  putStrLn "The derivative of this function is:"
  putStr   "  df/dx = " >> print (pretty dfdx)
  putStrLn "Enter a value for x to evaluate df/dx on:"
  putStr "x = "
  x <- readLn
  putStrLn $ "  df/dx | (x = " ++ show x ++ ") = " ++ show (evaluate dfdx x)
