module Main where

import MOPL

import Control.Exception (catch, SomeException(..))
import Control.Monad (liftM2)
import Data.Monoid
import Data.List (sort)

main = startTests
				<> test "valueOf [(a,5),(b,9),(c,10)] b" ( valueOf [("a",5),("b",9),("c",10)] "b" == 9)
				<> test "valueOf [(a,1),(a,2),(a,3)] a" ( valueOf [("a",1),("a",2),("a",3)] "a" == 1)
				<> test "insertS a 1 []" ( insertS "a" 1 [] == [("a",1)])
				<> test "insertS b 3 [(a,5),(b,9),(c,10)]" ( sort (insertS "b" 3 [("a",5),("b",9),("c",10)]) == [("a",5),("b",3),("c",10)])
				<> test "evalTerm [] (intTerm 5)" ( evalTerm [] (intTerm 5) == 5)
				<> test "evalTerm [(a,6)] (varTerm a)" ( evalTerm [("a",6)] (varTerm "a") == 6)
				<> test "evalTerm [(a,6)] (times (varTerm a) (intTerm 2))" ( evalTerm [("a",6)] (times (varTerm "a") (intTerm 2)) == 12)
				<> test "execAssign b (intTerm 6) []" ( execAssign "b" (intTerm 6) [] == [("b",6)])
				<> test "execAssign b (minus (varTerm b) (varTerm a) ) [(a,3),(b,8)]" ( sort (execAssign "b" (minus (varTerm "b") (varTerm "a") ) [("a",3),("b",8)]) == [("a",3),("b",5)])
				<> test "execPure [] program" ( sort (execPure [] program) == [("a",18),("b",10)])
     >>= endTests

program :: [Statement]
program = [
    assign	"a" (intTerm 8), --a = 8
    printTerm (plus (varTerm "a") (intTerm (-5))), --print(a-5)
    assign "b" (plus (varTerm "a") (intTerm 2)), --b = a+2
    assign "a" (plus (varTerm "a") (varTerm "b")), -- a = a+b
    printTerm (varTerm "a") -- print(a)
    ]



-- Mini testing framework
test :: String -> Bool -> IO Results
test msg b
  = do notImplemented <- isUndefined b
       case notImplemented of
         True      -> printResult yellow "function not implemented" >> return (Sum 1, Sum 0, Sum 0)
         False | b -> printResult green "passed" >> return (Sum 0, Sum 0, Sum 1)
         _         -> printResult red "failed" >> return (Sum 0, Sum 1, Sum 0)
  where printResult colorCode result
          = putStrLn $ "Test " ++ msg ++ " " ++ colorise colorCode result

type Results = (Sum Int, Sum Int, Sum Int) -- (Not implemented, failed, passed)

instance Monoid a => Monoid (IO a) where
  mempty = return mempty
  mappend = liftM2 mappend

startTests :: IO Results
startTests = putStrLn "Testing your solutions" >> return (Sum 0, Sum 0, Sum 0)

endTests :: Results -> IO ()
endTests (notImpl, failed, passed)
  = case (getSum notImpl, getSum failed, getSum passed) of
     (0, 0, _) -> putStrLn $ colorise green "All tests passed"
     (n, f, p) -> putStrLn $ unwords $
                  filter (not . null) [nNotImpl n, nFailed f, nPassed p]
  where nPassed 0 = ""
        nPassed p = colorise green $ show p ++ " tests passed"
        nFailed 0 = ""
        nFailed f = colorise red $ show f ++ " tests failed"
        nNotImpl 0 = ""
        nNotImpl n = colorise yellow $ show n ++ "x function not implemented"

isUndefined :: a -> IO Bool
isUndefined a = (a `seq` return False) `catch` \(SomeException _) -> return True

red, green, yellow :: Int
(red, green, yellow) = (31, 32, 33)

colorise :: Int -> String -> String
colorise colorCode s = "\ESC[0;" ++ show colorCode ++ "m" ++ s ++ "\ESC[0m"
