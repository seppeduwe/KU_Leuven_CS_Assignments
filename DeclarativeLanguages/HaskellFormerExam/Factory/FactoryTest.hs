import Factory
import Control.Monad

import Prelude hiding (catch) -- Required for older versions of GHC
import Data.List (sort)
import Data.Monoid
import Control.Exception (catch, SomeException(..))


main :: IO ()
main = startTests
				<> test  "amount (insert startResources Wheel) Wheel" ( amount (insert startResources Wheel) Wheel == 1)
				<> test "amount (insert startResources Wheel) Paint" ( amount (insert startResources Wheel) Paint == 0)
				<> test "amount (flip insert Wheel . flip insert Wheel $ startResources ) Wheel" ( amount (flip insert Wheel . flip insert Wheel $ startResources ) Wheel == 2)
				<> test "run [(Wheel,4)] ExpensiveCar  startResources [Wheel,Wheel,Wheel]" ( run [(Wheel,4)] ExpensiveCar  startResources [Wheel,Wheel,Wheel] == ([],[]))
				<> test "run [(Wheel,4)] ExpensiveCar  startResources [Wheel,Wheel,Wheel,Wheel]" ( run [(Wheel,4)] ExpensiveCar  startResources [Wheel,Wheel,Wheel,Wheel] == ([],[ExpensiveCar]))
				<> test "run [(Wheel,4)] ExpensiveCar  startResources [IkeaBody]" ( run [(Wheel,4)] ExpensiveCar  startResources [IkeaBody] == ([IkeaBody],[]))
				<> test "run [(Wheel,2)] Bike startResources [Wheel,Wheel,Wheel,Wheel]" ( run [(Wheel,2)] Bike  startResources [Wheel,Wheel,Wheel,Wheel] == ([],[Bike,Bike]))
				<> test "runStation factory resources1" ( runStation factory resources1 == ([Paint,IkeaBody,Paint,IkeaBody],[ExpensiveCar,ExpensiveCar,ExpensiveCar,ExpensiveCar,ExpensiveCar,CheapCar,CheapCar]))
				<> test "runStation factory resources2" ( runStation factory resources2 == ([Paint,Paint,Wheel,Wheel,Wheel,Wheel,Wheel,Wheel,Wheel,Wheel,Wheel,Wheel,Wheel,Wheel,IkeaBody,IkeaBody,IkeaBody],[CheapCar]))
				<> test "runStation factory resources3" ( runStation factory resources3 == ([Paint],[ExpensiveCar]))
				<> test "sort.snd $ runStation trivial [True,True,False]" ( (sort.snd) (runStation trivial [True,True,False]) == [False,True,True])
				>>= endTests

data Resource  = Wheel | Paint | BMWBody | IkeaBody deriving (Ord,Show,Eq)
data Car  = ExpensiveCar | CheapCar deriving (Ord,Show,Eq)
data Bike = Bike deriving (Ord,Show,Eq)

expensiveCarStation :: Station Resource Car 
expensiveCarStation = machine [(Wheel,4),(Paint,2),(BMWBody,1)] ExpensiveCar

cheapCarStation :: Station Resource Car 
cheapCarStation = machine [(Wheel,4),(Paint,1),(IkeaBody,1)] CheapCar

factory :: Station Resource Car
factory = combine [expensiveCarStation,cheapCarStation]

resources1 :: [Resource]
resources1 = concat . replicate 5 $ replicate 6 Wheel ++ replicate 3 Paint ++ [BMWBody,IkeaBody]

resources2 :: [Resource]
resources2 = replicate 5 Paint ++ replicate 20 Wheel ++ replicate 5 IkeaBody

resources3 :: [Resource]
resources3 = replicate 6 Wheel ++ replicate 4 Paint ++ [BMWBody, IkeaBody]



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
