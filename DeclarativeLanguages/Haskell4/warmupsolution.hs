import Control.Monad 

prog1 :: IO ()
prog1 = do
    m <- readLn :: IO Int
    n <- readLn :: IO Int
    replicateM_ m (print n)

prog2 :: IO ()
prog2 = do
    x <- getLine
    case x of
        "" -> return ()
        _ -> do
            print (reverse x) 
            prog2

index :: [IO a] -> IO Int -> IO a
index a = join . liftM (a !!)

