module AsciiBoxesTest (main) where

import AsciiBoxes

import Control.Exception
import Text.PrettyPrint
import Control.Applicative

-- Some tests
main :: IO ()
main = runTests
         -- [ makeTest <expression-to-compute-as-a-string>
         --            <haskell-expression-to-evaluate>
         --            <haskell-expected-expression>
         -- , makeTest <expression-to-compute-as-a-string>
         --            <haskell-expression-to-evaluate>
         --            <haskell-expected-expression>
         -- ... ]

         -- ~~~~~~~~~~~~~~~~~~~~~~~ RENDERBOX, SHOW AND BASIC BOXES ~~~~~~~~~~~~~~~~~~~~~~~
         [ -- Show an empty box
           makeTest "show emptyBox" (show emptyBox) ""
           -- Show a non-empty box
         , makeTest "show (constantBox (4,2) 'a')" (show (constantBox (4,2) 'a')) "aaaa\naaaa\n"
           -- The empty box is printed as the empty string
         , makeTest "renderBox emptyBox == \"\""  (renderBox emptyBox == "") True
           -- A simple constantBox example
         , makeTest "renderBox (constantBox (4,3) 'a')" (renderBox (constantBox (4,3) 'a')) "aaaa\naaaa\naaaa\n"
           -- Check the orientation of `renderBox
         , makeTest "renderBox (Box (8,3) (\\(x,y) -> if y>1 then 'a' else 'b'))" (renderBox (Box (8,3) (\(x,y) -> if y>1 then 'a' else 'b'))) "aaaaaaaa\nbbbbbbbb\nbbbbbbbb\n"
           -- Constructing an empty box with constantBox
         , makeTest "renderBox (constantBox (0,0) undefined)" (renderBox (constantBox (0,0) undefined)) ""

         -- ~~~~~~~~~~~~~~~~~~~~~~~ inArea ~~~~~~~~~~~~~~~~~~~~~~~
         , makeTest "inArea (0,3) (1,2) (3,4)" (inArea (0,3) (1,2) (3,4)) False
         , makeTest "inArea (1,2) (1,2) (3,4)" (inArea (1,2) (1,2) (3,4)) True
         , makeTest "inArea (2,3) (1,2) (3,4)" (inArea (2,3) (1,2) (3,4)) True
         , makeTest "inArea (4,3) (1,2) (3,4)" (inArea (4,3) (1,2) (3,4)) False
         , makeTest "inArea (0,3) (1,2) (3,4)" (inArea (0,3) (1,2) (3,4)) False
         , makeTest "inArea (2,6) (1,2) (3,4)" (inArea (2,6) (1,2) (3,4)) False
         , makeTest "inArea (0,3) (1,2) (3,4)" (inArea (0,3) (1,2) (3,4)) False


       -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       -- TODO:
       -- Change the rest of the tests (adjust to this format)
       -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

       -- -- ~~~~~~~~~~~~~~~~~~~~~~~ BESIDE ~~~~~~~~~~~~~~~~~~~~~~~
       -- -- Boxes of different height
       -- <> test "(renderBox (beside (constantBox (2,3) 'a') (constantBox (4,2) 'b')) == \"aa    \\naabbbb\\naabbbb\\n\")"
       --     (renderBox (beside (constantBox (2,3) 'a') (constantBox (4,2) 'b')) == "aa    \naabbbb\naabbbb\n")
       -- -- Boxes of same height
       -- <> test "(renderBox (beside (constantBox (2,3) 'a') (constantBox (1,3) 'b')) == \"aab\\naab\\naab\\n\")"
       --     (renderBox (beside (constantBox (2,3) 'a') (constantBox (1,3) 'b')) == "aab\naab\naab\n")
       -- -- emptyBox is the left identity
       -- <> test "(renderBox (emptyBox `beside` (constantBox (5,6) 'c')) == renderBox (constantBox (5,6) 'c'))"
       --     (renderBox (emptyBox `beside` (constantBox (5,6) 'c')) == renderBox (constantBox (5,6) 'c'))
       -- -- emptyBox is the right identity
       -- <> test "(renderBox ((constantBox (5,6) 'c') `beside` emptyBox) == renderBox (constantBox (5,6) 'c'))"
       --     (renderBox ((constantBox (5,6) 'c') `beside` emptyBox) == renderBox (constantBox (5,6) 'c'))

       -- -- ~~~~~~~~~~~~~~~~~~~~~~~ ABOVE ~~~~~~~~~~~~~~~~~~~~~~~
       -- -- Boxes of different width
       -- <> test "(renderBox (constantBox (2,3) 'a' `above` constantBox (3,2) 'b') == \"aa \\naa \\naa \\nbbb\\nbbb\\n\")"
       --     (renderBox (constantBox (2,3) 'a' `above` constantBox (3,2) 'b') == "aa \naa \naa \nbbb\nbbb\n")
       -- -- Boxes of the same width
       -- <> test "(renderBox (constantBox (3,2) 'a' `above` constantBox (3,1) 'b') == \"aaa\\naaa\\nbbb\\n\")"
       --     (renderBox (constantBox (3,2) 'a' `above` constantBox (3,1) 'b') == "aaa\naaa\nbbb\n")
       -- -- emptyBox is the left identity
       -- <> test "(renderBox (emptyBox `above` (constantBox (5,6) 'c')) == renderBox (constantBox (5,6) 'c'))"
       --     (renderBox (emptyBox `above` (constantBox (5,6) 'c')) == renderBox (constantBox (5,6) 'c'))
       -- -- emptyBox is the right identity
       -- <> test "(renderBox ((constantBox (5,6) 'c') `above` emptyBox) == renderBox (constantBox (5,6) 'c'))"
       --     (renderBox ((constantBox (5,6) 'c') `above` emptyBox) == renderBox (constantBox (5,6) 'c'))

       -- -- ~~~~~~~~~~~~~~~~~~~~~~~ WRAP BOX ~~~~~~~~~~~~~~~~~~~~~~~
       -- -- Wrap an empty box
       -- <> test "(renderBox (wrapBox '*' emptyBox) == \"**\\n**\\n\")"
       --   (renderBox (wrapBox '*' emptyBox) == "**\n**\n")
       -- -- Wrap a constant box
       -- <> test "(renderBox (wrapBox '#' (constantBox (3,2) 'a')) == \"#####\\n#aaa#\\n#aaa#\\n#####\\n\")"
       --   (renderBox (wrapBox '#' (constantBox (3,2) 'a')) == "#####\n#aaa#\n#aaa#\n#####\n")
       -- -- Wrap a box generated by besides
       -- <> test "(renderBox (wrapBox '.' (constantBox (1,2) 'a' `beside` constantBox (2,1) 'b')) == \".....\\n.a  .\\n.abb.\\n.....\\n\")"
       --     (renderBox (wrapBox '.' (constantBox (1,2) 'a' `beside` constantBox (2,1) 'b')) == ".....\n.a  .\n.abb.\n.....\n")
       -- -- Wrap a box generated by above
       -- <> test "(renderBox (wrapBox '.' (constantBox (1,2) 'a' `above` constantBox (2,1) 'b')) == \"....\\n.a .\\n.a .\\n.bb.\\n....\\n\")"
       --     (renderBox (wrapBox '.' (constantBox (1,2) 'a' `above` constantBox (2,1) 'b')) == "....\n.a .\n.a .\n.bb.\n....\n")

       -- -- ~~~~~~~~~~~~~~~~~~~~~~~ OVERLAY ~~~~~~~~~~~~~~~~~~~~~~~
       -- -- Overlay example 1
       -- <> test "(renderBox (constantBox (2,3) 'a' `overlay` constantBox (3,2) 'b') == \"aa \\naab\\naab\\n\")"
       --     (renderBox (constantBox (2,3) 'a' `overlay` constantBox (3,2) 'b') == "aa \naab\naab\n")
       -- -- Overlay example 2
       -- <> test "(renderBox (constantBox (3,2) 'b' `overlay` constantBox (2,3) 'a') == \"aa \\nbbb\\nbbb\\n\")"
       --     (renderBox (constantBox (3,2) 'b' `overlay` constantBox (2,3) 'a') == "aa \nbbb\nbbb\n")
       -- -- Overlaying a box with itself returns itself
       -- <> test "(renderBox (constantBox (2,3) 'a' `overlay` constantBox (2,3) 'a') == renderBox (constantBox (2,3) 'a'))"
       --     (renderBox (constantBox (2,3) 'a' `overlay` constantBox (2,3) 'a') == renderBox (constantBox (2,3) 'a'))
       -- -- emptyBox is the left identity
       -- <> test "(renderBox (emptyBox `overlay` constantBox (2,3) 'a') == renderBox (constantBox (2,3) 'a'))"
       --     (renderBox (emptyBox `overlay` constantBox (2,3) 'a') == renderBox (constantBox (2,3) 'a'))
       -- -- emptyBox is the right identity
       -- <> test "(renderBox (constantBox (2,3) 'a' `overlay` emptyBox) == renderBox (constantBox (2,3) 'a'))"
       --     (renderBox (constantBox (2,3) 'a' `overlay` emptyBox) == renderBox (constantBox (2,3) 'a'))

       -- -- ~~~~~~~~~~~~~~~~~~~~~~~ BESIDE MANY ~~~~~~~~~~~~~~~~~~~~~~~
       -- -- Empty list
       -- <> test "(renderBox (besideMany []) == \"\")"
       --     (renderBox (besideMany []) == "")
       -- -- Single element
       -- <> test "(renderBox (besideMany [constantBox (1,3) 'a']) == renderBox (constantBox (1,3) 'a'))"
       --     (renderBox (besideMany [constantBox (1,3) 'a']) == renderBox (constantBox (1,3) 'a'))
       -- -- Two elements
       -- <> test "(renderBox (besideMany [constantBox (1,3) 'a', constantBox (2,1) 'b']) == renderBox (constantBox (1,3) 'a' `beside` constantBox (2,1) 'b'))"
       --     (renderBox (besideMany [constantBox (1,3) 'a', constantBox (2,1) 'b']) == renderBox (constantBox (1,3) 'a' `beside` constantBox (2,1) 'b'))
       -- -- Three elements
       -- <> test "(renderBox (besideMany [constantBox (1,2) 'a', constantBox (2,1) 'b', constantBox (1,3) 'c']) == \"   c\\na  c\\nabbc\\n\")"
       --     (renderBox (besideMany [constantBox (1,2) 'a', constantBox (2,1) 'b', constantBox (1,3) 'c']) == "   c\na  c\nabbc\n")

       -- -- ~~~~~~~~~~~~~~~~~~~~~~~ BOXABLE ~~~~~~~~~~~~~~~~~~~~~~~
       -- -- Convert a single character
       -- <> test "(renderBox (toBox 'a') == \"a\\n\")"
       --     (renderBox (toBox 'a') == "a\n")
       -- -- Convert a string (list of characters)
       -- <> test "(renderBox (toBox \"hello\") == \"hello\\n\")"
       --     (renderBox (toBox "hello") == "hello\n")
       -- -- Convert a list of lists of characters
       -- <> test "(renderBox (toBox [\"hello\", \"world\"]) == \"helloworld\\n\")"
       --     (renderBox (toBox ["hello", "world"]) == "helloworld\n")

       -- -- ~~~~~~~~~~~~~~~~~~~~~~~ MAKEHISTOGRAM ~~~~~~~~~~~~~~~~~~~~~~~
       -- <> test "makeHistogram"
       --     (renderBox (makeHistogram [('a', 10), ('b', 20), ('c', 15)]) == "    ###    \n    ###    \n    ###    \n    ###    \n    ###    \n    ### ###\n    ### ###\n    ### ###\n    ### ###\n    ### ###\n### ### ###\n### ### ###\n### ### ###\n### ### ###\n### ### ###\n### ### ###\n### ### ###\n### ### ###\n### ### ###\n### ### ###\n(a) (b) (c)\n")
         ]

-- * Main Test Types
-- ----------------------------------------------------------------------------

-- | The status of a testcase.
data TestStatus = Passed | Failed | NotImplemented

-- | The results of running a test
data TestResult = TestResult { test_expression      :: String
                             , test_result_expected :: String
                             , test_result_actual   :: String
                             , test_status          :: TestStatus }

-- * Create and Run Tests
-- ----------------------------------------------------------------------------

-- | Run a list of tests and print the results to the stdout.
runTests :: [IO TestResult] -> IO ()
runTests test_results = do
  elements <- map pprTestResult <$> sequence test_results
  let document = hang (doubleQuotes (text "test-results") <+> colon)
                      4 (brackets (vcat $ punctuate comma elements))
  putStrLn (render (braces document))

-- | Create a new test. The arguments represent:
--   * rep: A string representation of the expression to be evaluated
--   * expression: The actual expression to evaluate
--   * expected: The expected value for expression
makeTest :: (Eq a, Show a) => String -> a -> a -> IO TestResult
makeTest rep expression expected = do
  mb_res <- evaluateTest expression
  return $ case mb_res of
    Just res
      | res == expected -> def { test_result_actual = show res
                               , test_status        = Passed }
      | otherwise       -> def { test_result_actual = show res
                               , test_status        = Failed }
    Nothing             -> def { test_result_actual = msg
                               , test_status        = NotImplemented }
  where
    -- TODO: Instead of this msg, change `evaluateTest` so that it returns
    -- the exception raised as a String
    msg = "crashed-at-runtime"
    def = TestResult { test_expression      = rep
                     , test_result_expected = show expected
                     , test_result_actual   = undefined
                     , test_status          = undefined }

-- | Evaluate an expression. Return Nothing if it fails (crashes) or (Just
-- result) otherwise.
evaluateTest :: a -> IO (Maybe a)
evaluateTest a = (a `seq` return (Just a)) `catch` \(SomeException _) -> return Nothing

-- * Printing the Results
-- ----------------------------------------------------------------------------

instance Show TestStatus where
  show Passed         = show "passed"
  show Failed         = show "failed"
  show NotImplemented = show "not-implemented"

-- | Pretty print a test result
pprTestResult :: TestResult -> Doc
pprTestResult result = braces $ vcat $ punctuate comma $ zipWith (<+>) names values
  where
    names  = map ((<+> colon) . doubleQuotes . text)
                 [ "expression"
                 , "expected-result"
                 , "actual-result"
                 , "status"]
    values = map text
                 [ show $ test_expression      result
                 , show $ test_result_expected result
                 , show $ test_result_actual   result
                 , show $ test_status   result ]

