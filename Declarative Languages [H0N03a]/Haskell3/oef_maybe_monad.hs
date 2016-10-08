module Solution where


sumABC :: [(String, Int)] -> Maybe Int
sumABC vars = case lookup "A" vars of
                   Nothing -> Nothing
                   Just a  -> case lookup "B" vars of
                                  Nothing -> Nothing
                                  Just b  -> case lookup "C" vars of
                                                 Nothing -> Nothing
                                                 Just c  -> Just (a + b + c)

sumABCBind :: [(String, Int)] -> Maybe Int
sumABCBind vars = lookup "A" vars >>= \a ->
               lookup "B" vars >>= \b ->
               lookup "C" vars >>= \c ->
               return (a + b + c)

sumABCDo :: [(String, Int)] -> Maybe Int
sumABCDo vars = do
    a <- lookup "A" vars
    b <- lookup "B" vars
    c <- lookup "C" vars
    return (a + b + c)
