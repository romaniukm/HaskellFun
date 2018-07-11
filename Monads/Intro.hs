import Text.Read

interactiveDoubling = do
    putStrLn "Choose a number:"
    s <- getLine
    let mx = readMaybe s :: Maybe Double
    case mx of
        Just x -> putStrLn ("The double of your number is " ++ show (2 * x))
        Nothing -> do
            putStrLn "This is not a valid number. Retrying..."
            interactiveDoubling

interactiveSumming = do
    putStrLn "Choose first number:"
    s1 <- getLine
    putStrLn "Choose second number:"
    s2 <- getLine
    let mx1 = readMaybe s1 :: Maybe Double
        mx2 = readMaybe s2 :: Maybe Double
    
    case (mx1, mx2) of
        (Nothing, Nothing) -> do 
            putStrLn "Both numbers are invalid. Retrying..."
            interactiveSumming
        (Just x, Nothing) -> do
            putStrLn "Second number is invalid. Retrying..."
            interactiveSumming
        (Nothing, Just y) -> do
            putStrLn "First number is invalid. Retrying..."
            interactiveSumming
        (Just x, Just y) -> do
            putStrLn ("The sum of your numbers is: " ++ show(x + y))

interactiveSumming2 :: IO()
interactiveSumming2 = do
    putStrLn "Choose two numbers:"
    mx <- readMaybe <$> getLine  -- equivalently: fmap readMaybe getLine
    my <- readMaybe <$> getLine
    case (+) <$> mx <*> my :: Maybe Double of
        Just z -> putStrLn ("The sum of your numbers is " ++ show z)
        Nothing -> do
            putStrLn "Invalid Number. Retrying..."
            interactiveSumming

interactiveConcatenating :: IO()
interactiveConcatenating = do
    putStrLn "Choose two strings:"
    sz <- (++) <$> getLine <*> getLine
    putStrLn "Let's concatenate them:"
    putStrLn sz
