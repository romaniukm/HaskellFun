fakeIf :: Bool -> a -> a -> a
fakeIf condition trueVal falseVal = 
    case condition of
        True  -> trueVal
        False -> falseVal

doGuessing num = do
    putStrLn "Enter your guess:"
    guess <- getLine
    case compare (read guess) num of
        LT -> do putStrLn "Too low!"
                 doGuessing num
        GT -> do putStrLn "Too high!"
                 doGuessing num
        EQ -> putStrLn "You win!"

greetKoen = do
    putStrLn "What's your name?"
    name <- getLine
    let greetStandard = do putStrLn "I think that Haskell is a great programming language!"
    case name of
        "Simon" -> greetStandard
        "John"  -> greetStandard
        "Phil"  -> greetStandard
        "Koen"  -> do putStrLn "Debugging Haskell is fun!"
        _       -> do putStrLn "I don't know who you are"

mainExercise =
    do x <- getX
       putStrLn x
    
getX = 
    do return "My Shangri-La"
       return "beneath"
       return "the summer moon"
       return "I will"
       return "return"
       return "again"
