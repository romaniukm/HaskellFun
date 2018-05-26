import Data.Char (toUpper)

main = do
    putStrLn "Please enter your name:"
    name <- getLine
    putStrLn ("Hello, " ++ name ++ ", how are you?")

triangleCalculator = do
    putStrLn "The base?"
    strBase <- getLine
    let base = read strBase
    putStrLn "The height?"
    strHeight <- getLine
    let height = read strHeight
    putStrLn ("The area of that triangle is " ++ show (base * height / 2))

doGuessing num = do
    putStrLn "Enter your guess:"
    guess <- getLine
    if (read guess) < num
        then do
            putStrLn "Too low!"
            doGuessing num
        else if (read guess) > num
            then do 
                putStrLn "Too high!"
                doGuessing num
            else putStrLn "You Win!"

koenSimonJohnPhil = do
    putStrLn "Enter your name:"
    name <- getLine
    if (name == "Koen")
        then do
            putStrLn "Debugging Haskell is fun!"
        else if ((name == "Simon") || (name == "John") || (name == "Phil"))
            then do
                putStrLn "I think Haskell is a great programming language!"
            else do
                putStrLn "I don't know who you are."

loudGreeting = do
    putStrLn "Enter your name:"
    name <- getLine
    let loudName = makeLoud name
    putStrLn ("Hello " ++ loudName ++ "!")
    putStrLn ("Oh boy! Am I excited to meet you, " ++ loudName ++ "!")

makeLoud :: String -> String
makeLoud s = map toUpper s
