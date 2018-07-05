doGuessing num = do {
    putStrLn "Enter your guess:";
    guess <- getLine;
    case compare (read guess) num of {
        LT -> do {
            putStrLn "Too Low!";
            doGuessing num
        };
        GT -> do {
            putStrLn "Too high!";
            doGuessing num
        };
        EQ -> do {
            putStrLn "You win!"
        }
    }
}