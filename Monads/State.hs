import Control.Applicative
import System.Random

rollDiceIO :: IO (Int, Int)
rollDiceIO = liftA2 (,) (randomRIO (1, 6)) (randomRIO (1, 6))

rollNDiceIO :: Int -> IO [Int]
rollNDiceIO n = sequence $ replicate n (randomRIO (1, 6))

rollDice :: StdGen -> ((Int, Int), StdGen)
rollDice g = ((n, m), g')
    where
        (n, g_) = randomR (1, 6) g
        (m, g') = randomR (1, 6) g_
