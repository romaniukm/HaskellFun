absolute x
    | x < 0     = 0 - x
    | otherwise = x

numOfRealSolutions a b c
    | disc > 0  = 2
    | disc == 0 = 1
    | otherwise = 0
        where
        disc = b^2 - 4 * a * c
