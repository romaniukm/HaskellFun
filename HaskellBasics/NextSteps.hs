mySignum x = 
    if x < 0
        then -1
        else if x > 0
            then 1
            else 0

mySignumG x
    | x < 0     = -1
    | x > 0     = 1
    | otherwise = 0

absolute x =
    if x < 0
        then -x
        else x

fst' :: (a, b) -> a
fst' (x, _) = x
