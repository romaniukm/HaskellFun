themselvesTimes :: [Int] -> [Int]
themselvesTimes xs = case xs of
    [] -> []
    x:ys -> (replicate x x) ++ (themselvesTimes ys)

(<*>) :: [(a -> b)] -> [a] -> [b]
fs <*> xs = [f x | f <- fs, x <- xs]
