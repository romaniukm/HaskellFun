import Data.Char (toLower)

quickSort :: (Ord a) => [a] -> [a]

-- Base case
quickSort [] = []

-- Recursive case
quickSort (x:xs) = (quickSort less) ++ (x : equal) ++ (quickSort more)
    where
        less = filter (< x) xs
        equal = filter (== x) xs
        more = filter (> x) xs

quickSort2 :: (Ord a) => (a -> a -> Ordering) -> [a] -> [a]
quickSort2 _ [] = []

quickSort2 cmp (x:xs) = (quickSort2 cmp less) ++ (x : equal) ++ (quickSort2 cmp more)
    where
        less = filter (\ y -> y `cmp` x == LT) xs
        equal = filter (\ y -> y `cmp` x == EQ) xs
        more = filter (\y -> y `cmp` x == GT) xs

insensitive s1 s2 = compare (map toLower s1) (map toLower s2)

for :: a -> (a -> Bool) -> (a -> a) -> (a -> IO ()) -> IO ()
for i p f job =
    if p i then do
        _ <- job i 
        for (f i) p f job
    else do 
        return ()

-- This is more or less the reference solution (couldn't figure this out first)
sequenceIO :: [IO a] -> IO [a]
sequenceIO [] = return []
sequenceIO (ac:acs) = do
     v <- ac
     vs <- sequenceIO acs
     return (v:vs)

mapIO :: (a -> IO b) -> [a] -> IO [b]
mapIO _ [] = return []
mapIO f (x:xs) = do
    v <- f x
    vs <- mapIO f xs
    return (v:vs)

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x, y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (x, y) = f x y

const' :: a -> b -> a
const' x _ = x
