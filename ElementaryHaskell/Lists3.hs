import Data.List

sumAlt :: [Integer] -> Integer
sumAlt [] = 0
sumAlt (x:xs) = x + sumAlt(xs)

productAlt :: [Integer] -> Integer
productAlt [] = 1
productAlt (x:xs) = x * productAlt(xs)

concatAlt :: [[a]] -> [a]
concatAlt [] = []
concatAlt (x:xs) = x ++ concatAlt(xs)

foldrAlt :: (a -> b -> b) -> b -> [a] -> b
foldrAlt f acc []     = acc
foldrAlt f acc (x:xs) = f x (foldrAlt f acc xs)

foldlAlt :: (a -> b -> a) -> a -> [b] -> a
foldlAlt f acc [] = acc
foldlAlt f acc (x:xs) = foldlAlt f (f acc x) xs

addStr :: String -> Float -> Float
addStr str x = read str + x

sumStr :: [String] -> Float
sumStr = foldr addStr 0.0

andList :: [Bool] -> Bool
andList [] = True
andList (x:xs) = x && andList(xs)

orList :: [Bool] -> Bool
orList [] = False
orList (x:xs) = x || orList(xs)

andListFold :: [Bool] -> Bool
andListFold = foldr (\ x y -> x && y) True

orListFold :: [Bool] -> Bool
orListFold = foldr (\ x y -> x || y) False

maximumList :: Ord a => [a] -> a
maximumList = foldr1 max

minimumList :: Ord a => [a] -> a
minimumList = foldr1 min

reverseAlt :: [a] -> [a]
reverseAlt = foldl' (\ xs x -> x:xs) []

scanrRecursive :: (a -> b -> b) -> b -> [a] -> [b]
scanrRecursive f acc [] = [acc]
scanrRecursive f acc (x:xs) = (f x (head accList)) : accList
    where accList = scanrRecursive f acc xs

scanrFold :: (a -> b -> b) -> b -> [a] -> [b]
scanrFold f acc xs = foldr g [acc] xs
    where g x' (acc:accList) = f x' acc : acc : accList

scanlRecursive :: (a -> b -> a) -> a -> [b] -> [a]
scanlRecursive f acc xs = acc : scanlRecursiveHelper f acc xs
    where
        scanlRecursiveHelper f acc [] = []
        scanlRecursiveHelper f acc (x:xs) = headAcc : scanlRecursiveHelper f headAcc xs
            where headAcc = f acc x

scanlFold :: (a -> b -> a) -> a -> [b] -> [a]
scanlFold f acc xs = foldl g [acc] xs
    where g accList xs = accList ++ [f (last accList) xs]

factList :: Integer -> [Integer]
factList n = scanl1 (*) [1..n]

isEven :: Int -> Bool
isEven n = (mod n 2 ) == 0

retainEven = filter isEven

retainEvenListComp es = [n | n <- es, isEven n]

retainLargeEvens :: [Int] -> [Int]
retainLargeEvens es = [n | n <- es, isEven n, n > 100]

evenMinusOne es = [n - 1 | n <- es, isEven n]

firstForEvenSeconds :: [(Int, Int)] -> [Int]
firstForEvenSeconds ps = [x | (x, y) <- ps, isEven y]

allPairs :: [(Int, Int)]
allPairs = [(x, y) | x <- [1..4], y <- [5..8]]

somePairs :: [(Int, Int)]
somePairs = [(x, y) | x <- [1..4], y <-[5..8], x + y > 8]

returnDivisible :: Int -> [Int] -> [Int]
returnDivisible n xs = [x | x <- xs, (mod x n) == 0]

choosingTails :: [[Int]] -> [[Int]]
choosingTails ls = [tail(xs) | xs <- ls, length xs > 0, head(xs) > 5]  -- the order of guards matters

filterListComp :: (a -> Bool) -> [a] -> [a]
filterListComp p xs = [x | x <- xs, p x]

mapListComp :: (a -> b) -> [a] -> [b]
mapListComp f xs = [f x | x <- xs]

doubleOfFirstForEvenSeconds :: [(Int, Int)]  -> [Int]
doubleOfFirstForEvenSeconds ps = map (\ (x, y) -> x * 2) (filter (\ (x, y) -> isEven y) ps)
