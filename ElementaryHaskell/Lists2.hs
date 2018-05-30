import Data.List

doubleList :: [Integer] -> [Integer]
doubleList [] = []
doubleList (n:ns) = (2 * n) : doubleList ns

multiplyList :: Integer -> [Integer] -> [Integer]
multiplyList _ [] = []
multiplyList m (n:ns) = (m * n) : multiplyList m ns

takeInt :: Integer -> [Integer] -> [Integer]
takeInt _ [] = []
takeInt 0 _ = []
takeInt m (n:ns) = n : takeInt (m - 1) ns

dropInt :: Integer -> [Integer] -> [Integer]
dropInt _ [] = []
dropInt 0 ns = ns
dropInt m (n:ns) = dropInt (m - 1) ns

sumInt :: [Integer] -> Integer
sumInt [] = 0
sumInt (n:ns) = n + sumInt ns

scanSum :: [Integer] -> [Integer]
scanSum ns = scanSumAux 0 ns

scanSumAux :: Integer -> [Integer] -> [Integer]
scanSumAux _ [] = []
scanSumAux m (n:ns) = 
    let nextm = m + n
    in  nextm : scanSumAux nextm ns

diffs :: [Integer] -> [Integer]
diffs [] = []
diffs (x:[]) = []
diffs (x:y:ys) = (y - x) : diffs (y:ys)

applyToIntegers :: (Integer -> Integer) -> [Integer] -> [Integer]
applyToIntegers _ [] = []
applyToIntegers f (n:ns) = (f n) : applyToIntegers f ns

multiplyListAlt :: Integer -> [Integer] -> [Integer]
multiplyListAlt m = applyToIntegers ((*) m)

mapAlt :: (a -> b) -> [a] -> [b]
mapAlt _ [] = []
mapAlt f (x:xs) = (f x) : mapAlt f xs

multiplyList3 :: Integer -> [Integer] -> [Integer]
multiplyList3 m = map((*) m)

heads :: [[a]] -> [a]
heads = map head

negateList :: [Int] -> [Int]
negateList = map ((*) (-1))

divisorsList :: [Int] -> [[Int]]
divisorsList = map divisors
    where divisors p = [f | f <- [1..p], p `mod` f == 0]

divisorsListNegated :: [Int] -> [[Int]]
divisorsListNegated xs = map negateList (divisorsList xs)

encodeRle :: Eq a => [a] -> [(Int, a)]
encodeRle xs = map encodeRun (group xs)
    where encodeRun (x:xs) = (length (x:xs), x)

decodeRle :: Eq a => [(Int, a)] -> [a]
decodeRle xs = concat (map decodeRun xs)
    where decodeRun (n, x) = replicate n x

lastAlt :: [a] -> a
lastAlt [] = error "Empty list"
lastAlt (x:[]) = x
lastAlt (x:xs) = lastAlt xs

initAlt :: [a] -> [a]
initAlt [] = error "Empty list"
initAlt (x:[]) = []
initAlt (x:xs) = x:initAlt(xs)
