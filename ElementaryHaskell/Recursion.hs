factorial 0 = 1
factorial n = n * factorial (n - 1)

doubleFactorial 0 = 1
doubleFactorial 1 = 1
doubleFactorial n = n * doubleFactorial (n - 2)

mult _ 0 = 0                        -- anything times 0 is 0
mult n m = (mult n (m - 1)) + n     -- recurse: multiply by one less, and add an extra copy

power _ 0 = 1
power x y = x * power x (y - 1)

plusOne x = x + 1

addition x 0 = x
addition x y = addition (plusOne x) (y - 1)

log2 1 = 0
log2 n = 1 + log2(div n 2)

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length xs

catCat :: [a] -> [a] -> [a]
catCat [] ys     = ys
catCat (x:xs) ys = x : (xs ++ ys)

myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate n x = x : myReplicate (n - 1) x

myExtract :: [a] -> Int -> a
myExtract [] _     = error "Attempting to extract element from empty list"
myExtract (x:xs) 0 = x
myExtract (x:xs) n = myExtract xs (n - 1)

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

lengthAlt :: [a] -> Int
lengthAlt xs = lengthWithAcc xs 0

lengthWithAcc :: [a] -> Int -> Int
lengthWithAcc [] acc   = acc
lengthWithAcc (x:xs) acc = lengthWithAcc xs (acc + 1)
