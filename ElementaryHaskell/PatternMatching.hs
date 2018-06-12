data Foo = Bar | Baz Int

f :: Foo -> Int
f Bar = 1
f (Baz x) = x - 1

dropThree :: [a] -> [a]
dropThree (_:_:_:xs) = xs
dropThree _ = []

fstPlusSnd :: (Num a) => (a, a) -> a
fstPlusSnd (x, y) = x + y

norm3D :: (Floating a) => (a, a, a) -> a
norm3D (x, y, z) = sqrt(x^2 + y^2 + z^2)

scanrAs :: (a -> b -> b) -> b -> [a] -> [b]
scanrAs f acc [] = [acc]
scanrAs f acc (x:xs) = (f x prevHead) : prevAll
    where prevAll@(prevHead:prevTail) = scanrAs f acc xs

data Foo2 = Bar2 | Baz2 {bazNumber::Int, bazName::String}

h :: Foo2 -> Int
h Baz2 {bazName=name} = length name
h Bar2 {} = 0

x = Baz2 1 "Haskell"
y = Baz2 {bazName = "Curry", bazNumber = 2}

g :: Foo -> Bool
g Bar {} = True
g Baz {} = False
