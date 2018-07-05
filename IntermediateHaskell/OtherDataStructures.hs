data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f = g where
    g (Leaf x) = Leaf (f x)
    g (Branch left right) = Branch (g left) (g right)

treeFold :: (b -> b -> b) -> (a -> b) -> Tree a -> b
treeFold fbranch fleaf = g where
    g (Leaf x) = fleaf x
    g (Branch left right) = fbranch (g left) (g right)

tree1 :: Tree Integer
tree1 =
    Branch
        (Branch
            (Branch
                (Leaf 1)
                (Branch (Leaf 2) (Leaf 3)))
            (Branch
                (Leaf 4)
                (Branch (Leaf 5) (Leaf 6))))
        (Branch
            (Branch (Leaf 7) (Leaf 8))
            (Leaf 9))

doubleTree = treeMap (*2)  -- doubles each value in tree
sumTree = treeFold (+) id  -- sum of the leaf values in tree
fringeTree = treeFold (++) (: [])  -- list of the leaves of tree

data Weird a b = First a
               | Second b
               | Third [(a, b)]
               | Fourth (Weird a b)

weirdMap :: (a -> c) -> (b -> d) -> Weird a b -> Weird c d
weirdMap fa fb = g
    where
        g (First x) = First (fa x)
        g (Second y) = Second (fb y)
        g (Third xys) = Third [(fa x, fb y) | (x, y) <- xys]
        g (Fourth w) = Fourth (g w)

weirdFold :: (a -> c) -> (b -> c) -> ([(a, b)] -> c) -> (c -> c) -> Weird a b -> c
weirdFold f1 f2 f3 f4 = g
    where
        g (First x) = f1 x
        g (Second y) = f2 x
        g (Third xys) = f3 xys
        g (Fourth w) = f4 (g w)