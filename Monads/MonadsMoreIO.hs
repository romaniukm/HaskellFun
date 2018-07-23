printList :: Show a => [a] -> IO ()
printList xs = sequence_ $ map print xs

generation = replicate 3

-- This is probably not the clever solution they're is asking for...
manyGenerations 0 initPop = initPop
manyGenerations n initPop = (manyGenerations (n - 1) initPop) >>= generation
