f x = x + 3

square x = x ^ 2

squareOfF x = (square . f) x

fOfSquare x = (f . square ) x

revWords :: String -> String
revWords input = (unwords . reverse . words) input
