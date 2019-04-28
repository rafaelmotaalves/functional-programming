-- BASIC  HASKELL
fat :: Int -> Int
fat x 
 | x < 1 = 1
 | otherwise = x * fat(x - 1)

all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal x y z w = (x == y) && (y == z) && (z == w)

equalCount:: Int -> Int -> Int -> Int
equalCount x y z
 | (x == y) && (y == z) = 3
 | (x == y) || (y == z) || (x == z) = 2
 | otherwise = 0


sales :: Int -> Float
sales 0 = 10.0
sales 1 = 20.0
sales 2 = 20.0
sales 3 = 10.0
sales 4 = 50.0

-- Given a value s and a number n, return the number of weeks,
-- between week 0 and n that have the sales number equal s
countEqualSalesWeek :: Int -> Float -> Int
countEqualSalesWeek n s
 | (n < 0) = 0
 | (sales n == s) =  1 + countEqualSalesWeek (n - 1) s 
 | otherwise = countEqualSalesWeek (n - 1) s
