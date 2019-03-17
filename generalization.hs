
isCrecent :: (Int -> Int) -> Int -> Bool
isCrecent f x
 | (x == 0) = True
 | otherwise = (f (x) > f(x-1)) && isCrecent (f) (x-1)


square :: (Num t) => t -> t
square x = x * x

allSquare :: [Int] -> [Int]
allSquare x = map (square) (x)

sumSquares :: (Num t) =>  t -> t -> t
sumSquares x y = square x + square y

onlyBiggerThanZero :: (Integral t) => [t] -> [t]
onlyBiggerThanZero ls = filter (>0) ls


maior :: [Int] -> Int
maior ls = foldl1 (\acumulator elem -> max (acumulator) (elem)) ls

maiores :: [[Int]] -> [Int]
maiores ls = map (maior) ls