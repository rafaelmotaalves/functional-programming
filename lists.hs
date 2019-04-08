double :: [Int] -> [Int]
double [] = []
double (x:xs) = 2 * x : double xs

member :: Int -> [Int] -> Bool
member x [] = False
member x (y:ys)
 | x == y = True
 | otherwise = member x ys
 
eDigito :: Char -> Bool
eDigito x = x >= '0' && x <= '9'

digits :: String -> String
digits [] = []
digits (x:xs)
 | eDigito x = x : digits xs
 | otherwise = digits xs

sumPairs :: [(Int, Int)] -> [Int]
sumPairs l = [ x + y | (x, y) <- l ]

maiorLista :: [Int] -> Int
maiorLista [] = minBound :: Int
maiorLista [x] = x
maiorLista (head:tail)
 | head > maiorLista tail = head
 | otherwise = maiorLista tail

take2 :: Int -> [t] -> [t]
take2 _ [] = []
take2 i (x:xs)
 | i >= length ((x:xs)) = (x:xs)
 | i == 0 = []
 | otherwise = x : take2 (i-1) (xs)

drop2 :: Int -> [t] -> [t]
drop2 _ [] = []
drop2 i (x:xs)
 | i >= length ((x:xs)) = []
 | i == 0 = x:xs
 | otherwise = drop2 (i-1) (xs)

takeWhile2 :: (t -> Bool) -> [t] -> [t]
takeWhile2 _ [] = []
takeWhile2 eval (x:xs)
 | (eval x) = x : takeWhile2 eval xs
 | otherwise = []
 
dropWhile2 :: (t -> Bool) -> [t] -> [t]
dropWhile2 _ [] = []
dropWhile2 eval (x:xs)
 | (eval x) = dropWhile2 eval xs
 | otherwise = (x:xs)