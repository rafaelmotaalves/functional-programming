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

