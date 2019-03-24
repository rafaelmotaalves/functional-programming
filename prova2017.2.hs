sublistas :: [a] -> [[a]]
sublistas [] = [[]]
sublistas (x:xs) = sublistas xs ++ map (\ys -> x:ys ) (sublistas xs) 


filtrarEInserir :: [[Int]] -> Int -> ([[Int]], Int)
filtrarEInserir [] _ = ([], 0)
filtrarEInserir a b = (x, y)
    where
        x = filterOddBiggerEven a
        y = maximum ([somaImpares(z) * b | z <- x])


filterOddBiggerEven :: [[Int]] -> [[Int]]
filterOddBiggerEven a = filter (\b -> somaImpares b > somaPares b) a

filterSoma :: (Int -> Bool) -> [Int] -> Int
filterSoma _ [] = 0
filterSoma f x
    | length (filtered) == 0 = 0
    | otherwise = foldr1 (+) filtered
    where filtered = [y | y <- x, f y]

somaImpares :: [Int] -> Int
somaImpares = filterSoma odd

somaPares :: [Int] -> Int
somaPares = filterSoma even

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f _ [x] = [f(x)]
altMap f g (x:y:z) = f(x) : g(y) : altMap (f) (g) (z)


listaPoli :: [(Integer,Integer,Integer)] -> [Integer -> Integer]
listaPoli coefs = map (\ (a, b, c) -> poli(a) (b) (c)) (coefs)

poli :: Integer -> Integer -> Integer -> Integer -> Integer
poli a b c = (\x ->  a * (x*x) + b * (x) + c )

appListPoli :: [Integer -> Integer] -> [Integer] -> [Integer]
appListPoli fs arr = [ f x | (f, x) <- zip (fs) (arr)]

data Mobile = Pendente Int | Barra Mobile Mobile

peso :: Mobile -> Int
peso (Pendente x) = x
peso (Barra x y) = peso x + peso y 


balanceado :: Mobile -> Bool
balanceado (Pendente _) = True
balanceado (Barra x y) = peso x == peso y 
    && balanceado x && balanceado y