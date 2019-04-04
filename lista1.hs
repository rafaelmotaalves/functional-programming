-- 1
-- map.(.) é uma expressão válida em haskell, ela indica a composição de map, com a função de composição
-- o tipo  da função é (b -> c) -> [a -> b] -> [a -> c]
-- (a -> b) -> [a] -> [b]
-- (b -> c) -> (a -> b) -> a -> c
-- (a -> b) ????/

-- 2
-- Sublistas

sublistas :: [a] -> [[a]]
sublistas [] = [[]]
sublistas (x:xs) = (map (\y -> x:y) (sub)) ++ sub
    where sub = sublistas xs

-- 3

poli :: Integer -> Integer -> Integer -> (Integer -> Integer)
poli a b c x =  a * (x*x) + b * x + c

listaPoli :: [(Integer, Integer, Integer)] -> [Integer -> Integer]
listaPoli x = map (\(a, b, c) -> poli (a) (b) (c)) x

appListaPoli :: [Integer -> Integer] -> [Integer] -> [Integer]
appListaPoli [] _ = []
appListaPoli _ [] = []
appListaPoli (f:fs) (x:xs) = f (x) : appListaPoli fs xs 

-- 4
isMatrix :: [[Integer]] -> Bool
isMatrix [] = True
isMatrix [x] = True
isMatrix (x:xs) = (length x == length (head xs)) && isMatrix (xs)


swap :: [[Integer]] -> Int -> Int -> [[Integer]]
swap m i j = bef ++ [l] ++ mid ++ [f] ++ af
    where f = m !! i
          l = m !! j
          bef = getBefore m i
          af = getAfter m j
          mid = getMiddle m i

getBefore :: [[Integer]] -> Int -> [[Integer]]
getBefore m i = take (i) m

getMiddle :: [[Integer]] -> Int -> [[Integer]]
getMiddle m i = init (drop (i + 1) m)

getAfter :: [[Integer]] -> Int -> [[Integer]]
getAfter m j = drop (j+1) m


-- 7
type Codigo = Int
data Voto = Presidente Codigo | Senador Codigo | Deputado Codigo | Branco deriving (Show)

type Urna = [ Voto ]
type Apuracao = [ ( Voto , Int ) ]

instance Eq Voto where
    (==) (Presidente x) (Presidente y) = x == y
    (==) (Senador x) (Senador y) = x == y
    (==) (Deputado x) (Deputado y) = x == y
    (==) _ _ = False

totalVotos :: Urna -> Voto -> Int
totalVotos arr candidate = foldl (\ac item -> if item == candidate then ac + 1 else ac) (0) arr

apurar :: Urna -> Apuracao
apurar urna = zip candidates votes
    where  
        candidates = uniqueCandidates urna
        votes = map (totalVotos urna) candidates 

uniqueCandidates :: Urna -> [Voto]
uniqueCandidates [] = []
uniqueCandidates (x:xs) = rest ++ if (not (elem (x) (rest))) then [x] else []
    where rest = uniqueCandidates xs