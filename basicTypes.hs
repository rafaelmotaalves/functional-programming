addSpaces :: Int -> String
addSpaces x
 | x == 0 = ""
 | otherwise = " " ++ addSpaces (x - 1)

toTheRight :: Int -> String -> String
toTheRight i s = addSpaces (i) ++ s



sales :: Int -> Float
sales 0 = 10.0
sales 1 = 20.0
sales 2 = 20.0
sales 3 = 10.0
sales 4 = 50.0

header = "Semana Venda\n"

salesTable :: Int -> String
salesTable n = header  
    ++ weekTableRows n
    ++ totalTableRow n
    ++ meanTableRow n

totalSales :: Int -> Float
totalSales n
 | n < 0 = 0
 | otherwise = totalSales (n - 1) + sales n

meanSales :: Int -> Float
meanSales n = (totalSales (n)) / fromIntegral(n + 1)

weekTableRows :: Int -> String
weekTableRows n
 | n < 0 = ""
 | otherwise = weekTableRows(n - 1) ++ ( (show (n)) ++ " " ++ (show( sales (n)))) ++ "\n"

totalTableRow :: Int -> String
totalTableRow n = "Total " ++ show (totalSales (n)) ++ "\n"

meanTableRow :: Int -> String
meanTableRow n = "Media " ++ show (meanSales (n)) ++ "\n"


-- ################################################################################3

menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior x y z = (
    mini (mini(x)(y)) (z), 
    maxi (maxi(x)(y)) (z)
    )

ordenaTripla :: (Int, Int, Int) -> (Int, Int , Int)
ordenaTripla (x, y, z)
 | firstIsBiggerThanOthers (x) (y) (z) && (y >= z) = (z, y, x)
 | firstIsBiggerThanOthers (x) (y) (z) && (z > y) = (y, z, x)
 | firstIsBiggerThanOthers (y) (x) (z) && (x >= z) = (z, x, y)
 | firstIsBiggerThanOthers (y) (x) (z) && (z > x) = (x, z, y)
 | firstIsBiggerThanOthers (z) (x) (y) && (x >= y) = (y, x, z)
 | firstIsBiggerThanOthers (z) (x) (y) && (y > x) = (x, y, z)

firstIsBiggerThanOthers :: Int -> Int -> Int -> Bool
firstIsBiggerThanOthers x y z = x > y && x > z

-- Max between 2 ints
maxi :: Int -> Int -> Int
maxi x y
 | x > y = x
 | otherwise = y

-- Min between 2 ints
mini :: Int -> Int -> Int
mini x y
 | x < y = x
 | otherwise = y



type Ponto = (Float, Float)
type Reta = (Ponto, Ponto)

getX :: Ponto -> Float
getX point = fst (point)

getY :: Ponto -> Float
getY point = snd (point)

isVertical :: Reta -> Bool
isVertical x = getX (fst x) == getX(snd x)

-- y = ( (y2 - y1) * (x - x1) / (x2 - x1)) + y1
pontoY :: Float -> Reta -> Float
pontoY x ((x1, y1), (x2, y2)) = ((dify * (x - x1)) / difx) + y1
    where difx = x2 - x1
          dify = y2 - y1
        