-- 1

fa :: [Int] -> [Int]
fa [] = []
fa [x] = []
fa (x:xs) = (if (x == (head xs)) then [x] else []) ++ fa xs

fb :: [Int] -> [Int]
fb arr = [ x | (x,y) <- zip (arr) (tail arr), x == y]


-- 2
filterInRange :: [Int] -> [Int]
filterInRange x = filter (\y -> y >= 0 && y < 100) (x)

evenList :: [Int] -> [Bool]
evenList x = map (even) (x)

andAll :: [Bool] -> Bool
andAll x = foldr (&&) True (x)

g :: [Int] -> Bool
g arr = andAll (evenList (filterInRange arr))

--- 3
type Fabricante = String
type Potencia = Float
data Lampada = Compacta String Float | Incandescente String Float

instance Show Lampada where
    show (Compacta fab pot) = "Compacta " ++ fab ++ " " ++ (show pot) ++ "W"
    show (Incandescente fab pot) = "Incandescente " ++ fab ++ " " ++ (show pot) ++ "W" 

instance Eq Lampada where
    (==) (Compacta fab pot) (Compacta fab2 pot2) = (fab == fab2) && (pot == pot2)
    (==) (Incandescente fab pot) (Incandescente fab2 pot2) = (fab == fab2) && (pot == pot2)
    (==) (Compacta _ _ ) (Incandescente _ _) = False
    (==) (Incandescente _ _ ) (Compacta _ _) = False