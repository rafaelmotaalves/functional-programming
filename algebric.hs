data Shape = Circle Float | Rectangle Float Float

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle h b) = h * b 


data Expr = Lit Int |
    Add Expr Expr | 
    Sub Expr Expr |
    Paren Expr

showExpr :: Expr -> String
showExpr (Lit n) = show n
showExpr (Add x y) = showExpr (x) ++ " + " ++ showExpr (y)
showExpr (Sub x y) = showExpr (x) ++ " - " ++ showExpr (y)
showExpr (Paren x) = "(" ++ showExpr(x) ++ ")"


data List t = Nil | Node t (List t)

toList :: List t -> [t]
toList Nil = []
toList (Node x y) = x : toList y 

fromList :: [t] -> List t
fromList [] = Nil
fromList (x:xs) = (Node (x) (fromList xs))

data Tree t = TreeNil | TreeNode t (Tree t) (Tree t)

depth :: Tree t -> Int
depth TreeNil = 0
depth (TreeNode _ l r) = 1 + max (depth l) (depth r)

collapse :: Tree t -> [t]
collapse TreeNil = []
collapse (TreeNode value left right) = value : (collapse (left) ++ collapse (right))

mapTree :: (t -> u) -> Tree t -> Tree u 
mapTree _ TreeNil = TreeNil
mapTree f (TreeNode value left right) = (TreeNode (f value) (mapTree (f) (left)) (mapTree (f) (right)))