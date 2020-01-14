import Data.Char
-- HW5
sumNSquares :: Int -> Int
sumNSquares n = sum [x^2 | x <- [1..n]]
grid :: Int -> Int -> [(Int,Int)]
grid n m = [(x,y) | x <- [0..n], y <- [0..m]]
square :: Int -> [(Int,Int)]
square n = filter (\(x,y) -> x /= y) (grid n n)
replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]
divisors :: Int -> [Int]
divisors n = [x | x <- [1..n], n `mod` x == 0]
isPrime :: Int -> Bool
isPrime n = length (divisors n) == 2
matches :: Int -> [Int] -> [Int]
matches n xs = [x | x <- xs, n == x]
myElem :: Int -> [Int] -> Bool
myElem n xs = length (matches n xs) > 0
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x<-[1..n],y<-[1..n],z<-[1..n], 
            x^2+y^2==z^2]

-- HW6
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)
pow :: Int -> Int -> Int
pow n 0 = 1
pow n p = n * (pow n (p-1))
lengthR :: [a] -> Int
lengthR []     = 0
lengthR (x:xs) = 1 + lengthR xs
dropR :: Int -> [a] -> [a]
dropR 0 xs     = xs
dropR n (x:xs) = dropR (n-1) xs
andR :: [Bool] -> Bool
andR [] = True
andR (x:xs) = x && andR xs
concatR :: [[a]] -> [a]
concatR       [] = []
concatR (xs:xss) = xs ++ concatR xss
replicateR :: Int -> a -> [a]
replicateR 0 x = []
replicateR n x = x : replicateR (n-1) x
nthElem :: [a] -> Int -> a
nthElem (x:xs) n
    | n == 0    = x
    | otherwise = nthElem xs (n-1)
elemR :: Eq a => a -> [a] -> Bool
elemR t [] = False
elemR t (x:xs) = t == x || (elemR t xs)
elemNum t xs = length (matches t xs)
unzip' :: [(a,b)] -> ([a],[b])
unzip' [] = ([],[])
unzip' ((x,y):zs) = (x:a,y:b)
                    where (a,b) = unzip' zs
zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' [] _ _ = []
zip3' _ [] _ = []
zip3' _ _ [] = []
zip3' (x:xs) (y:ys) (z:zs) = (x,y,z):(zip3' xs ys zs)

-- HW7
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | x >  y    = y : merge (x:xs) ys
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
            where (ys,zs) = halve xs
halve :: [a] -> ([a],[a])
halve xs = splitAt (length xs `div` 2) xs
isPalin :: String -> Bool
isPalin s = sproc == reverse sproc
                where sproc = process s
process :: String -> String
process s = filter (\c -> not (elem c ",.;:'\"!? ")) (map toLower s)

-- HW8
length8 :: [a] -> Int
length8 xs = sum (map (\x -> 1) xs)
--map :: (a -> b) -> [a] -> [b]
--filter :: (a -> Bool) -> [a] -> [a]
--map f (map g xs) = map (f . g) xs
--filter p (filter q xs) = filter (\x->p x && q x) xs
all8 :: (a -> Bool) -> [a] -> Bool
all8 p = and . map p
any8 :: (a -> Bool) -> [a] -> Bool
any8 p = or . map p
--cont prob 2c hw8

-- HW 9
composeList :: [(a -> a)] -> (a -> a)
composeList = foldr (.) id
flip :: (a -> b -> c) -> (b -> a -> c)
flip = \f a b -> f b a
num6 :: Char -> Bool
num6 = \c -> not (elem c "\t\n")
total :: (Int -> Int) -> Int -> Int
total = \f n -> sum (map f [0..n])
num8a xs yss = map (\ys -> zip xs ys) yss
num8b yss xs = map (\ys -> zip ys xs) yss

-- HW 10
data Shape = Circle Float
           | Rectangle Float Float
           | Triangle Float Float Float
                deriving Show
instance Eq Shape where
    Circle r == Circle r' = if r<0 then r'<0 else r==r'
    Rectangle l w == Rectangle l' w' = l==l' && w==w'
    Triangle a b c == Triangle a' b' c' = a==a'&&b==b'&&c==c'
    _ == _ = False
regular :: Shape -> Bool
regular (Circle r) = True
regular (Rectangle l w) = l==w
regular (Triangle a b c) = a==b&&b==c
compare :: Ord a => a -> a -> Ordering
compare a b
    | a > b     = GT
    | b > a     = LT
    | otherwise = EQ
data Nat = Zero | Succ Nat
    deriving Show
add :: Nat -> Nat -> Nat
add (Zero) m   = m
add (Succ n) m = Succ (add n m)
mult :: Nat -> Nat -> Nat
mult (Succ Zero) n  = n
mult (Succ m) n     = add n (add m n)
mult _ _ = Zero
--instance Eq a => Eq (Maybe a) where
--  Nothing == Nothing = True
--  Just n == Just m = n==m
--  _ == _ = False
--instance Eq a => Eq [a] where
--  [] == [] = True
--  (x:xs) == (y:ys) = x==y && xs==ys
--  _ == _ = False

-- HW 11
data Tree a = Leaf a | Node (Tree a) (Tree a)
balanced :: Tree a -> Bool
balanced (Node l r) = abs (leaves l - leaves r) <= 1
                        && balanced l && balanced r
balanced (Leaf x)   = True
leaves :: Tree a -> Int
leaves (Leaf _) = 1
leaves (Node l r) = leaves l + leaves r
balance :: [a] -> Tree a
balance [x] = Leaf x
balance xs  = Node (balance ys) (balance zs)
                where (ys,zs) = halve xs
data Expr = Lit Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr
          | Div Expr Expr
size :: Expr -> Int
size (Add e1 e2)  = 1 + size e1 + size e2
size (Sub e1 e2)  = 1 + size e1 + size e2
size (Mult e1 e2) = 1 + size e1 + size e2
size (Div e1 e2)  = 1 + size e1 + size e2
size (Lit _)      = 0
eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mult e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 `div` eval e2
data Treeboi = Nilt | Nodey Int Treeboi Treeboi
mirrort :: Treeboi -> Treeboi
mirrort (Nilt)        = Nilt
mirrort (Nodey n l r) = Nodey n (mirrort r) (mirrort l)

