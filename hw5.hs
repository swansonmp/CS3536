import Data.Char

-- Number 1
third xs = head [x | (x,y) <- zip xs [1..], y == 3]

-- Number 2 
hw5p2 (a:(b:_)) = a + b
hw5p2 (a:_)   = a
hw5p2 _     = 0

-- Number 3 not sure why the type decs werent working
safetailA :: [a] -> [a]
safetailA xs = if null xs then [] else tail xs

safetailB :: [a] -> [a]
safetailB xs
    | null xs   = []
    | otherwise = tail xs

safetailC :: [a] -> [a]
safetailC []    = []
safetailC xs    = tail xs

-- Number 4
-- (||) :: Bool -> Bool -> Bool
-- 1.
-- True || True     = True
-- True || False    = True
-- False || True    = True
-- False || False   = False
-- 2.
-- False || False   = False
-- _     || _       = True
-- 3.
-- False || b   = b
-- True || _    = True
-- 4.
-- b || b   = b
-- _ || _   = True

-- Number 5
-- True && True = True
-- _    && _    = False
and5 :: Bool -> Bool -> Bool
and5 a b = if a then (if b then b else False) else False

-- Number 6
-- True  && b = b
-- False && _ = False
and52 :: Bool -> Bool -> Bool
and52 a b = if a then b else False

-- Number 7
listproc :: (a, [(a,Bool)], a) -> a
listproc (_, [], c)             = c
listproc (a, ((_, True):_), _)  = a
listproc (_, ((b, _):_), _)     = b

-- Number 8
firstHundredSquares = sum [x * x | x <- [1..100]]

-- Number 9
grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x, y) | x <- [0..m] ,y <- [0..n]]

-- Number 10
square :: Int -> [(Int, Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]

-- Number 11
replicate5 :: Int -> a -> [a]
replicate5 n x = [x | _ <- [1..n]]

-- Number 12
divisors :: Int -> [Int]
divisors x = [y | y <- [1..x], x `mod` y == 0]

isPrime :: Int -> Bool
isPrime x = length (divisors x) <= 2

-- Number 13
matches :: Int -> [Int] -> [Int]
matches x xs = [y | y <- xs, y == x]

myElem :: Int -> [Int] -> Bool
myElem x xs = length (matches x xs) > 0

-- Number 14
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- Number 15
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])

