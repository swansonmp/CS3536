-- Number 1
--and6 :: [Bool] -> Bool
--and6 []     = True
--and6 x:xs   = if x then and6 xs else False

--or6 :: [Bool] -> Bool
--or6 []      = False
--or6 x:xs    = if not x then or6 xs else True

-- Number 2
sumdown :: Int -> Int
sumdown n
    | n < 0     = error "undefined"
    | n == 0    = 0
    | otherwise = n + sumdown (n - 1)

-- Number 3
pow6 :: Int -> Int -> Int
pow6 n p
    | p < 0     = error "undefined"
    | p == 0    = 1
    | otherwise = n * pow6 n (p - 1)

-- Number 4


-- Number 5


-- Number 6


-- Number 7


-- Number 8
zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' [] _ _ = []
zip3' _ [] _ = []
zip3' _ _ [] = []
zip3' (x:xs) (y:ys) (z:zs) = (x,y,z):zip3' xs ys zs

-- Number 9


