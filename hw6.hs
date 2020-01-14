-- Number 1
and6 :: [Bool] -> Bool
and6 []     = True
and6 (x:xs) = if x then and6 xs else False

or6 :: [Bool] -> Bool
or6 []      = False
or6 (x:xs)  = if not x then or6 xs else True

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
--show eval of...
--length[1,2,3]
--drop 3 [1,2,3,4,5]
--init[1,2,3]

-- Number 5
concat6 :: [[a]] -> [a]
concat6 [] = []
concat6 (xs:xss) = xs ++ concat6 xss

replicate6 :: Int -> a -> [a]
replicate6 0 x = []
replicate6 n x = x:replicate (n-1) x

selectNthElem :: [a] -> Int -> a
selectNthElem xs 0 = head xs
selectNthElem xs n = selectNthElem (tail xs) (n-1)

elem6 :: Eq a => a -> [a] -> Bool
elem6 x xs
    | xs == []      = False
    | x == head xs  = True
    | otherwise     = elem6 x (tail xs)

-- Number 6
-- elemNum using recursion
elemNum :: Eq a => a -> [a] -> Int
elemNum t [] = 0
elemNum t (x:xs)
    | t == x    = 1 + elemNum t xs
    | otherwise = 0 + elemNum t xs

-- elemNum using list comp
elemNum' :: Eq a => a -> [a] -> Int
elemNum' t xs = length [x | x <- xs, t == x]

-- Number 7
unzip6 :: [(a,b)] -> ([a],[b])
unzip6 [] = ([],[])
unzip6 ((a,b):abps)   = (a:fst abls, b:snd abls) 
                        where
                            abls = unzip6 abps

-- Number 8
-- zip3 using recursion
zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' [] _ _ = []
zip3' _ [] _ = []
zip3' _ _ [] = []
zip3' (x:xs) (y:ys) (z:zs) = (x,y,z):zip3' xs ys zs

--define zip3 in terms of zip???
--zip3'' :: [a] -> [b] -> [c] -> [(a,b,c)]
--zip3'' (x:xs) (y:ys) (z:zs) = 

-- Number 9
returnNth :: [a] -> Int -> a
returnNth xs n = head [x | (x,y) <- zip xs [0..], y == n]

