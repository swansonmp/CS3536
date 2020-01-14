-- HW2
square2 x = x * x
cube x = x * x * x
cube' x = square2 x * x
product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product xs
qsort []     = []
qsort (x:xs) = qsort s ++ [x] ++ qsort l
                where
                    s = [a | a <- xs, a < x]
                    l = [b | b <- xs, b > x]
double x = x * 2
dThenS x = square2 (double x)
sThenD x = double (square2 x)
n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5] 
last' :: [a] -> a
last' xs = head (reverse xs)
last'' xs = head (drop n xs)
            where n = length xs - 1
init' :: [a] -> [a]
init' xs = reverse (drop 1 (reverse xs))
init'' xs = take n xs
            where n = length xs - 1

-- HW3
bools = [True,False]
num = [[1,2],[2,3]]
add x y z = x+y+z
copy x = (x,x)
apply f x = f x
twice f x = f (f x)
xor a b = not a && b || a && (not b)
mystery m n p = not((m==n) && (n==p))
threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent x y z = not ((x==y)||(x==z)||(y==z))

-- HW4 
averageThree :: Float -> Float -> Float -> Float
averageThree x y z = (x+y+z)/3
howManyAboveAverage :: Float -> Float -> Float -> Int
howManyAboveAverage x y z = sum [1 | w <- ws, w > avg]
                            where
                                ws = [x,y,z]
                                avg = averageThree x y z
maxOccurs :: Int -> Int -> (Int,Int)
maxOccurs x y
    | x == y    = (x,2)
    | x > y     = (x,1)
    | otherwise = (y,1)
maxThreeOccurs :: Int -> Int -> Int -> (Int,Int)
maxThreeOccurs x y z
    | z > m     = (z,1)
    | z < m     = (m,n)
    | otherwise = (z,n+1)
        where (m,n) = maxOccurs x y
halve :: [a] -> ([a],[a])
halve xs = (take n xs, drop n xs)
            where n = length xs `div` 2
thirdA xs = head (tail (tail xs))
thirdB xs = xs !! 2
thirdC xs = head [x | (x,n) <- zip xs [0..], n == 2]

-- HW5
safeSum2 :: [Int] -> Int
safeSum2 [] = 0
safeSum2 [x] = x
safeSum2 (x:y:xs) = x+y
safetailA xs = if xs == [] then [] else tail xs
safetailB xs
    | xs == []  = []
    | otherwise = tail xs
safetailC [] = []
safetailC xs = tail xs
or' :: Bool -> Bool -> Bool
or' True True    = True
or' True False   = True
or' False True   = True
or' False False  = False
or'' False False = False
or'' _ _         = True
or''' False b    = b
or''' True _     = True
or'''' b c   | b == c    = b
            | otherwise = True
and' a b = if a then if b then b else b else a
and'' a b = if a then b else a
listproc :: (a,[(a,Bool)],a) -> a
listproc (_,[],c)           = c
listproc (a,((_,True):_),_) = a
listproc (_,((b,_):_),_)    = b
sumFirstNSquares n = sum [x*x | x <- [1..n]]
grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]
square :: Int -> [(Int,Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]
replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]
divisors :: Int -> [Int]
divisors n = [x | x <- [1..n], n `mod` x == 0]
isPrime :: Int -> Bool
isPrime n = length (divisors n) == 2
matches :: Int -> [Int] -> [Int]
matches t xs = [x | x <- xs, x == t]
myElem :: Int -> [Int] -> Bool
myElem t xs = length (matches t xs) > 0
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2+y^2==z^2]
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']
positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])

-- HW6
listand [] = True
listand (x:xs) = x && listand xs
listor [] = False
listor (x:xs) = x || listor xs
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)
pow n 0 = 1
pow n p = n * pow n (p-1)
concat6 :: [[a]] -> [a]
concat6 [] = []
concat6 (xs:xss) = xs ++ concat6 xss
replicate6 :: Int -> a -> [a]
replicate6 0 x = []
replicate6 n x = x:replicate6 (n-1) x
selectNthElem6 :: [a] -> Int -> a
selectNthElem6 (x:_) 0 = x
selectNthElem6 (x:xs) n = selectNthElem6 xs (n-1)
elem6 :: Eq a => a -> [a] -> Bool
elem6 k [] = False
elem6 k (x:xs) = k == x || elem6 k xs
elemNum :: Eq a => a -> [a] -> Int
elemNum k [] = 0
elemNum k (x:xs)
    | k == x    = 1 + elemNum k xs
    | otherwise = 0 + elemNum k xs
elemNum' :: Eq a => a -> [a] -> Int
elemNum' k xs = length [x | x <- xs, k == x]
unzip' :: [(a,b)] -> ([a],[b])
unzip' [] = ([],[])
unzip' ((a,b):abs) = ((a:x),(b:y))
                    where (x,y) = unzip' abs
zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' [] _ _ = []
zip3' _ [] _ = []
zip3' _ _ [] = []
zip3' (x:xs) (y:ys) (z:zs) = (x,y,z): zip3 xs ys zs
returnNth :: Int -> [a] -> a
returnNth n xs = head [x | (x,y) <- zip xs [0..n], y == n]

-- HW7
--dude ive done enough for one day like geez

