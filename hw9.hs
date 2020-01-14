addOne :: Int -> Int
addOne n = 1 + n

-- Number 1
num1 :: [Int] -> Int
num1 xs = sum (map (^2) xs)

-- Number 2A
qsortBy :: Ord b => (a -> b) -> [a] -> [a]
qsortBy f [] = []
qsortBy f (x:xs) = qsortBy f [y | y <- xs, f y <= f x]
                    ++ [x] 
                    ++ qsortBy f [y | y <- xs, f y > f x]
--rank is product of first 2 elements
--  plus (last elem of list minus first elem of list)
sortTriples :: [(Int, Int, [Int])] -> [(Int, Int, [Int])]
sortTriples xs = qsortBy (\(a,b,cs) -> -1 * (a*b + (head cs) 
                    - (head (reverse cs)))) xs

-- Number 2B
halve :: [a] -> ([a],[a])
halve xs = (take half xs, drop half xs)
                where half = length xs `div` 2
mergeBy :: Ord b => (a -> b) -> [a] -> [a] -> [a]
mergeBy f xs [] = xs
mergeBy f [] ys = ys
mergeBy f (x:xs) (y:ys)
    | f x < f y = x : mergeBy f xs (y:ys)
    | otherwise = y : mergeBy f (x:xs) ys
msortBy :: Ord b => (a -> b) -> [a] -> [a]
msortBy f [] = []
msortBy f [x] = [x]
msortBy f xs = mergeBy f (msortBy f ys) (msortBy f zs)
                    where (ys, zs) = halve xs
sortBySumsThenSize :: [([Int], Float)] -> [([Int], Float)]
sortBySumsThenSize xs = msortBy (\(ys,z) -> -(sum ys)) as
                            where as = msortBy (\(bs,c) -> c) xs

-- Number 3
id3 x = x
--(id . f) => (Int -> Bool) -> (Bool -> Bool)
--(f . id) => (a -> a) -> (Int -> Bool)
--id f => (Int -> Bool) -> (Int -> Bool)

-- Number 4
composeList :: [(a -> a)] -> (a -> a)
composeList = foldr (.) id

-- Number 5
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' = \f x y -> f y x

-- Number 6
num6 :: Char -> Bool
num6 = \x -> not (elem x " \t\n")

-- Number 7
total :: (Int -> Int) -> Int -> Int
total = \f n -> sum (map f [0..n])

-- Number 8A
--zip every xs with every list in yss
num8a :: [a] -> [[b]] -> [[(a,b)]]
num8a = \xs yss -> map (zip xs) yss

-- Number 8B
num8b :: [[a]] -> [b] -> [[(a,b)]]
num8b = \yss xs -> map (\ys -> zip ys xs) yss

-- Number 9
--filter p . map f = map g . filter h
--
--f :: a -> b
--p :: b -> Bool
--h :: a -> Bool
--g :: a -> b
--
--h = p . f
--g = f
--
--right side more efficient bc you wont have to map
--as many items assuming at least one item is filtered

-- Number 10
num10a = filter (>0) . map (+1)
num10b = map (+1) . filter (>=0)


