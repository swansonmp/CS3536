-- Number 1
length' :: [a] -> Int
length' xs = sum (map one xs)
                where one x = 1

-- Functions used in 2,3,4,...
greaterOne n = n > 1
addOne n = n + 1

-- Number 2
addUp ns = filter greaterOne (map addOne ns)
addUp' ns = map fun1 (filter fun2 ns)
            where
                fun1 n = addOne n
                fun2 n = n > 0

-- Number 3
--number3 ns = map addOne (map addOne ns)
-- write map f (map g xs) as one call to map
-- map (f . g) xs
num3 = map (addOne . addOne)

-- Number 4
--number4 ns = filter greaterOne (filter lessTen ns)
--                where lessTen n = n < 10
-- write filter p (filter q xs) as one call to map
-- filter (p . q) xs
-- FIX THIS --------------------------------------------------
num4 xs = filter r xs
            where r x = greaterOne x && lessTen x

lessTen n = n < 10

-- Number 5
num5 f p xs = [f x | x <- xs, p x]
num5' f p xs = map f (filter p xs) 

-- Number 6
--WHY ARE THESE TYPEDEFS NOT COMPILING
all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p
any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ []                  = []
takeWhile' p (x:xs)  | p x       = x : takeWhile p xs
                     | otherwise = [] 
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ []                  = []
dropWhile' p (x:xs)  | p x       = dropWhile p xs
                     | otherwise = x:xs

-- Number 7
-- I STILL DONT UNDERSTAND THIS ONE
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []
filter' p = foldr (\x xs -> if p x then x:xs else xs) []

-- Number 8
dec2Int :: [Int] -> Int
dec2Int = foldl f 0
            where f n x = 10 * n + x

-- Number 9
curry' :: ((a,b) -> c) -> (a -> b -> c)
--curry' f = \x y -> f (x,y)
curry' f x y = f (x,y)
uncurry' :: (a -> b -> c) -> ((a,b) -> c)
uncurry' f = \(x,y) -> f x y

-- Number 10
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = map (uncurry f) (zip xs ys)

-- Number 11
iter :: Int -> (a -> a) -> a -> a
iter 0 _ x = x
iter n f x = iter (n-1) f (f x)

-- Number 12
power2 :: Int -> Int
power2 0 = 1
power2 n = iter (n-1) double 2
              where double x = x * 2

-- Number 13
-- ret sum of squares of pos ints
sumSqrPos :: [Int] -> Int
sumSqrPos xs = foldr (+) 0 (map (^2) (filter (>0) xs))

-- Number 14
--the id function
mystery :: [a] -> [a]
mystery xs = foldr (++) [] (map sing xs)
                where sing x = [x]

-- Number 15
filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst p (x:xs) 
    | p x       = x : filterFirst p xs
    | otherwise = xs

