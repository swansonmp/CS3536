import Data.Char

--HW6
and' :: [Bool] -> Bool
and' [] = True
and' (b:bs) = b && and bs
or' :: [Bool] -> Bool
or' [] = False
or' (b:bs) = b || or bs
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)
--TODO this can be a call to fold
pow _ 0 = 1
pow n p = n * pow n (p-1)
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss
replicate' 0 x = []
replicate' n x = x : replicate' (n-1) x
select' :: [a] -> Int -> a
select' [] _     = error "invalid index"
select' (x:xs) 0 = x
select' (x:xs) n = select' xs (n-1)
elem' _  []     = False
elem' k' (k:ks) = k'==k || elem' k' ks
unzip' :: [(a,b)] -> ([a],[b])
unzip' [] = ([],[])
unzip' ((a,b):abs) = ((a:as),(b:bs))
                        where (as,bs) = unzip' abs
elemNum :: Eq a => a -> [a] -> Int
elemNum _ [] = 0
elemNum x (y:ys)
    | x == y    = 1 + elemNum x ys
    | otherwise = 0 + elemNum x ys
elemNum' x ys = length [x | y <- ys, x == y]
zip3' [] _ _ = []
zip3' _ [] _ = []
zip3' _ _ [] = []
zip3' (x:xs) (y:ys) (z:zs) = (x,y,z) : zip3' xs ys zs
--TODO in terms of zip???
select'' xs n = head [x | (x, i) <- zip xs [0..], i == n]

--HW7
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
            where (ys,zs) = halve xs
halve :: [a] -> ([a],[a])
halve xs = (take half xs, drop half xs)
            where half = length xs `div` 2
isPalin :: String -> Bool
isPalin s = s'== reverse s'
                where s' = parse s
parse :: String -> String
parse s = [toLower c | c <- s, not (elem c " ,:!?.\';\"")]

-- HW8
length' = sum . map (\a -> 1)
--map f (map g xs)       -> map (f . g) xs
--filter p (filter q xs) -> filter (\x -> p x && q x) xs
--map f (filter p xs)
all' p = and . map p
any' p = or . map p
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs)
    | p x       = x : takeWhile' p xs
    | otherwise = x:xs
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs)
    | p x       = dropWhile' p xs
    | otherwise = xs
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x xs -> (f x):xs) [] xs
filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = foldr (\x xs -> if p x then x:xs else xs) [] xs
dec2int :: [Int] -> Int
dec2int = foldl f 0
                where f n x = 10 * n + x
curry' = (\f (x,y) -> f x y)
uncurry' = (\f x y -> f (x,y))
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = map (uncurry f) (zip xs ys)
iter :: Int -> (a -> a) -> a -> a
iter 0 _ x = x
iter n f x = iter (n-1) f (f x)
power2 :: Int -> Int
power2 n = iter (n) (*2) 1
sumsquares :: [Int] -> Int
sumsquares = sum . map (^2)
filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst _ [] = []
filterFirst p (x:xs)
    | p x       = x : filterFirst p xs
    | otherwise = xs
composeList :: [(a -> a)] -> (a -> a)
composeList = foldr (.) id
flip :: (a -> b -> c) -> (b -> a -> c)
flip = (\f a b -> f b a)
hw9p6 :: Char -> Bool
hw9p6 c = not (elem c " \t\n")
total :: (Int -> Int) -> Int -> Int
total = \f n -> sum (map f [0..n])
hw9p8a xs yss = map (zip xs) yss
hw9p8b xs yss = map (\ys -> zip ys) xs -- TODO fix
--TODO look at hw9
--filter (>0) . map (+1) = map (+1) . filter (>=0)
data Shape = Circle Float
           | Rectangle Float Float
           | Triangle Float Float Float
regular :: Shape -> Bool
regular (Circle r) = True
regular (Rectangle l w) = l==w
regular (Triangle a b c) = a==b && b==c
instance Eq Shape where
    (Circle r1) == (Circle r2) = if r1<0 then r2<0 else r1==r2
    (Rectangle l1 w1) == (Rectangle l2 w2) = l1==l2 && w1==w2
    (Triangle a1 b1 c1) == (Triangle a2 b2 c2) = a1==a2 && b1==b2 && c1==c2
    _ == _ = False
compare :: Ord a => a -> a -> Ordering
compare x y
    | x < y     = LT
    | x > y     = GT
    | otherwise = EQ
--instance Eq a => Eq (Maybe a) where
--  Nothing == Nothing = True
--  Just a == Just b = a==b
--  _ == _ = False
--instance Eq a => Eq [a] where
--  [] == []        = True
--  (x:xs) = (y:ys) = x==y && xs==ys
--  _ == _ = False

-- HW11
--data Tree a = Leaf a | Node (Tree a) (Tree a)
--balanced :: Tree a -> Bool
--balanced (Leaf _)   = True
--balanced (Node l r) = (abs (leaves l - leaves r) <= 1) && balanced l && balanced r
--leaves :: Tree a -> Int
--leaves (Leaf _)   = 1
--leaves (Node l r) = leaves l + leaves r
--balance :: [a] -> Tree a
--balance [x] = Leaf x
--balance xs  = Node (balance ys) (balance zs)
--                where (ys,zs) = halvey xs
--halvey ws = (take half ws, drop half ws)
--            where half = length ws `div` 2
--data Expr = Lit Int
--          | Add Expr Expr
--          | Sub Expr Expr
--          | Mult Expr Expr
--          | Div Expr Expr
--size :: Expr -> Int
--size (Lit _) = 0
--size (Add e1 e2) = 1 + (size e1) + (size e2)
--size (Sub e1 e2) = 1 + (size e1) + (size e2)
--size (Mult... you know the rest
--eval... you've done this already
data Tree = NilT | Node Int Tree Tree
reflect :: Tree -> Tree
reflect NilT = NilT
reflect (Node n l r) = Node n (reflect r) (reflect l)
collapse :: Tree -> [Int]
collapse NilT = []
collapse (Node n l r) = collapse l ++ [n] ++ collapse r
sort :: Tree -> [Int]
sort t = msort (collapse t)
twist :: Either a b -> Either b a
twist (Left a) = Right a
twist (Right b) = Left b

