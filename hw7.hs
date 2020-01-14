import Data.Char

-- Number 1
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (x:xs) = insert x (iSort xs)
                where 
                    insert :: Ord a => a -> [a] -> [a]
                    insert x [] = [x]
                    insert x (y:ys)
                        -- | x <= y    = x:y:ys --Normal
                        -- | x >= y    = x:y:ys --Reverse
                        | x < y     = x:y:ys --No dups
                        | x == y    = x:ys   --
                        | otherwise = y:insert x ys

-- Number 2
merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys)
    | x <= y    = x:merge xs (y:ys)
    | otherwise = y:merge (x:xs) ys
merge xs ys         = xs ++ ys


-- Number 3
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort as) (msort bs)
                where
                    (as,bs) = halve xs

halve :: [a] -> ([a],[a])
halve xs = (take n xs, drop n xs)
            where
                n = length xs `div` 2

-- Number 4
isPalin :: String -> Bool
isPalin str = pstr == reverse pstr
                where pstr = parseStr str
                      parseStr :: String -> String
                      parseStr str = [toLower c | c <- str, not (elem c p)]
                                 where p = " ,!?.\',;\""

-- Number 5
-- snd :: (a,b) -> b
-- sing :: a -> [a]

-- Number 6
--Because id also has type (a -> a), which is the most general
--id CAN return ([[a]]->[[a]]) (ex: [[1],[2]]->[[1],[2]])
--but can also return more general type (ex: 1->1)
--Using the aforementioned type doesn't cover all valid type cases that id can handle

