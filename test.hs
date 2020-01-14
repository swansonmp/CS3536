qsort []        = []
qsort (x:xs)    = qsort smaller ++ [x] ++ qsort larger 
                where
                    smaller = [a | a <- xs, a < x]
                    larger  = [b | b <- xs, b > x]

pyramid :: Int -> Int
pyramid 1 = 1
pyramid n = n * n + pyramid (n-1)

plist :: Int -> [Int]
plist 1 = [1]
plist n = qsort (pyramid n:plist (n-1))

