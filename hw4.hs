averageThree :: Float -> Float -> Float -> Float
averageThree x y z = (x + y + z) / 3

howManyAboveAverage :: Float -> Float -> Float -> Int
howManyAboveAverage x y z   = gtAvg x avg + gtAvg y avg + gtAvg z avg
                                where avg = averageThree x y z

gtAvg :: Float -> Float -> Int
gtAvg n avg = if (n > avg) then 1 else 0


maxOccurs :: Int -> Int -> (Int, Int)
maxOccurs x y | x > y       = (x, 1)
              | x < y       = (y, 1)
              | otherwise   = (x, 2)

maxOccurs2 :: (Int, Int) -> (Int, Int)
maxOccurs2 (x, y) | x > y       = (x, 1)
                  | x < y       = (y, 1)
                  | otherwise   = (x, 2)

maxThreeOccurs :: Int -> Int -> Int -> (Int, Int)
maxThreeOccurs x y z    = (max, (eqMax x max + eqMax y max + eqMax z max))
                            where max = getMax x y z

getMax :: Int -> Int -> Int -> Int
getMax x y z
    | x >= y && x >= z  = x
    | y >= x && y >= z  = y
    | otherwise         = z

eqMax :: Int -> Int -> Int
eqMax n max = if (n == max) then 1 else 0


halve :: [a] -> ([a], [a])
halve xs = (take half xs, drop half xs)
            where half = length xs `div` 2

thirdA :: [a] -> a
thirdA xs = head (tail (tail xs))

thirdB :: [a] -> a
thirdB xs = xs !! 2


