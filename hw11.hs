-- Number 1
data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving Show
balanced :: Tree a -> Bool
balanced (Leaf _)   = True
balanced (Node l r) = abs (leaves l - leaves r) <= 1
                        && balanced l && balanced r
leaves :: Tree a -> Int
leaves (Leaf _)   = 1
leaves (Node l r) = leaves l + leaves r

-- Number 2
balance :: [a] -> Tree a
balance [x] = Leaf x
balance xs  = Node (balance ys) (balance zs)
                where (ys,zs) = halve xs

halve :: [a] -> ([a],[a])
halve xs = (take half xs, drop half xs)
                where half = length xs `div` 2

-- Number 3, 4
data Expr = Lit Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr
          | Div Expr Expr

eval :: Expr -> Int
eval (Lit n)        = n
eval (Add e1 e2)    = eval e1 + eval e2
eval (Sub e1 e2)    = eval e1 - eval e2
eval (Mult e1 e2)   = eval e1 * eval e2
eval (Div e1 e2)    = eval e1 `div` eval e2
-- "*** Exception: divide by zero"

size :: Expr -> Int
size (Add e1 e2)  = 1 + size e1 + size e2
size (Sub e1 e2)  = 1 + size e1 + size e2
size (Mult e1 e2) = 1 + size e1 + size e2
size (Div e1 e2)  = 1 + size e1 + size e2
size _            = 0

-- Number 5
data Tree5 = NilT | Node5 Int Tree5 Tree5
    deriving Show

left :: Tree5 -> Tree5
left (Node5 _ l _) = l
right :: Tree5 -> Tree5
right (Node5 _ _ r ) = r

reflect :: Tree5 -> Tree5
reflect (NilT) = NilT
reflect (Node5 n l r) = Node5 n r l

-- Number 6
--data Either a b = Left a | Right b
--twist :: Either a b -> Either b a
--twist (Left a)  = Right a
--twist (Right b) = Left b

-- Number 7

-- Number 8, 9
--Just (Nothing) :: Maybe (Maybe a)
--Just (Just a)  :: Maybe (Maybe a)
squashMaybe :: Maybe (Maybe a) -> Maybe a
squashMaybe (Just a) = a
composeMaybe :: (a -> Maybe b) -> 
                (b -> Maybe c) -> 
                (a -> Maybe c)
composeMaybe f g = \a -> case (f a) of
                            Nothing -> Nothing
                            Just b -> g b


