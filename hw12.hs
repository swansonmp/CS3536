-- Question 1
--instance Monad Maybe where
--    return x = Just x
--    (>>=) m f = case m of
--                    Nothing -> Nothing
--                    Just x -> f x
instance Monad [] where
    --return :: a -> [a]
    return x = [x]
    --(>>=) :: [a] -> (a -> [b]) -> [b]
    (>>=) (m:ms) f = (f m) ++ (>>=) ms f


-- Question 2
data Tree = NilT 
          | Node Int Tree Tree

collapse :: Tree -> [Int]
collapse NilT           = []
collapse (Node n l r)   = collapse l ++ [n] ++ collapse r
sort :: Tree -> [Int]
sort t = qsort (collapse t)
qsort []        = []
qsort (x:xs)    = qsort lt ++ [x] ++ qsort gt
                    where
                        lt = [a | a <- xs, a <= x]
                        gt = [b | b <- xs, b > x]

-- Question 3,4
data Expr = Lit Int
          | Op Ops Expr Expr
          | If BExp Expr Expr
data Ops  = Add | Sub | Mult | Div
data BExp = BoolLit Bool
          | And BExp BExp
          | Not BExp
          | Equal Expr Expr
          | Greater Expr Expr

bEval :: BExp -> Bool
bEval (And b1 b2)     = bEval b1 && bEval b2
bEval (Not b)         = not (bEval b)
bEval (Equal e1 e2)   = eval e1 == eval e2
bEval (Greater e1 e2) = eval e1 >  eval e2
eval :: Expr -> Int
eval (Lit n)         = n
eval (Op Add e1 e2)  = eval e1 + eval e2
eval (Op Sub e1 e2)  = eval e1 - eval e2
eval (Op Mult e1 e2) = eval e1 * eval e2
eval (Op Div e1 e2)  = eval e1 `div` eval e2
size :: Expr -> Int
size (Op _ e1 e2) = 1 + size e1 + size e2
size _          = 0

-- Question 5
data Tr a b = Lf a
            | Nd (Tr a b) b (Tr a b)
mapTr :: (a -> c) -> (b -> d)-> Tr a b -> Tr c d
mapTr f g (Lf x)      = Lf (f x)
mapTr f g (Nd l y r)  = Nd (mapTr f g l) (g y) (mapTr f g r)

-- Question 6
data GTree a = GNode a [GTree a]
                deriving Show
mapGT :: (a -> b) -> GTree a -> GTree b
mapGT f (GNode x xs) = GNode (f x) (map (mapGT f) xs)
--an empty gtree can not be created, as the constructor
--requires that you create at least one node with data

