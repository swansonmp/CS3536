-- Number 1
data Month  = January | February | March
            | April | May | June
            | July | August | September
            | October | November | December
                deriving (Show,Eq,Ord)

toSeason :: Month -> String
toSeason m 
    | m <= February = "Winter"
    | m <= May      = "Spring"
    | m <= August   = "Summer"
    | m <= November = "Fall"
    | otherwise     = "Winter"

-- Number 2,3,4,5
data Shape  = Circle Float
            | Rectangle Float Float
            | Triangle Float Float Float
                deriving (Show)

instance Eq Shape where
    Circle r       == Circle r'         = if r<0 then r'<0 else r==r'
    Rectangle l w  == Rectangle l' w'   = l==l' && w==w'
    Triangle a b c == Triangle a' b' c' = a==a' && b==b' && c==c'
    _              == _                 = False

perimeter :: Shape -> Float
perimeter (Circle r)        = 2*pi*r
perimeter (Rectangle l w)   = 2*l+2*w
perimeter (Triangle a b c)  = a+b+c

regular :: Shape -> Bool
regular (Circle r)          = True
regular (Rectangle l w)     = l==w
regular (Triangle a b c)    = a==b&&b==c


-- Number 6
data Ordering' = LT' | EQ' | GT'
compare :: Ord a => a -> a -> Ordering'
compare x y
    | x < y     = LT'
    | x == y    = EQ'
    | otherwise = GT'

-- Number 7
data Nat = Zero | Succ Nat
instance Show Nat where
    show n = show (nat2int n)
instance Read Nat where
--    readsPrec s = int2nat (read s :: Int)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n
int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))
add :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult _ Zero = Zero
mult (Succ Zero) m = m
mult (Succ n)    m = add m (mult n m)

-- Number 8
--instance Eq a => Eq (Maybe a) where
--    Nothing == Nothing = True
--    Just n  == Just m  = n==m
--    _       == _       = False
--instance Eq a => Eq [a] where
--    []     == []     = True
--    (x:xs) == (y:ys) = x==y && xs==ys
--     _     == _      = False
