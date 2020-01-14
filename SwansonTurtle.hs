--Matthew Swanson
--Turtle Graphics Project
--2019.04.10

--import Data.Char for use of the toLower method
import Data.Char

--type synonym for a state of the turtle.
--includes a direction, state of the pen, and the current point
type State      = (Direction, Pen, Point)
--data dec for the 4 cardinal directions
data Direction  = North | East | South | West
                    deriving Show
--type synonym for state of the pen.
--penUp is False, pedDown is True.
type Pen        = Bool 
--type for a coordinate point. (x,y)
type Point      = (Int, Int)
--type that takes a State and returns another state, typically modified
type Command    = State -> State

-- moves the turtle one step along the grid
-- in the direction it's currently facing
-- uses pattern matching on Point to increment/decrement Point accordingly
-- North(1,0), South(-1,0), East(0,-1), West(0,1)
move :: Command
move (North, p, (x,y)) = (North, p, (x+1,y))
move (South, p, (x,y)) = (South, p, (x-1,y))
move (East, p, (x,y))  = (East, p, (x,y-1))
move (West, p, (x,y))  = (West, p, (x,y+1))

-- turns the turtle 90* counterclockwise
-- uses pattern matching on Direction to modify Direction accordingly
left :: Command
left (North, p, pt) = (West, p, pt)
left (South, p, pt) = (East, p, pt)
left (East, p, pt)  = (North, p, pt)
left (West, p, pt)  = (South, p, pt)
-- turns the turtle 90* clockwise
-- uses pattern matching on Direction to modify Direction accordingly
right :: Command
right (North, p, pt) = (East, p, pt)
right (South, p, pt) = (West, p, pt)
right (East, p, pt)  = (South, p, pt)
right (West, p, pt)  = (North, p, pt)

-- raises the turtle's pen from the paper
-- takes a state and sets Pen to False using pattern matching
penUp :: Command
penUp (d, _, (x,y))     = (d, False, (x,y))
-- lowers the turtle's pen onto the paper
-- takes a state and sets Pen to True using pattern matching
penDown :: Command
penDown (d, _, (x,y))   = (d, True, (x,y))

-- makes n copies of x as a list
-- makes use of the neat trick to use [1..n] as a counter in the list comp
copy :: a -> Int -> [a]
copy x n = [x | _ <- [1..n]]

--takes a list of commands and returns a list of states
--applies the list of commands to the starting State (facing North, pen up, at the origin)
turtle :: [Command] -> [State]
turtle = scan applyto (West, False, (0,0)) --sigh

--takes an input and a function and returns the result of the application
applyto :: a -> (a -> b) -> b
applyto x f = f x

--scan op val [x1,x2,...] = [val, op val x1, op (op val x1) x2,
--takes the second arg and each item of the list and feeds the function through them,
--with the middle indexes of the list being intermediate results.
--v is the first element of the list, and the final result is stored at the end
scan :: (a -> b -> a) -> a -> [b] -> [a]
scan _ v [] = [v]
scan f v (x:xs) = v : scan f (f v x) xs

--input a list of commands and output a list of characters
--uses composition of four different functions
--explanations of each function can be found at corresponding functions
display :: [Command] -> [Char]
display = layout . picture . trail . turtle

--trail takes a list of states and returns a list of visited points
--the list comprehension returns only the points where the pen was activated
trail :: [State] -> [Point]
trail xs = [(x,y) | (_,b,(x,y)) <- xs, b]

--takes a list of list of char and concats it into a list of char to be sent to output
layout :: [[Char]] -> [Char]
layout = concat

--takes a list of visited points and returns a char list of list
--that is a char representation of the points that the turtle has visited
--utilizes composition of bitmap and symbolize
picture :: [Point] -> [[Char]]
picture = symbolize . bitmap

--takes a list of points and returns a list of list of Bool
--with true representing spots a)the turtle has visited and
--b)spots when the pen was down
bitmap :: [Point] -> [[Bool]]
bitmap ps = [[(x,y) `elem` ps | y <- yrange] | x <- xrange]
            where   xrange   = range (fsts ps)
                    yrange   = range (snds ps)
                    range [] = []
                    range xs = [minimum xs .. maximum xs]

--takes a list of pairs and returns a list of the first elements
--maps the library fst function with the input list
fsts :: [(a, b)] -> [a]
fsts = map fst
--takes a list of pairs and returns a list of the second elements
--maps the library snd function with the input list
snds :: [(a, b)] -> [b]
snds = map snd

--converts a boolean to a character
--used in converting elements of the bitmap to output-able characters
boolstr :: Bool -> Char
boolstr x = if x then '#' else '.'

--converts the bitmap into a corresponding list of list of characters
--maps boolstr to every list within the list of lists.
--also appends a newline at the end of each list
symbolize :: [[Bool]] -> [[Char]]
symbolize xss = [(map boolstr xs) ++ ['\n'] | xs <- xss]

--takes a list of commands, processes them, and sends the
--result to output. Composes the aformentioned display method
--with the library putStr, which just sends the result of display to output
draw :: [Command] -> IO ()
draw = putStr . display


--Pattern-drawing methods--

-- draws a square
square :: Int -> [Command]
square 0 = []
square k = [penDown] ++ concat (copy side 4) ++ [penUp]
                where side = copy move (k-1) ++ [right]
--draws a solid square
--calls square on desired size then recusively draws smaller squares
block 0 = []
block k = square k ++ block (k-1)

--poorly named function that is short for 'move times'
--while keeping original orientation and pen state, 
--moves n units in a corresponding direction RELATIVE to 
--the turtles current orientation
--EX: moveT 3 North while the turtle is facing east will result in
--moving east 3 units, while mainting Easterly orientation
moveT :: Int -> Direction -> [Command]
moveT n North    = moveN n
moveT n East     = [right] ++ moveN n ++ [left]
moveT n South    = uturn ++ moveN n ++ uturn
moveT n West     = [left] ++ moveN n ++ [right]
--helper function for moveT; just returns a list of n moves
moveN :: Int -> [Command]
moveN n = concat (copy [move] n)
--wee function that turns the turtle 180*
uturn :: [Command]
uturn = [right,right]

--my first interesting turtle trail which takes two ints and 
--returns commands to draw a drawing of a prism
--the first argument determines the length-width of the prism, and
--the second argument determines the depth of the prism
--draws a square, and then traverses the corners of the square, adding in the connecty-bits
--finally, it draws the second square
prism :: Int -> Int -> [Command]
prism l h = square l ++ staircaseAndRet h ++ moveT (l-1) East
    ++ staircaseAndRet h ++ moveT (l-1) North ++ staircaseAndRet h
    ++ moveT (l-1) West ++ staircaseAndRet h ++ moveT (l-1) South
    ++ staircase h ++ square l
--travels a staircase like path, copying a subfunction wiggle n times
staircase :: Int -> [Command]
staircase n = concat (copy wiggle n)
--like staircase, but returns pack to the original position
staircaseAndRet :: Int -> [Command]
staircaseAndRet n = staircase n ++ uturn ++ staircase n ++ uturn
--performs one step of the staircase maneuver, drawing in only the start and stop positions
wiggle :: [Command]
wiggle = [move,left,move,penDown,right,penUp]

--my second interesting turtle trail that returns commands
--to draw and string fed as an argument
--concats the result of mapping each character in the string to helper drawChar
----the initial north allows the turtle to orient its drawings to be human readable
drawStr :: String -> [Command]
drawStr s = [penUp,left] ++ (foldr (++) [] (map drawChar s))
--returns commands to draw a single character
--turtle stops at the position where the next char would be drawn
--uses a long case statement to call submethods for each char
--this took way too long
drawChar :: Char -> [Command]
drawChar c = case toLower c of
                'a' -> drawA
                'b' -> drawB
                'c' -> drawC
                'd' -> drawD
                'e' -> drawE
                'f' -> drawF
                'g' -> drawG 
                'h' -> drawH
                'i' -> drawI
                'j' -> drawJ
                'k' -> drawK
                'l' -> drawL
                'm' -> drawM
                'n' -> drawN
                'o' -> drawO
                'p' -> drawP
                'q' -> drawQ
                'r' -> drawR
                's' -> drawS
                't' -> drawT
                'u' -> drawU
                'v' -> drawV
                'w' -> drawW 
                'x' -> drawX
                'y' -> drawY
                'z' -> drawZ
                ' ' -> drawSpace
                '.' -> drawPeriod
                ',' -> drawComma
                '!' -> drawExclamation
                '?' -> drawQuestion
                '\'' -> drawApost
                otherwise -> []
drawA :: [Command]
drawA = [penDown] ++ moveT 4 North ++ moveT 4 East 
    ++ moveT 2 South ++ moveT 3 West ++ moveT 3 East 
    ++ moveT 2 South ++ [penUp] ++ moveT 2 East
drawB :: [Command]
drawB = [penDown] ++ moveT 4 North ++ moveT 3 East ++ [penUp] 
    ++ moveT 1 South ++ moveT 1 East ++ [penDown,penUp] ++ moveT 2 South
    ++ [penDown,penUp] ++ moveT 1 West ++ moveT 1 North ++ [penDown]
    ++ moveT 3 West ++ moveT 2 South ++ moveT 3 East ++ [penUp] ++ moveT 3 East
drawC :: [Command]
drawC = [penDown] ++ moveT 4 North ++ moveT 4 East
    ++ moveT 4 West ++ moveT 4 South ++ moveT 4 East
    ++ [penUp] ++ moveT 2 East
drawD :: [Command]
drawD = [penDown] ++ moveT 4 North ++ moveT 3 East ++ [penUp]
    ++ moveT 1 South ++ moveT 1 East ++ [penDown]
    ++ moveT 2 South ++ [penUp] ++ moveT 1 West ++ moveT 1 South
    ++ [penDown] ++ moveT 3 West ++ [penUp] ++ moveT 6 East
drawE :: [Command]
drawE = [penDown] ++ moveT 4 North ++ moveT 4 East
    ++ moveT 4 West ++ moveT 2 South ++ moveT 3 East
    ++ moveT 3 West ++ moveT 2 South ++ moveT 4 East
    ++ [penUp] ++ moveT 2 East
drawF :: [Command]
drawF = [penDown] ++ moveT 4 North ++ moveT 4 East
    ++ moveT 4 West ++ moveT 2 South ++ moveT 3 East
    ++ moveT 3 West ++ moveT 2 South ++ [penUp] ++ moveT 6 East
drawG :: [Command]
drawG = [penDown] ++ moveT 4 North ++ moveT 4 East ++ [penUp] ++ moveT 2 West
    ++ moveT 2 South ++ [penDown] ++ moveT 2 East ++ moveT 2 South
    ++ moveT 3 West ++ moveT 3 East ++ [penUp] ++ moveT 2 East
drawH :: [Command]
drawH = [penDown] ++ moveT 4 North ++ moveT 2 South
    ++ moveT 4 East ++ moveT 2 North ++ moveT 4 South
    ++ [penUp] ++ moveT 2 East
drawI :: [Command]
drawI = [penDown] ++ moveT 2 East ++ moveT 4 North ++ moveT 2 West
    ++ moveT 4 East ++ moveT 2 West ++ moveT 4 South
    ++ moveT 2 East ++ [penUp] ++ moveT 2 East
drawJ :: [Command]
drawJ = [penDown] ++ moveT 1 North ++ moveT 1 South ++ moveT 4 East
    ++ moveT 4 North ++ moveT 4 South ++ [penUp] ++ moveT 2 East
drawK :: [Command]
drawK = [penDown] ++ moveT 4 North ++ moveT 2 South ++ moveT 2 East
    ++ [penUp] ++ moveT 1 East ++ moveT 1 South ++ [penDown,penUp]
    ++ moveT 2 North ++ [penDown,penUp] ++ moveT 1 East ++ moveT 1 North
    ++ [penDown,penUp] ++ moveT 4 South ++ [penDown,penUp] ++ moveT 2 East
drawL :: [Command]
drawL = [penDown] ++ moveT 4 North ++ moveT 4 South 
    ++ moveT 4 East ++ [penUp] ++ moveT 2 East
drawM :: [Command]
drawM = [penDown] ++ moveT 4 North ++ [penUp] ++ moveT 1 East ++ moveT 1 South
    ++ [penDown,penUp] ++ moveT 1 East ++ moveT 1 South ++ [penDown,penUp]
    ++ moveT 1 East ++ moveT 1 North ++ [penDown,penUp] ++ moveT 1 East ++ moveT 1 North
    ++ [penDown] ++ moveT 4 South ++ [penUp] ++ moveT 2 East
drawN :: [Command]
drawN = [penDown] ++ moveT 4 North ++ [penUp] ++ moveT 1 East ++ moveT 1 South
    ++ [penDown,penUp] ++ moveT 1 East ++ moveT 1 South ++ [penDown,penUp]
    ++ moveT 1 East ++ moveT 1 South ++ [penDown,penUp] ++ moveT 1 East ++ moveT 1 South 
    ++ [penDown] ++ moveT 4 North ++ [penUp] ++ moveT 4 South ++ moveT 2 East
drawO :: [Command]
drawO = [penDown] ++ moveT 4 North ++ moveT 4 East
    ++ moveT 4 South ++ moveT 4 West ++ moveT 4 East
    ++ [penUp] ++ moveT 2 East
drawP :: [Command]
drawP = [penDown] ++ moveT 4 North ++ moveT 4 East
    ++ moveT 2 South ++ moveT 4 West
    ++ [penUp] ++ moveT 2 South ++ moveT 6 East
drawQ :: [Command]
drawQ = [penDown] ++ moveT 4 North ++ moveT 2 East ++ moveT 4 South 
    ++ moveT 1 West ++ moveT 3 East ++ [penUp] ++ moveT 2 East
drawR :: [Command]
drawR = [penDown] ++ moveT 4 North ++ moveT 3 East ++ [penUp]
    ++ moveT 1 South ++ moveT 1 East ++ [penDown,penUp]
    ++ moveT 1 South ++ moveT 2 West ++ [penDown] ++ moveT 2 West
    ++ moveT 3 East ++ [penUp] ++ moveT 1 South ++ [penUp,penDown,penUp] 
    ++ moveT 1 South ++ moveT 1 East ++ [penDown,penUp] ++ moveT 2 East
drawS = [penDown] ++ moveT 4 East ++ moveT 2 North ++ moveT 4 West
    ++ moveT 2 North ++ moveT 4 East ++ [penUp] ++ moveT 4 South
    ++ moveT 2 East
drawT :: [Command]
drawT = moveT 4 North ++ [penDown] ++ moveT 4 East ++ moveT 2 West
    ++ moveT 4 South ++ [penUp] ++ moveT 4 East
drawU :: [Command]
drawU = [penDown] ++ moveT 4 North ++ moveT 4 South ++ moveT 4 East
    ++ moveT 4 North ++ moveT 4 South ++ [penUp] ++ moveT 2 East
drawV :: [Command]
drawV = moveT 2 North ++ [penDown] ++ moveT 2 North ++ moveT 2 South
    ++ [penUp] ++ moveT 1 East ++ moveT 1 South ++ [penDown,penUp]
    ++ moveT 1 East ++ moveT 1 South ++ [penDown,penUp] ++ moveT 1 East
    ++ moveT 1 North ++ [penDown,penUp] ++ moveT 1 East ++ moveT 1 North
    ++ [penDown] ++ moveT 2 North ++ [penUp] ++ moveT 4 South ++ moveT 2 East
drawW :: [Command]
drawW = [move,penDown] ++ moveT 3 North ++ [penUp] ++ moveT 4 South
    ++ moveT 1 East ++ [penDown] ++ moveT 1 North ++ [penUp]
    ++ moveT 1 North ++ moveT 1 East ++ [penDown,penUp]
    ++ moveT 1 East ++ moveT 1 South ++ [penDown] ++ moveT 1 South
    ++ [penUp] ++ moveT 1 East ++ moveT 1 North ++ [penDown]
    ++ moveT 3 North ++ [penUp] ++ moveT 4 South ++ moveT 2 East
drawX :: [Command]
drawX = [penDown,penUp] ++ moveT 1 North ++ moveT 1 East ++ [penDown,penUp]
    ++ moveT 1 North ++ moveT 1 East ++ [penDown,penUp] ++ moveT 1 North
    ++ moveT 1 East ++ [penDown,penUp] ++ moveT 1 North ++ moveT 1 East
    ++ [penDown,penUp] ++ moveT 4 West ++ [penDown,penUp] ++ moveT 1 South
    ++ moveT 1 East ++ [penDown,penUp] ++ moveT 1 South ++ moveT 1 East 
    ++ [penDown,penUp] ++ moveT 1 South ++ moveT 1 East ++ [penDown,penUp]
    ++ moveT 1 South ++ moveT 1 East ++ [penDown,penUp] ++ moveT 2 East
drawY :: [Command]
drawY = moveT 4 North ++ [penDown] ++ moveT 1 South ++ [penUp]
    ++ moveT 1 South ++ moveT 1 East ++ [penDown] ++ moveT 2 East
    ++ [penUp] ++ moveT 1 East ++ moveT 1 North ++ [penDown] ++ moveT 1 North
    ++ [penUp] ++ moveT 2 West ++ moveT 2 South ++ [penDown] ++ moveT 2 South
    ++ [penUp] ++ moveT 4 East
drawZ :: [Command]
drawZ = moveT 4 North ++ [penDown] ++ moveT 4 East ++ [penUp] 
    ++ moveT 1 West ++ moveT 1 South ++ [penDown,penUp] ++ moveT 1 West
    ++ moveT 1 South ++ [penDown,penUp] ++ moveT 1 West ++ moveT 1 South 
    ++ [penDown,penUp] ++ moveT 1 West ++ moveT 1 South ++ [penDown] 
    ++ moveT 4 East ++ [penUp] ++ moveT 2 East
drawSpace :: [Command]
drawSpace = moveT 6 East
drawPeriod :: [Command]
drawPeriod = moveT 2 East ++ [penDown,penUp] ++ moveT 4 East
drawComma :: [Command]
drawComma = moveT 2 East ++ [penDown] ++ moveT 1 North ++ moveT 1 West
    ++ [penUp] ++ moveT 1 South ++ moveT 5 East
drawExclamation :: [Command]
drawExclamation = moveT 2 East ++ [penDown,penUp] ++ moveT 2 North
    ++ [penDown] ++ moveT 2 North ++ [penUp] ++ moveT 4 South ++ moveT 4 East
drawQuestion :: [Command]
drawQuestion = moveT 4 North ++ [penDown] ++ moveT 4 East ++ moveT 2 South ++ moveT 2 West
    ++ [penUp] ++ moveT 2 South ++ [penDown,penUp] ++ moveT 4 East
drawApost :: [Command]
drawApost = moveT 4 North ++ moveT 2 East ++ [penDown] ++ moveT 1 South ++ [penUp]
    ++ moveT 3 South ++ moveT 4 East

--my third and final interesting turtle trail
--returns commands to draw a spiral, with its int parameter determining
--the length of the largest side minus 1
spiral :: Int -> [Command]
spiral 0 = []
spiral n = [penDown] ++ moveN n ++ [left] ++ spiral (n-1)

