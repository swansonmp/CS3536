-- Swanson Interpreter Project
-- 2019.05.01

import Data.Char

-- Problems 1-9
type Variable = String
type Val = Int
data Expr = Const Val
          | Var Variable
          | Minus Expr Expr
          | Times Expr Expr
          | Greater Expr Expr
data Command = Assign Variable Expr
             | Seq Command Command
             | Cond Expr Command Command
             | While Expr Command
type Store = Variable -> Val

--initializes store to zero everything
initial :: Store
initial = \v -> 0
--fetches the Val from the Store you pass in with Variable you pass in
fetch :: Store -> Variable -> Val
fetch s v = s v
--updates the Variable in the old Store with that Val and returns a new Store
update :: Store -> Variable -> Val -> Store
update s v a x
    | x == v    = a
    | otherwise = s x

--Evaluates Exprs.
--Recursive calls for operations, pattern matching 
--  and 'fetch-ing' for Consts and Vars
eval :: Expr -> Store -> Val
eval (Const n)       s = n
eval (Var v)         s = fetch s v
eval (Minus e1 e2)   s = eval e1 s - eval e2 s
eval (Times e1 e2)   s = eval e1 s * eval e2 s
eval (Greater e1 e2) s = if eval e1 s > eval e2 s then 1 else 0

--Beef of the interpreter
--Reads Commands and changes the Store for each Command,
--  passing it along. This new score is our result
--While took me a while--(no) pun intended
interpret :: Command -> Store -> Store
interpret (Assign v e)   s = update s v (eval e s)
interpret (Seq c1 c2)    s = interpret c2 (interpret c1 s)
interpret (Cond e c1 c2) s = switch (eval e s) (interpret c1) (interpret c2) s
interpret (While e c)    s = switch (eval e s) (interpret (Seq c (While e c))) id s

--Performs one of two Store-altering functions depending on the Val passed in
--Used in the Cond and While Commands
switch :: Val -> (Store -> Store) -> (Store -> Store) -> Store -> Store
switch n f g s
    | n == 1    = f s
    | n == 0    = g s
    | otherwise = error "invalid value in switch"

--Poorly-written function that helps me test the program
--Initializes vars x,y,z, to the Vals passed in, returning a Store
initvars x y z = interpret (Seq (Assign "X" (Const x))
                    (Seq (Assign "Y" (Const y)) (Assign "Z" (Const z)))) 
                        initial

astex = (While (Greater (Var "X") (Var "Y")) 
            (Seq (Assign "X" (Minus (Var "X") (Const 1))) 
            (Assign "Z" (Times (Var "Z") (Var "Z")))))
ast1  = (Cond (Greater (Var "X") (Var "Z"))
            (Cond (Greater (Var "X") (Var "Y"))
                (Assign "X" (Const 0))
                (Assign "Y" (Const 0)))
            (Assign "Z" (Minus (Var "X") 
                    (Minus (Const 0) (Var "Y")))))
ast2 = (Seq (Assign "Z" (Var "X")) 
        (Cond (Greater (Const 1) (Var "Y"))
            (Assign "Z" (Const 1))
            (While (Greater (Var "Y") (Const 1))
                (Seq (Assign "Z" (Times (Var "Z") (Var "X")))
                    (Assign "Y" (Minus (Var "Y") (Const 1)))))))
ast3 = (Cond (Greater (Var "foo") (Const 0))
        (Seq (Assign "foobarbar" (Var "foo")) (Assign "foo" (Const 0)))
        (Assign "foo" (Const 1)))
ast4 = (Cond (Greater (Var "X") (Var "Y"))
        (While (Greater (Var "X") (Const 0)) (Assign "Z" (Times (Var "Z") (Var "X"))))
        (While (Greater (Var "Y") (Const 0)) (Assign "Y" (Times (Var "Z") (Var "Y")))))
ast5 = (Cond (Greater (Var "grade") (Const 90))
        (Assign "letter" (Const 65))
        (Cond (Greater (Var "grade") (Const 80))
            (Assign "letter" (Const 66))
            (Cond (Greater (Var "grade") (Const 70))
                (Assign "letter" (Const 67))
                (Cond (Greater (Var "grade") (Const 60))
                    (Assign "letter" (Const 68))
                    (Assign "letter" (Const 70))))))


-- Parser

data Token = Ident String
           | Number Val
           | Symbol String
type Parser a = [Token] -> Maybe (a,[Token])
--parser conbinators
-- <|> :: Parser a -> Parser a -> Parser a
(parser1 <|> parser2) s =
    let parser2IfNothing Nothing = parser2 s
        parser2IfNothing x       = x
    in
        parser2IfNothing (parser1 s)
-- modify :: Parser a -> (a -> b) -> Parser b
(parser `modify` f) s =
    let modResult Nothing      = Nothing
        modResult (Just (x,y)) = Just (f x,y)
    in
        modResult (parser s)
-- <&> :: Parser a -> Parser b -> Parser (a,b)
(parser1 <&> parser2) s =
    let parser2After Nothing      = Nothing
        parser2After (Just (x,s)) = (parser2 `modify` (\y -> (x,y))) s
    in
        parser2After (parser1 s)
-- emptyseq :: Parser a
emptyseq s = Just ([],s)
-- optional Parser a -> Parser [a]
optional pr = (pr `modify` (consonto [])) <|> emptyseq
                where consonto [] x = [x]

number :: Parser Val
number (Number n : s)   = Just (n,s)
number _                = Nothing
variable :: Parser Variable
variable (Ident v : s)  = Just (v,s)
variable _              = Nothing
literal  :: String -> Parser String
literal l (Symbol y : s) = if l==y then Just (y,s) else Nothing 
literal _ _              = Nothing

--top level expr-parsing function
expr :: Parser Expr
expr = (aexp <&> optional (literal ">" <&> aexp)) `modify` optGreater
        where optGreater (e1, [])       = e1
              optGreater (e1, [(_,e2)]) = Greater e1 e2
              optGreater _              = error "impossible"
aexp :: Parser Expr
aexp = (bexp <&> optional (literal "-" <&> aexp)) `modify` optSub
        where optSub (e1, [])       = e1
              optSub (e1, [(_,e2)]) = Minus e1 e2
              optSub _              = error "impossible"
bexp :: Parser Expr
bexp = (cexp <&> optional (literal "*" <&> bexp)) `modify` optMult
        where optMult (e1, [])       = e1
              optMult (e1, [(_,e2)]) = Times e1 e2
              optMult _              = error "impossible"
cexp :: Parser Expr
cexp = ((literal "(" <&> expr <&> literal ")") `modify` unparenth) <|> (number `modify` Const) <|> (variable `modify` Var)
        where unparenth ((_,e),_) = e

--Big boy that takes a token list. Pg 10's grammar put into code; which goes for most of the rest of the parser functions
command  = (unitcom <&> optional (literal ";" <&> command)) `modify` optSeq
            where optSeq (c1,[])       = c1
                  optSeq (c1,[(_,c2)]) = Seq c1 c2
                  optSeq _             = error "impossible"
unitcom  = whilecom <|> (ifcom <|> assign)
whilecom = (literal "WHILE" <&> expr <&> literal "DO" <&> command <&> literal "END") `modify` mkWhileNode
            where mkWhileNode ((((_,e),_),c),_) = While e c
ifcom    = (literal "IF" <&> expr <&> literal "THEN" <&> command <&> literal "ELSE" <&> command <&> literal "ENDIF") `modify` mkIfNode
            where mkIfNode ((((((_,e),_),c1),_),c2),_) = Cond e c1 c2
assign   = (variable <&> literal ":=" <&> expr) `modify` mkAssignNode
            where mkAssignNode ((v,_),e) = Assign v e


lit (Ident s)  = s ++ " "
lit (Symbol s) = s ++ " "
lit (Number s) = show s ++ " "
--Error reporting funtion
--If we get a Parser with Nothing, we know something went totally wrong
--If we get a Parser with a command list and no remaining tokens, then we're set
--If we get a Parser with a command list and remaining tokens, that means there's
--  Tokens left to parse, which means that something is wrong
report Nothing      = error "Parse error"
report (Just(c,[])) = c
report (Just(c,xs)) = error (stringwith ("Syntax error \n Unparsed:-\n", " ", "\n") (map lit xs))
                        where stringwith (front, sep, back) ls =
                                let sepback []     = back
                                    sepback [a]    = a ++ back
                                    sepback (a:xs) = a ++ sep ++ sepback xs
                                in  front ++ sepback ls

--'main' function for the parser.
--command is run, and then report makes sure that nothing went awry
mainParser :: [Token] -> Command
mainParser = report . command

--Test Token Lists
tokenex = [Symbol "WHILE", Ident "X", Symbol ">", Ident "Y", Symbol "DO", 
            Ident "X", Symbol ":=", Ident "X", Symbol "-", Number 1, Symbol ";", 
            Ident "Z", Symbol ":=", Ident "Z", Symbol "*", Ident "Z", Symbol "END"]
token1 = [Symbol "IF", Ident "X", Symbol ">", Ident "Z", 
            Symbol "THEN", Symbol "IF", Ident "X", Symbol ">", Ident "Y", Symbol "THEN", 
            Ident "X", Symbol ":=", Number 0, Symbol "ELSE",
            Ident "Y", Symbol ":=", Number 0, Symbol "ENDIF", 
            Symbol "ELSE", Ident "Z", Symbol ":=", Ident "X", Symbol "-", Number 0, Symbol "-", Ident "Y", Symbol "ENDIF"]
token2 = [Ident "Z", Symbol ":=", Number 1, Symbol ";",
            Symbol "IF", Number 1, Symbol ">", Ident "Y", Symbol "THEN",
            Ident "Z", Symbol ":=", Number 1, Symbol "ELSE",
            Symbol "WHILE", Ident "Y", Symbol ">", Number 0, 
            Symbol "DO", Ident "Z", Symbol ":=", Ident "Z", Symbol "*", Ident "X", Symbol ";", 
            Ident "Y", Symbol ":=", Ident "Y", Symbol "-", Number 1, Symbol "END", Symbol "ENDIF"]
token3 = [Symbol "WHILE", Ident "X", Symbol ">", Number 0, 
            Symbol "DO", Ident "TY", Symbol ":=", Ident "Y", Symbol ";", 
            Symbol "WHILE", Ident "TY", Symbol ">", Number 0, 
            Symbol "DO", Ident "Z", Symbol ":=", Ident "Z", Symbol "-", Number 0, Symbol "-", Ident "Z", Symbol ";", 
            Ident "TY", Symbol ":=", Ident "TY", Symbol "-", Number 1, Symbol "END", Symbol ";", 
            Ident "Z", Symbol ":=", Ident "Z", Symbol "*", Ident "Z", Symbol ";", 
            Ident "X", Symbol ":=", Ident "X", Symbol "-", Number 1, Symbol "END"]
token4 = [Ident "Z", Symbol ":=", Ident "X", Symbol ";", 
            Symbol "WHILE", Ident "Z", Symbol ">", Number 0, 
            Symbol "DO", Ident "Z", Symbol ":=", Ident "Z", Symbol "-", Number 2, Symbol "END", Symbol ";", 
            Symbol "IF", Ident "Z", Symbol ">", Number 0, Symbol "-", Number 1, 
            Symbol "THEN", Ident "Z", Symbol ":=", Number 0, 
            Symbol "ELSE", Ident "Z", Symbol ":=", Number 1, Symbol "ENDIF"]
token5 = [Ident "Z", Symbol ":=", Ident "X", Symbol ";", 
            Symbol "WHILE", Ident "Z", Symbol ">", Number 0, 
            Symbol "DO", Ident "Z", Symbol ":=", Ident "Z", Symbol "-", Number 1, Symbol ";", 
            Symbol "IF", Ident "X", Symbol ">", Ident "Z", Symbol "*", Number 2, Symbol "-", Number 1, 
            Symbol "THEN", Symbol "IF", Ident "Z", Symbol "*", Number 2, Symbol ">", Ident "X", Symbol "-", Number 1 ,
            Symbol "THEN", Ident "Y", Symbol ":=", Ident "Z", Symbol ";", 
            Ident "Z", Symbol ":=", Number 0, 
            Symbol "ELSE", Ident "Y", Symbol ":=", Ident "Y", Symbol "ENDIF", 
            Symbol "ELSE", Ident "Y", Symbol ":=", Ident "Y", Symbol "ENDIF", Symbol "END"]


-- Lexer

--Takes a String, and returns whether or not it's a keyword
keyword :: String -> Bool
keyword s = or [s==k | k <- ["IF","THEN","ELSE","ENDIF","WHILE","DO","END"]]
---Takes a String, and returns a corresp Token
--  If keyword passes, we know it's a Symbol
--  Else, we know it's an Ident
keycheck :: String -> Token
keycheck s = if keyword s then Symbol s else Ident s

--Takes a Char, returns whether or not it's a letter, digit, or 'etc'
letdigetc :: Char -> Bool
letdigetc c = isLetter c || isDigit c || c=='\'' || c=='_'
--Takes a Char, returns whether or not it's a layout character
layout :: Char -> Bool
layout c = or [c==k | k <- " \n\t"]
--Takes a Char, returns whether or not it's a symbol character
symbolchar :: Char -> Bool
symbolchar c = or [c==k | k <- "*->:=;"]

--Beef of the lexer
--Calls various functions to handle the character we're currently reading
lexer :: String -> [Token]
lexer [] = []
lexer (a:x) 
    | layout a      = lexer x
    | a == '('      = Symbol "(" : (lexer x)
    | a == ')'      = Symbol ")" : (lexer x)
    | isLetter a    = getword [a] x
    | isDigit a     = getnum (intOfDigit a) x
    | symbolchar a  = getsymbol [a] x
    | otherwise     = error ("Lexical error : unrecognized token " ++ (a:x))

--Gets a 'word', which can either be a Symbol or Ident
getword :: String -> String -> [Token]
getword l [] = [keycheck (reverse l)]
getword l (a:x) = if letdigetc a then getword (a:l) x
                    else (keycheck (reverse l)) : (lexer (a:x))
--Gets a symbol. The logic here is similar to the other 3, where we read the String in reverse.
--As long as we read valid Chars, recursively accumulate an accumulator.
--When we reach an invalid Char or EOF; flip the String, and create a token out of it
getsymbol :: String -> String -> [Token]
getsymbol l [] = [Symbol (reverse l)]
getsymbol l (a:x) = if symbolchar a then getsymbol (a:l) x
                    else (Symbol (reverse l)) : (lexer (a:x))
--Neat little function that gets a number.
--I was stuck on this one for too long; had only done this problem the other way around before.
--I actually think that doing it this way is easier, as you don't need another var to track your place
getnum :: Int -> String -> [Token]
getnum n []    = [Number n]
getnum n (a:x) = if isDigit a then getnum (n*10+(intOfDigit a)) x
                    else (Number n) : (lexer (a:x))

--goober code
intOfDigit :: Char -> Int
intOfDigit c
    | c=='0' = 0
    | c=='1' = 1
    | c=='2' = 2
    | c=='3' = 3
    | c=='4' = 4
    | c=='5' = 5
    | c=='6' = 6
    | c=='7' = 7
    | c=='8' = 8
    | c=='9' = 9

--'main' function. Takes a program String and returns a result Store,
--passing input 'through the pipeline'
run :: String -> Store -> Store
run str s = interpret (mainParser (lexer str)) s

programdeif = "IF X>Y THEN X:=0 ELSE Y:=0 ENDIF"
programdewhile = "WHILE X>Y DO X:=X-1 END"
programdeseq = "X:=1; Y:=2; Z:=3"
--Test programs
--The assignment example
programex = "WHILE X > Y DO X := X - 1; Z := Z * Z END"
--rando; utilizes nested if statemnets
program1 = "IF X>Z THEN IF X>Y THEN X:=0 ELSE Y:=0 ENDIF ELSE Z:=X-0-Y ENDIF"
--pow function; ret X^Y = Z
program2 = "Z:=1; IF 1>Y THEN Z:=1 ELSE WHILE Y>0 DO Z:=Z*X; Y:=Y-1 END ENDIF"
--rando; utilizes nestes whiles
program3 = "WHILE X>0 DO TY:=Y; WHILE TY>0 DO Z:=Z-0-Z; TY:=TY-1 END; Z:=Z*Z; X:=X-1 END"
--even/odd function. ret 0 if X is even and ret 1 if X is odd
program4 = "Z:=X; WHILE Z>0 DO Z:=Z-2 END; IF Z>0-1 THEN Z:=0 ELSE Z:=1 ENDIF"
--div by 2: x is param, y is ret, z is temp
program5 = "Z:=X; WHILE Z>0 DO Z:=Z-1; IF X>Z*2-1 THEN IF Z*2>X-1 THEN Y:=Z; Z:=0 ELSE Y:=Y ENDIF ELSE Y:=Y ENDIF END"
--collatz
--was planning on writing a program to calc length of the collatz sequence of x but ran out of time
--would've utilized the even/odd function and the div by 2 function
--program6 = "Y := 1; WHILE X > 1 DO Z := X; " ++ program4 ++ "IF ENDIF END"

