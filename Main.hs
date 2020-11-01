{-# LANGUAGE QuasiQuotes #-}

module Main
   where

import Data.List hiding (insert)
import System.Environment
import Text.Printf
import Text.Read hiding (step)
import Text.RawString.QQ
import Network.CGI
import qualified Task as T (length, sum)
import Task hiding (length, sum)
import Debug.Trace

------- EXERCISES EXPRESSIONS

expr1 = squares (cons (Con 3) (cons (square (Con 2)) nil))
expr2 = add (square (add (square (Con 3)) (square (Con 5)))) (Con 4)
expr3 = cons (square (Con 3)) (squares (cons (Con 3) (cons (Con 9) nil)))
expr4 = squares (cons (Con 3) (cons (square (square(Con 2))) (cons (Con 4) nil)))
expr5 = squares (cons (Con 3) (cons (Con 5) (cons (Con 7) (cons (Con 3) nil))))
expr6 = squares (cons (Con 3) (cons (Con 9) nil))
expr7 = squares (cons (add (Con 3) (add (Con 0) (Con 3))) (cons (add (add (Con 2) (Con 1)) (Con 3)) nil))
expr8 = fib (Con 5)
expr9 = add (fib (square (Con 2))) (fib (Con 2))
expr10 = mult (fib (square (Con 2))) (fib (Con 2))
expr12 = App (Var "f") (Con 8)
expr13 = App (Var "fac") (Con 5)
expr14 = add (Con 3) (Con 21)
expr15 = cons (square (Con 3)) (cons (square (Con 7)) (cons (Con 7) (cons (Con 3) nil)))
expr16 =
   squares 
      (cons
         (Con 7)
         (cons
            (Con 5)
            nil
         )
      )

expr17 = T.length (cons (Con 3) (cons (Con 5) (cons (Con 7) (cons (Con 3) nil))))
expr18 = T.sum (cons (Con 3) (cons (Con 5) (cons (Con 7) (cons (Con 3) nil))))

instr =  [ (expr17, lengthInstr)
         , (expr18, sumListInstr)
         , (expr5, squaresInstr)
         , (expr8, fibInstr)
         , (expr12, fInstr)
         , (expr13, facInstr)
         ]

len = length instr

------- INSTRUCTIONS

lengthInstr = frmt lengthStr
sumListInstr = frmt sumlistStr
squaresInstr = frmt squaresStr
facInstr = frmt facStr
fibInstr = frmt fibStr
fInstr = frmt fStr

frmt fnsStr = bStr ++ fnsStr ++ eStr

squaresStr = [r|
    square x = x * x

    squares [] = []
    squares (x:xs) = square x : squares xs
|]

facStr = [r|
    fac 1 = 1
    fac n = n * fac (n-1)
|]

fibStr = [r|
    fib 0 = 0
    fib 1 = 1
    fib n = fib (n-1) + fib (n-2)
|]

fStr = [r|
    f 1 = 1
    f n = 4 * f (n `div` 2) + 3
|]

lengthStr = [r|
    length []     = 0
    length (x:xs) = 1 + length xs
|]

sumlistStr = [r|
    sum []     = 0
    sum (x:xs) = x + sum xs
|]

bStr = [r|
    <p>Given are following function definitions:</p>
    <pre><code>
|]

eStr = [r|
    </code></pre>
    <p>View the rewrite steps below:</p>
|]

wrap :: String -> String -> String -> String -> String -> String -> String
wrap dropdown instr stepbuttons counter steps tables = [r|
   <!doctype html>
   <html>
   <head>
      <title>Haskell Recursion Tutor</title>
      <link rel='stylesheet' href='/css/haskell-recursion.css'>
   </head>
   <body>
      <div class="card flex">
         <h1>Haskell Recursion Tutor</h1>
         <p>Choose an exercise:</p>
         <select id="dd">|] ++ dropdown ++ [r|</select>
      </div>
      <script>
      document
            .getElementById("dd")
            .addEventListener("change", function() {
                window.location.href = this.value;
            })
      </script>
      <div class=card>
         <div id='instructions'>|] ++ instr ++ [r|</div>
         <div id='stepbuttons'>|] ++ stepbuttons ++ [r|</div>
         <div id='counter'>|] ++ counter ++ [r|</div>
         <div id='steps-tables'>
            <div id='exercise'>
               <div id='steps'>|] ++ steps ++ [r|</div>
            </div>
            <div id='tables'>|] ++ tables ++ [r| </div>
         </div>
      </div>
   </body>
</html>
|]

toHTML :: Int -> Maybe Path -> Maybe Path -> Expr -> String
toHTML n p1 p2 expr | Just ys <- collectList expr = 
   "[" ++ intercalate ", " (map (toHTML 10 p1 p2) ys) ++ "]"
toHTML n p1 p2 expr =
   case collect expr of
      (Con n, [])   -> "<span" ++ tag (p1,p2) ++ ">" ++ show n  ++ "</span>" 
      (Var "cons", [x, xs]) -> parIf (n > 5) $ "<span" ++ tag (p1,p2) ++ ">" ++ toHTML 6 (goRight $ goLeft p1) (goRight $ goLeft p2) x  ++ " : " ++ toHTML 5 (goRight p1) (goRight p2) xs ++ "</span>" 
      (Var "+", [x, y])     -> parIf (n > 6) $ "<span" ++ tag (p1,p2) ++ ">" ++ toHTML 6 (goRight $ goLeft p1) (goRight $ goLeft p2) x  ++ " + " ++ toHTML 7 (goRight p1) (goRight p2) y ++ "</span>" 
      (Var "*", [x, y])     -> parIf (n > 7) $ "<span" ++ tag (p1,p2) ++ ">" ++ toHTML 7 (goRight $ goLeft p1) (goRight $ goLeft p2) x  ++ " * " ++ toHTML 8 (goRight p1) (goRight p2) y ++ "</span>" 
      (Var f, args)         -> parIf (n >= 10 && not (null args)) $ "<span" ++ tag (p1,p2) ++ ">" ++ unwords (f : map (toHTML 10 (goRight p1) (goRight p2)) args) ++ "</span>"
      _ -> error "toHTML: invalid expression"

tag :: (Maybe Path, Maybe Path) -> String
tag (p1, p2) =
   case (p1, p2) of
      (Just [], Just []) -> " data-source data-target"
      (_, Just []) -> " data-target"
      (Just [], _) -> " data-source"
      (_, _) -> ""

goLeft :: Maybe Path -> Maybe Path
goLeft (Just (L:p)) = Just p
goLeft _ = Nothing

goRight :: Maybe Path -> Maybe Path
goRight (Just (R:p)) = Just p
goRight _ = Nothing

htmlStrings :: Expr -> [String]
htmlStrings expr = ["<div>" ++ toHTML 0 (Just p1) (Just p2) e ++ "</div>" | (e,p1,p2) <- map (\((e,p1),p2) -> (e,p1,p2)) (zip (stepsSub expr) ([[]] ++ (map snd (stepsSub expr))))]

htmlStringsN :: Expr -> Int -> [String]
htmlStringsN expr n = ["<div>" ++ toHTML 0 (Just p1) (Just p2) e ++ "</div>" | (e,p1,p2) <- map (\((e,p1),p2) -> (e,p1,p2)) (zip (stepsSubN expr n) ([[]] ++ (map snd (stepsSubN expr n))))]

stepsHtml e = foldr (++) "" (htmlStrings e)
stepsHtmlN e n = foldr (++) "" (htmlStringsN e n)


inOutStep :: Expr -> Maybe (String, [Expr])
inOutStep e =
   case (e, step e) of
      (e, Just (e', p)) -> Just (fname $ subexpr p e, [subexpr p e, subexpr p e']) -- [getParam1 $ subexpr p e, getParam2 $ subexpr p e, subexpr p e']
      _                 -> Nothing
               
   where      
      getParam1 :: Expr -> Expr
      getParam1 (App (App f a) b) = a
      getParam1 (App f a) = a
      getParam1 _ = Var "null"

      getParam2 :: Expr -> Expr
      getParam2 (App (App f a) b) = b
      getParam2 _ = Var "null"

      fname :: Expr -> String
      fname (App f a) = case f of
         (App f' a') -> case f' of 
            (Var "+") -> "add"
            (Var "*") -> "multiply"
            _         -> show f'
         _           -> show f

inOutSteps :: Expr -> Int -> [(String, [Expr])]
inOutSteps e n
   | n <= 0    = []
   | otherwise = case (inOutStep e) of
      Just x -> inOutSteps (step' e) (n-1) ++ [x]
      Nothing -> []

inOutStepsAll :: Expr -> [(String, [Expr])]
inOutStepsAll e =
   case (inOutStep e) of
      Just x -> inOutStepsAll (step' e) ++ [x]
      Nothing -> []

type Table = (String, [[Expr]])

{--
exprToTables :: Expr -> Int -> [Table]
exprToTables e n = exprToTables' e n [] 

exprToTables' :: Expr -> Int -> [Table] -> [Table]
exprToTables' e n ts
   | n <= 0    = []
   | otherwise = case (inOutStep e) of
      Just x -> insert x (exprToTables' (step' e) (n-1) ts)
      Nothing -> exprToTables' e (n-1) ts
--}

exprToTables :: Expr -> Int -> [Table]
exprToTables e n = reverseRows $ exprToTables' (inOutSteps e n) []

exprToTablesAll :: Expr -> [Table]
exprToTablesAll e = reverseRows $ exprToTables' (inOutStepsAll e) []

exprToTables' :: [(String, [Expr])] -> [Table] -> [Table]
exprToTables' (r:rows) ts
   | rows == [] = insert r ts
   | otherwise  = insert r (exprToTables' rows ts)

reverseRows :: [Table] -> [Table]
reverseRows [] = []
reverseRows ((s,rows):ts) = (s, reverse rows):reverseRows ts

insert :: (String, [Expr]) -> [Table] -> [Table]
insert (s,exprs) [] = [(s, [exprs])]
insert (s,exprs) (t:ts)
   | s == fst t = if exprs `elem` snd t then (t:ts)
                  else (s, exprs:snd t):ts
   | otherwise  = t : (insert (s,exprs) ts)

rowsToNormalize :: [Table] -> Int
rowsToNormalize []     = 0
rowsToNormalize (t:ts) = rowsToNormalize' t + rowsToNormalize ts
   where
      rowsToNormalize' (_, [])   = 0
      rowsToNormalize' (s, (exprs:rows)) = (eToNormalize exprs) + (rowsToNormalize' (s,rows))

      eToNormalize exprs
         | (eval $ last exprs) == (last exprs) = 0
         | otherwise                           = 1

normalizeStep :: [Table] -> [Table]
normalizeStep [] = []
normalizeStep ((s,rows):ts) 
   | tableNormalized (s, rows) = (s,rows) : normalizeStep ts
   | otherwise                 = (s, normalizeLastRow rows) : normalizeStep ts
   
   where
      normalizeLastRow rows
         | rowNormalized (last rows) = normalizeLastRow (init rows) ++ [(last rows)]
         | otherwise                 = (init rows) ++ [(evalLast $ last rows)]

      evalLast exprs = (init exprs) ++ [(eval $ last exprs)]

normalizeRowPos :: Table -> Int
normalizeRowPos (_, []) = -1
normalizeRowPos t@(s, rows)
   | tableNormalized t = -1
   | otherwise         = sum $ map fromEnum [ rowNormalized row | row <- rows ]

normalizeTablePos :: [Table] -> Int
normalizeTablePos [] = -1
normalizeTablePos (t:ts)
   | ts == []          = if tableNormalized t then -1 else 0
   | tableNormalized t = 1 + normalizeTablePos ts
   | otherwise         = 0

normalizePos :: [Table] -> [Int]
normalizePos = map normalizeRowPos

rowNormalized row = last row == (eval $ last row)

tableNormalized :: Table -> Bool
tableNormalized t = rowsToNormalize [t] == 0


-- TODO: merge functions outTables' and outTables''

outTables' :: [Table] -> String
outTables' [] = ""
outTables' tbls@(t:ts) = outTable t ++ outTables' ts
   where
      outTable ("", _) = ""
      outTable (s, exprs) = "<table><thead><th colspan=10>"++s++"</th></thead>\n" ++ outRow exprs ++ "</table>"

      outRow [] = ""
      outRow (elist:elists) = "<tr>" ++ td elist ++ "</tr>\n" ++ outRow elists

      td es = concat $ map (\e -> "<td>" ++ show e ++ "</td>") es

outTables'' :: [Table] -> Int -> Int -> String
outTables'' [] _ _ = ""
outTables'' tbls@(t:ts) n m
   | n == 0    = outTable t m ++ outTables'' ts n m
   | otherwise = outTables'' (normalizeStep tbls) (n-1) m
   where
      outTable ("", _) m = ""
      outTable t@(s, exprs) m = "<table><thead><th colspan=10>"++s++"</th></thead>\n" ++ outRow exprs (rowsToNormalize [t]) ++ "</table>"
      
      outRow [] _ = ""
      outRow (elist:elists) m = if (m<=0) then "<tr class=rewrite>" ++ td elist ++ "</tr>\n" ++ outRow elists (m-1)
                                else "<tr>" ++ td elist ++ "</tr>\n" ++ outRow elists (m-1)
      
      td es = concat $ map (\e -> "<td>" ++ show e ++ "</td>") es

outTables :: Expr -> Int -> String
outTables e n
   | n <= 0                  = ""
   | n >= (length $ steps e) = outTables'' (exprToTables e n) m m
   | otherwise               = outTables'  (exprToTables e n)
      where m = (n - (length $ steps e)+1)


outStepButtons :: String -> Int -> Int -> Int -> String
outStepButtons scriptName page step maxStep
   | step<=1       = "<a>&larr;</a><a href='" ++ scriptName ++ "?page=" ++ show page ++ "&step=2'>&rarr;</a>"
   | step>=maxStep = "<a href='" ++ scriptName ++ "?page=" ++ show page ++ "&step=" ++ show (step-1) ++ "'>&larr;</a><a>&rarr;</a>"
   | otherwise     = "<a href='" ++ scriptName ++ "?page=" ++ show page ++ "&step=" ++ show (step-1) ++ "'>&larr;</a><a href='" ++ scriptName ++ "?page=" ++ show page ++ "&step=" ++ show (step+1) ++ "'>&rarr;</a>"

dropdown :: Int -> String
dropdown page = unwords ["<option " ++ (if(i==page) then "selected " else "") ++"value='tutor?page=" ++ show i ++ "'>" ++ (show $ fst $ instr !! (i-1)) ++ " </option>" | i <- [1.. len]]

counter :: Int -> Int -> Int -> String
counter current totRewriteSteps totNormSteps = "<div>" ++ show current ++ "/" ++ show (totRewriteSteps + totNormSteps) ++ "</div>" ++ msg
   where
      msg
         | current > totRewriteSteps = "<div class='message'>Evaluation done. Green arrows indicate data passed back through the passive flow.</div>"
         | otherwise                 = ""

cgiMain = do pStr <- getInput "page"
             sStr <- getInput "step"
             scriptN <- scriptName
             let pageMaybe = case pStr of
                           Just _ -> readMaybe ((\(Just s) -> s) pStr) :: Maybe Int
                           Nothing -> Nothing 
                 pageNr = case pageMaybe of
                        Just n -> n
                        Nothing -> 1
                 pageToShow = max (min pageNr len) 1
                 expr = fst $ instr !! (pageNr-1)
                 instructions = snd $ instr !! (pageNr-1)

                 stepNr = case sStr of
                     Just _ -> readMaybe ((\(Just s) -> s) sStr) :: Maybe Int
                     Nothing -> Nothing

                 stepToShow = case stepNr of
                     Just n -> if n>=1 then n else 1
                     Nothing -> 1
                  
             output $ wrap
                        (dropdown pageToShow)
                        instructions
                        (outStepButtons scriptN pageToShow stepToShow ((length $ steps expr) + (rowsToNormalize $ exprToTablesAll expr)))
                        (counter stepToShow (length $ steps expr) (rowsToNormalize $ exprToTablesAll expr))
                        (stepsHtmlN expr (stepToShow))
                        (outTables expr (stepToShow-1))

                        --

main = runCGI $ handleErrors cgiMain



{--

normalizeTables :: [Table] -> Int -> [Table]
normalizeTables [] _ = []

normalizeTables (t:ts) n
   | n <= 0    = []
   | otherwise = normalizeTable t : normalizeTables ts (n-1)
      where
         normalizeTable (s, rs) = (s, normalizeRows rs)

         normalizeRows [] = []
         normalizeRows (elist:elists) = map eval elist : normalizeRows elists

normalizeTables' :: [Table'] -> Int -> [Table']
normalizeTables' [] = []
normalizeTables' (t:ts) = normalizeTable t : normalizeTables' ts
   where
      normalizeTable (s, rs) = (s, normalizeRows rs)

      normalizeRows [] = []
      normalizeRows (elist:elists) = map eval elist : normalizeRows elists

[
   (
      "squares",
      [
         (squares [7, 5],square 7 : squares [5],null),
         (squares [5],square 5 : squares [],null),
         (squares [],[],null)
      ]
   ),
   (
      "square",
      [
         (square 7,49,null),
         (square 5,25,null)
      ]
   )
]

inOutStep :: Expr -> [String]
inOutStep e = case (e, step e) of
   (_, Nothing) -> [] -- end rewriting
   (e, Just (e', p)) -> case (isBin $  subexpr p e) of
      False -> [fname $  subexpr p e, getParam1 $  subexpr p e, show $  subexpr p e']
      True -> [fname $  subexpr p e, getParam1 $  subexpr p e, getParam2 $  subexpr p e, show $  subexpr p e']
               
   where      
      getParam1 :: Expr -> String
      getParam1 (App (App f a) b) = show a
      getParam1 (App f a) = show a
      getParam1 _ = ""

      getParam2 :: Expr -> String
      getParam2 (App (App f a) b) = show b
      getParam2 _ = ""

inOutSteps :: Expr -> [[String]]
inOutSteps e = map inOutStep $ steps e 

subStep :: Expr -> (String, Expr, Expr)
subStep e =
   case step e of
      Just (e', p) -> (fname $ subexpr p e,  subexpr p e,  subexpr p e')
      _            -> ("", e, e)

insert' :: [String] -> [Table] -> [Table]
insert' s [] = [(s, [(x, y)])]
insert' s x y (t:ts)
   | s == fst t = if (x,y) `elem` snd t then [(s, [(x, y)])]
                  else (s, (x,y):snd t) : ts
   | otherwise  = t:insert' s x y ts

stepsToHTML :: Int -> [[String]] -> String
stepsToHTML _ [[]] = ""
stepsToHTML 0 _ = ""
stepsToHTML m steps =
   case steps of
      ((n:i:o:[]):xs)     -> (printf "<div>%s - %s - %s</div>\n" n i o) ++ stepsToHTML (m-1) xs
      ((n:i1:i2:o:[]):xs) -> (printf "<div>%s - %s - %s - %s</div>\n" n i1 i2 o) ++ stepsToHTML (m-1) xs

stepsToTables' :: Int -> [Expr] -> [Table] -> [Table]
stepsToTables' 0 _  _          = []
stepsToTables' _ [] _          = []
stepsToTables' n (x:xs) ts = insert (fname x) x (step' x) (stepsToTables' (n-1) xs ts)

outSteps :: Expr -> Int -> String
outSteps e 0 = ""
outSteps e n = 
   if (n > (length $ steps e)) then outSteps e (length $ steps e)
   else "<div>" ++ show e ++ "</div>\n" ++ outSteps (step' e) (n-1)

stepsToJSON :: [[String]] -> String
stepsToJSON [[]] = ""
stepsToJSON ((n:i:o:[]):xs) = (printf "{ name: '%s', input: '%s', output: '%s' }" n i o) ++ if xs == [[]] then "\n" else ",\n" ++ stepsToJSON xs
stepsToJSON ((n:i1:i2:o:[]):xs) = (printf "{ name: '%s', input1: '%s', input2: '%s', output: '%s' }" n i1 i2 o) ++ if xs == [] then "\n" else ",\n" ++ stepsToJSON xs

exprToJSON e = "[\n" ++ (stepsToJSON $ inOutSteps e) ++ "]"

render :: Int -> IO ()
render i =
   let i' = i `mod` length instr
       expr = fst $ instr !! i'
       instructions = snd $ instr !! i'
       in
          do writeFile "front-end/index.html" $ wrap instructions (stepsHtml expr) (exprToJSON expr)

inOutStep :: Expr -> (String, Expr, Expr, Expr)
inOutStep e =
   case (e, step e) of
      (_, Nothing) -> ("", Var "null", Var "null", Var "null") -- end rewriting
      (e, Just (e', p)) -> case (isBin $ subexpr p e) of
         False -> (fname $ subexpr p e, subexpr p e, subexpr p e', Var "null")
         True -> (fname $ subexpr p e, getParam1 $ subexpr p e, getParam2 $ subexpr p e, subexpr p e')
               
   where      
      getParam1 :: Expr -> Expr
      getParam1 (App (App f a) b) = a
      getParam1 (App f a) = a
      getParam1 _ = Var "null"

      getParam2 :: Expr -> Expr
      getParam2 (App (App f a) b) = b
      getParam2 _ = Var "null"

      fname :: Expr -> String
      fname (App f a) = case f of
         (App f' a') -> case f' of 
            (Var "+") -> "plus"
            (Var "*") -> "multiply"
            _         -> show f'
         _           -> show f


--}