{-# LANGUAGE QuasiQuotes #-}

module Main
   where

import System.Environment
import Text.Printf
import Text.Read hiding (step)
import Text.RawString.QQ
import Network.CGI
import Task
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
expr11 = square (mult (fib (mult (Con 2) (Con 2))) (fib (Con 2)))
expr12 = App (Var "f") (Con 8)
expr13 = App (Var "fac") (Con 5)
expr14 = add (Con 3) (Con 21)

instr =  [ (expr1, squaresInstr)
         , (expr2, squaresInstr)
         , (expr3, squaresInstr)
         , (expr4, squaresInstr)
         , (expr5, squaresInstr)
         , (expr6, squaresInstr)
         , (expr7, squaresInstr)
         , (expr8, fibInstr)
         ]

len = length instr

------- INSTRUCTIONS

squaresInstr = frmt squaresStr
facInstr = frmt facStr
fibInstr = frmt fibStr

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

bStr = [r|
    <p>Given are following function definitions:</p>
    <pre><code>
|]

eStr = [r|
    </code></pre>
    <p>View the rewrite steps below:</p>
|]

wrap :: String -> String -> String -> String -> String -> String
wrap dropdown instr stepbuttons steps tables = [r|
   <!doctype html>
   <html>
   <head>
      <title>Haskell Recursion Tutor</title>
      <link rel='stylesheet' href='/css/haskell-recursion.css'>
   </head>
   <body>
      <h1>Recursion Steps in Haskell</h1>
      <select id="dd">|] ++ dropdown ++ [r|</select>
      <script>
      document
            .getElementById("dd")
            .addEventListener("change", function() {
                window.location.href = this.value;
            })
      </script>
      <div id='instructions'>|] ++ instr ++ [r|</div>
      <div id='stepbuttons'>|] ++ stepbuttons ++ [r|</div>
      <div id='steps-tables'>
         <div id='exercise'>
            <div id='steps'>|] ++ steps ++ [r|</div>
         </div>
         <div id='tables'>|] ++ tables ++ [r| </div>
      </div>
   </body>
</html>
|]

toHTML :: Int -> Maybe Path -> Maybe Path -> Expr -> String
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

inOutStep :: Expr -> (String, Expr, Expr, Expr)
inOutStep e = case (e, step e) of
   (_, Nothing) -> ("", Var "null", Var "null", Var "null") -- end rewriting
   (e, Just (e', p)) -> case (isBin $  subexpr p e) of
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

type Table' = (String, [(Expr, Expr, Expr)])

insert :: (String, Expr, Expr, Expr) -> [Table'] -> [Table']
insert (s,x,y,z) [] = [(s, [(x, y, z)])]
insert (s,x,y,z) (t:ts)
   | s == fst t = if (x,y,z) `elem` snd t then (t:ts)
                  else (s, (x,y,z):snd t):ts
   | otherwise  = (insert (s,x,y,z) ts) ++ [t]

exprToTablesN :: Expr -> Int -> [Table']
exprToTablesN e n = exprToTablesN' e n [] 
   where
      exprToTablesN' :: Expr -> Int -> [Table'] -> [Table']
      exprToTablesN' _ 0 _  = []
      exprToTablesN' e n ts = insert (inOutStep e) (exprToTablesN' (step' e) (n-1) ts)



normalizeTables :: [Table'] -> [Table']
normalizeTables [] = []
normalizeTables (t:ts) = normalizeTable t : normalizeTables ts
   where
      normalizeTable (s, rs) = (s, normalizeRows rs)
         where
            normalizeRows [] = []
            normalizeRows ((e1, e2, e3):rs)
               | e3 == Var "null" = (e1, eval e2, e3) : normalizeRows rs
               | otherwise        = (e1, eval e2, eval e3) : normalizeRows rs

outTables' :: [Table'] -> String
outTables' [] = ""
outTables' (t:ts) = outTable t ++ outTables' ts
   where
      outTable ("", _) = ""
      outTable (s, es) = "<table><thead><th colspan=2>"++s++"</th></thead>\n" ++ outRow es ++"</table>"

      outRow [] = ""
      outRow ((e1,e2,Var "null"):es) = "<tr><td>" ++ show e1 ++ "</td><td>" ++ show e2 ++ "</td></tr>\n" ++ outRow es
      outRow ((e1,e2,e3):es) = "<tr><td>" ++ show e1 ++ "</td><td>" ++ show e2 ++ "</td><td>" ++ show e3 ++ "</td></tr>\n" ++ outRow es

outTablesN :: Expr -> Int -> String
outTablesN e n = outTables' $ exprToTablesN e n

outStepButtons :: Int -> Int -> Int -> String
outStepButtons page step maxStep
   | step<=1       = "<a>&larr;</a><a href='tutor?page=" ++ show page ++ "&step=2'>&rarr;</a>"
   | step>=maxStep = "<a href='tutor?page=" ++ show page ++ "&step=" ++ show (step-1) ++ "'>&larr;</a><a>&rarr;</a>"
   | otherwise     = "<a href='tutor?page=" ++ show page ++ "&step=" ++ show (step-1) ++ "'>&larr;</a><a href='tutor?page=" ++ show page ++ "&step=" ++ show (step+1) ++ "'>&rarr;</a>"

dropdown :: Int -> String
dropdown page = unwords ["<option " ++ (if(i==page) then "selected " else "") ++"value='tutor?page=" ++ show i ++ "'>" ++ (show $ fst $ instr !! (i-1)) ++ " </option>" | i <- [1.. len]]

cgiMain = do pStr <- getInput "page"
             sStr <- getInput "step"
             let p = case pStr of
                           Just _ -> readMaybe ((\(Just s) -> s) pStr) :: Maybe Int
                           Nothing -> Nothing 
                 pageNr = case p of
                        Just n -> n
                        Nothing -> 1
                 i = max (min pageNr len) 1
                 expr = fst $ instr !! (i-1)
                 instructions = snd $ instr !! (i-1)

                 s = case sStr of
                     Just _ -> readMaybe ((\(Just s) -> s) sStr) :: Maybe Int
                     Nothing -> Nothing

                 stepNr = case s of
                        Just n -> if n>1 then n else 1
                        Nothing -> 1
                  
             output $ wrap (dropdown i) instructions (outStepButtons i stepNr (length $ steps expr)) (stepsHtmlN expr (stepNr)) (outTablesN expr (stepNr-1))

main = runCGI $ handleErrors cgiMain

{--

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

--}