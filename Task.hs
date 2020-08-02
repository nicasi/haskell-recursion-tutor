{-# LANGUAGE QuasiQuotes #-}

module Task
   where

import System.Environment
import Text.Printf
import Text.RawString.QQ

data Expr = Var String | App Expr Expr | Con Int
   deriving (Eq)

instance Show Expr where
   show = ppExpr 0

-- infixr 5 :
-- infixl 6 +
-- infixl 7 *

ppExpr :: Int -> Expr -> String
ppExpr n expr =
   case collect expr of
      (Con n, [])   -> show n
      (Var "cons", [x, xs]) -> parIf (n > 5) $ ppExpr 6 x ++ " : " ++ ppExpr 5 xs
      (Var "+", [x, y])     -> parIf (n > 6) $ ppExpr 6 x ++ " + " ++ ppExpr 7 y
      (Var "*", [x, y])     -> parIf (n > 7) $ ppExpr 7 x ++ " * " ++ ppExpr 8 y
      (Var f, args)         -> parIf (n >= 10 && not (null args)) $ unwords (f : map (ppExpr 10) args)
      _ -> error "ppExpr: invalid expression"

collect :: Expr -> (Expr, [Expr])
collect = rec []
   where
      rec acc (App f a) = rec (a:acc) f
      rec acc e = (e, acc)

-- priorities: 0 is laagste, 10 is rechts van applicatie

parIf :: Bool -> String -> String
parIf True  s = "(" ++ s ++ ")"
parIf False s = s

type Table = (String, [(Expr, Expr)])
type Task = (Expr, [Table])

bin :: String -> Expr -> Expr -> Expr
bin s x y = App ( App ( Var s ) x ) y

cons :: Expr -> Expr -> Expr
cons = bin "cons"

nil :: Expr
nil = Var "[]"

fib :: Expr -> Expr
fib e = App (Var "fib") e

square :: Expr -> Expr
square e = App (Var "square") e

squares :: Expr -> Expr
squares e = App (Var "squares") e

mult :: Expr -> Expr -> Expr
mult x y = App (App (Var "*") x) y

add :: Expr -> Expr -> Expr
add x y = App (App (Var "+") x) y

--------------------------

type Path = [Direction]

data Direction = L | R 
 deriving (Show, Eq)

subexpr :: Path -> Expr -> Expr
subexpr [] e = e
subexpr (L:p) (App f a) = subexpr p f
subexpr (R:p) (App f a) = subexpr p a
subexpr _ _ = error "subexpr: invalid path"

{--
stepsSub e =
   case step e of
      Just (e', p) -> (e, p) : stepsSub e'
      Nothing      -> []
--}

isBin :: Expr -> Bool
isBin (App f a) =
   case f of
      (App f' a') -> True
      _           -> False
isBin _ = False

isAdd :: Expr -> Bool
isAdd (App f a) =
   case f of
      (App (Var "+") a') -> True
      _                  -> False



p l = mapM print $ l

stepsSub :: Expr -> [(Expr, Path)]
stepsSub e =
   case step e of
      Just (e', p) -> (e, p) : stepsSub e'
      Nothing      -> (e, []) : []

stepsSubN :: Expr -> Int -> [(Expr, Path)]
stepsSubN _ 0 = []
stepsSubN e n =
   case step e of
      Just (e', p) -> (e, p) : stepsSubN e' (n-1)
      Nothing      -> (e, []) : []

-- lijst met daarin: oude expressie, locatie (in oude en nieuwe expressie), nieuwe expressie, oude subexpressie, herschreven subexpressie
stepsSub' :: Expr -> [(Expr, Path, Expr, Expr, Expr)]
stepsSub' e =
   case step e of
      Just (e', p) -> (e, p, e', subexpr p e, subexpr p e') : stepsSub' e'
      Nothing      -> []

eval :: Expr -> Expr
eval = last . steps

step' :: Expr -> Expr
step' e =
   case step e of
      Just (e', p) -> e' 
      Nothing      -> Var "[]"

steps :: Expr -> [Expr]
steps e =
   case step e of
      Just (e', p) -> e : steps e'
      Nothing      -> [e]

-- lijst met daarin: oude expressie, locatie (in oude en nieuwe expressie), nieuwe expressie
steps' :: Expr -> [(Expr, Path, Expr)]
steps' e =
   case step e of
      Just (e', p) -> (e, p, e') : steps' e'
      Nothing      -> []

step :: Expr -> Maybe (Expr, Path)
step (Con _) = Nothing
step (Var _) = Nothing
step (App (App (Var "*") (Con n)) (Con m)) = Just (Con (n*m), [])
step (App (App (Var "+") (Con n)) (Con m)) = Just (Con (n+m), [])
step (App (Var "fac") (Con 1)) = Just (Con 1, [])
step (App (Var "fac") (Con n)) = Just (mult (Con (n)) (App (Var "fac") (Con (n-1))), [])
step (App (Var "fib") (Con 0)) = Just (Con 0, [])
step (App (Var "fib") (Con 1)) = Just (Con 1, [])
step (App (Var "fib") (Con n)) = Just (add (fib (Con (n-1))) (fib (Con (n-2))), [])
step (App (Var "f") (Con 1)) = Just (Con 1, [])
step (App (Var "f") (Con n)) = Just (add (mult (Con 4) (App (Var "f") (Con (n `div` 2)))) (Con 3), [])
step (App (Var "squares") (App (App (Var "cons") x) xs)) =
   Just (cons (square x) (squares xs), [])
step (App (Var "squares") (Var "[]")) =
   Just (nil, [])
step (App (Var "square") (Con n)) = Just (Con (n*n), []) 
step (App f a) = 
   case step f of
      Just (f', p) -> Just (App f' a, L:p)
      Nothing ->
         case step a of
            Just (a', p) -> Just (App f a', R:p)
            Nothing -> Nothing