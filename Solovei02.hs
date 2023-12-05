{-# OPTIONS_GHC -Wall #-}
module Solovei02 where

-- Задача 1 -----------------------------------------
sumFr :: [Integer] -> Integer
sumFr xs = foldr (+) 0 xs 
  
-- Задача 2 ----------------------------------------- 
factorial :: Integer -> Integer
factorial n = if n < 2 then 1 else (foldl (*) 1 [2..n])

-- Задача 3 -----------------------------------------
concatFr :: [Integer] -> [Integer] -> [Integer]
concatFr [] [] = []
concatFr xs [] = xs
concatFr [] ys = ys
concatFr xs ys = foldr (:) ys xs

-- Задача 4 -----------------------------------------
sortInsert :: [Integer] -> [Integer]
sortInsert xs = foldl insert [] xs 

-- Задача 5 -----------------------------------------
map2 :: (a->b->c) -> [a] -> [b] -> [c]
map2 _ [] _ = []
map2 _ _ [] = []
map2 f xs ys = (f (head xs) (head ys)) : (map2 f (tail xs) (tail ys)) 

 
-- Задача 6 -----------------------------------------
expPart :: Integer -> Integer -> Double
expPart m n = sumFrD (map2 (divInteger) (take (fromIntegral n) [m^i | i <- [1..n]]) (take (fromIntegral n) factorialsM)) 

-- Задача 7 -----------------------------------------
triangle :: [Integer]
triangle = scanl (+) 1 [2..]

-- Задача 8 -----------------------------------------
piramid :: [Integer]
piramid = scanl1 (+) [i^2 | i <- [1..]]

-- Задача 9 -----------------------------------------
indexes :: [Int] -> [Int] -> [Int]
indexes [] ys = [0..(length ys)]
indexes _ [] = []
indexes xs ys = filter (theSameStartI xs ys) [0..((length ys) - 1)]


-- Additional functions --

-- Inserts element into a sorted list in growing order
insert :: [Integer] -> Integer -> [Integer]
insert [] e = [e]
insert xs e = if (e <= head xs) then (e : xs) else (head xs : (insert (tail xs) e))

-- Checks whether first argument is the beginning of the second argument  --
theSameStart :: [Int] -> [Int] -> Bool
theSameStart [] [] = True
theSameStart _ [] = False
theSameStart [] _ = True
theSameStart xs ys = if (head xs) == (head ys) then (theSameStart (tail xs) (tail ys)) else False

-- Checks whether first argument is the beginning of the second argument starting from given index
theSameStartI :: [Int] -> [Int] -> Int -> Bool
theSameStartI xs ys i = theSameStart xs (drop i ys) 

-- Infinite list of factorials of all numbers
factorialsM :: [Integer]
factorialsM = [factorial x | x <- [1,2..]]

-- For Double
sumFrD :: [Double] -> Double
sumFrD xs = foldr (+) 0.0 xs 

-- Divide two integer
divInteger:: Integer -> Integer -> Double
divInteger x y = (fromIntegral x) / (fromIntegral y) 

-- Practice #3 (my attempt)

type Term = [String]
type Expr = [Term]

findAllExprs :: Int -> String -> [Expr]
findAllExprs = undefined

calcExpr :: Expr -> Int
calcExpr [] = 0
calcExpr xs = (calcTerm (head xs)) + calcExpr (tail xs) 

allPosExprs :: String -> [Expr]
allPosExprs = undefined

calcTerm :: Term -> Int
calcTerm [] = 1
calcTerm xs = (read (head xs) :: Int) * calcTerm (tail xs)


