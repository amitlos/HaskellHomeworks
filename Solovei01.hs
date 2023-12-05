{-# OPTIONS_GHC -Wall #-}
module Solovei01 where

-- Задача 1 -----------------------------------------
factorial :: Integer -> Integer
factorial x = if x < 1 then 1 else x * factorial (x - 1) 

-- Задача 2 -----------------------------------------
listSum :: [Int] -> [Int] -> [Int]
listSum xs ys = if (null xs) && (null ys) then [] else (((if null xs then 0 else head xs) + (if null ys then 0 else head ys)) : listSum (if null xs then [] else tail xs) (if null ys then [] else tail ys)) 

-- Задача 3 ----------------------------------------- 
oddEven :: [Int] -> [Int] 
oddEven xs = if (null xs) || (null (tail xs)) then xs else head (tail xs) : (head xs : (oddEven (tail (tail xs)))) 

-- Задача 4 -----------------------------------------
position :: Int -> [Int] -> Int
position n xs = if null xs then (negate 1) else (if (head xs) == n then 0 else (if (position n (tail xs)) == (negate 1) then negate 1 else (1 + position n (tail xs)))) 
                     
-- Задача 5 -----------------------------------------
set :: [Int] -> [Int] 
set xs = if null xs then [] else (if (position (head xs) (tail xs)) == (negate 1) then head xs : set (tail xs) else set (tail xs)) 

-- Задача 6 -----------------------------------------
union :: [Int] -> [Int] -> [Int]
union xs ys = set (uniteTwoLists xs ys)

-- Задача 7 -----------------------------------------
intersection :: [Int] -> [Int] -> [Int]
intersection xs ys = intersectionN (set xs) (set ys) 

-- Задача 8 -----------------------------------------
factorialsM :: [Integer]
factorialsM = [factorial x | x <- [1,2..]]

-- Функція, що об'єднує два списки без збереження порядку, з дублікатами
uniteTwoLists :: [Int] -> [Int] -> [Int]
uniteTwoLists xs ys =  if null xs then ys else uniteTwoLists (tail xs) (head xs : ys)

-- Function that returns the intersection of normal sets(without dublicates)
intersectionN :: [Int] -> [Int] -> [Int]
intersectionN xs ys = if ((null xs) || (null ys)) then [] else (if (position (head xs) ys == negate 1) then (intersectionN (tail xs) ys) else (head xs : intersectionN (tail xs) ys)) 

cumSumPrefix :: [Int] -> [Int]
cumSumPrefix xs = if null xs then [] else  let 
                                            first = head xs
                                            other = tail xs 
                                            in (first : map (first+) (cumSumPrefix other)) 
