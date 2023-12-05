{-# OPTIONS_GHC -Wall #-}
module Solovei03 where

-- Задача 1 -----------------------------------------
testing :: [Int] -> Bool
testing [] = True
testing (_:[]) = True
testing (x:xs) = if x > (head xs) then False else testing xs  

-- Задача 2 -----------------------------------------
all35 :: Int -> [Int]
all35 n = take (div (n-1) 15) [15,30..]
-- I can prove 

-- Задача 3 -----------------------------------------
compress :: [Int] -> [Int]
compress [] = []
compress xs = head xs : (compress (deleteElems (head xs) (tail xs))) 

-- Задача 4 -----------------------------------------
primeFactor :: Int -> [Int]
primeFactor n   | n > 1 = let x = (divByPr n) 
                          in x : primeFactor (div n x)
                | True  = []   
  
-- Задача 5 ----------------------------------------- 
fibons :: [Integer]
fibons = [fib x | x <- [0..]] 

-- Задача 6 -----------------------------------------
lastTail :: String -> String
lastTail [] = []
lastTail (s:[]) = [s] 
lastTail xs = let lastInTail = lastTail (tail xs)
            in if (max xs lastInTail) == xs then xs else lastInTail     

-- Задача 7 -----------------------------------------
intToString :: Int -> Int -> String
intToString n m = map intToChar (buildList m n (posAm n m))

-- Задача 8 -----------------------------------------
sumPalindrom2 :: Integer -> Integer
sumPalindrom2 n = sumFr [x | x <- [1..n], isIntPalindrom x] 

-- Deleting all elements on the begining equal to a given element
deleteElems :: Eq a => a -> [a] -> [a]
deleteElems _ [] = []
deleteElems n xs = if (n == head xs) then deleteElems n (tail xs) else xs

-- Function that checks whether given number is prime
isPrime :: Int -> Bool
isPrime k = if k > 1 then null [ x | x <- [2..(k - 1)], k `mod` x == 0] else False

-- Looking for the smallest prime divisor
divByPr :: Int -> Int
divByPr n = if n > 1 then head [x | x <- [2..n], isPrime x, n `mod` x == 0] else 1

-- Fibonachi number n
fib :: Int -> Integer
fib n = if (n < 2) then 1 else fib (n-1) + fib(n-2) 

-- Define amount of positions needed to write this number 
posAm :: Int -> Int -> Int
posAm n m =  head [x | x <- [1..], n^x > m ]

buildList :: Int -> Int -> Int -> [Int]
buildList n _ 1 = [n]
buildList n m maxx = (n `div` m^(maxx - 1)) : buildList (n `mod` m^(maxx - 1)) m (maxx - 1)

intToChar :: Int -> Char 
intToChar n = case n of 
            0 -> '0'
            1 -> '1'
            2 -> '2'
            3 -> '3'
            4 -> '4'
            5 -> '5'
            6 -> '6'
            7 -> '7'
            8 -> '8'
            9 -> '9'
            10 -> 'A'
            11 -> 'B'
            12 -> 'C'
            13 -> 'D'
            14 -> 'E'
            15 -> 'F'
            _ -> '_'

isIntPalindrom :: Integer -> Bool
isIntPalindrom n = let x = intToString 2 (fromIntegral n)
                in (x == reverse x)

sumFr :: [Integer] -> Integer
sumFr xs = foldr (+) 0 xs 