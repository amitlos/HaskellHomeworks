{-# OPTIONS_GHC -Wall #-}
module Solovei04 where

-- Mastermind -----------------------------------------

-- Фішка може мати один з шести кольорів
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- Код - просто список фішок
type Code = [Peg]

-- Крок гри (Move) будує конструктор Move використовуючи код (Code) і два цілих;  
-- кількість повних і часткових відповідностей кода-пропозиції і шифру
data Move = Move Code Int Int deriving (Show, Eq)

-- Список містить всі різні допустимі кольори
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Задача 1 -----------------------------------------
exactMatches :: Code -> Code -> Int
exactMatches [] _ = 0
exactMatches _ [] = 0
exactMatches cdx cdy = (if (head cdx) == (head cdy) then 1 else 0) + exactMatches (tail cdx) (tail cdy)


-- Задача 2 -----------------------------------------
countColors :: Code -> [Int]
countColors cd = map (cntColor cd) colors

cntColor :: Code -> Peg -> Int
cntColor [] _ = 0
cntColor cd clr = (if ((head cd) == clr) then 1 else 0) + cntColor (tail cd) clr    

-- Задача 3 ----------------------------------------- 
matches :: Code -> Code -> Int
matches cd pr = sum (zipWith min (countColors cd) (countColors pr))   

-- Задача 4 -----------------------------------------
getMove :: Code -> Code -> Move
getMove cd pr = Move pr (exactMatches cd pr) ((matches cd pr)- (exactMatches cd pr)) 

-- Задача 5 -----------------------------------------
isConsistent :: Move -> Code -> Bool
isConsistent (Move pr f p) cd = case getMove cd pr of
                                Move _ f1 p1 -> (f1 == f) && (p1 == p)

-- Задача 6 -----------------------------------------
filterCodes :: Move -> [Code] -> [Code]
filterCodes mv cds = filter (isConsistent mv) cds  

-- Задача 7 -----------------------------------------
allCodes :: Int -> [Code]
allCodes 1 = [[Red], [Green], [Blue], [Yellow], [Orange], [Purple]]
allCodes n = concatMap addAllPosCol (allCodes (n - 1))

addAllPosCol :: Code -> [Code]
addAllPosCol cd = map (: cd) colors 
   
-- Задача 8 -----------------------------------------
solve :: Code -> [Move]
solve cd = solveRec cd (allCodes 4)

solveRec :: Code -> [Code] -> [Move]
solveRec cd cds =  let mv = getMove cd (head cds)
                    in if isLast mv then [mv] else mv : (solveRec cd (filterCodes mv cds))  

isLast :: Move -> Bool
isLast (Move _ f _ ) = (f == 4) 