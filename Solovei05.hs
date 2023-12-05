{-# OPTIONS_GHC -Wall #-}
module Solovei05 where

type Graph  = [[Int]]

-- Задача 1 ------------------------------------
isGraph :: Graph -> Bool 
isGraph [] = False
isGraph gr = areEdgesGood (length gr) gr 

-- Checks whether there can be such adjacent vertices lists
areEdgesGood :: Int -> [[Int]] -> Bool
areEdgesGood _ [] = True
areEdgesGood amv (x:xs) = if (not (allUnique x)) || (hasBigger (amv - 1) x) then False else (areEdgesGood amv xs)  

allUnique :: Eq a => [a] -> Bool
allUnique [] = True
allUnique xs = if ((head xs) `elem` (tail xs)) then False else allUnique (tail xs) 

hasBigger :: Int -> [Int] -> Bool
hasBigger _ [] = False
hasBigger n (x:xs) = if x > n then True else (hasBigger n xs)  


-- Задача 2 ------------------------------------

-- Attempt #2
isTournament :: Graph -> Bool 
isTournament gr = (hasRightEdges gr) && (hasNoDoubleRoad gr)

numbOfEdges :: Graph -> Int
numbOfEdges gr = length (concat gr)

numbOfVert :: Graph -> Int
numbOfVert gr = length gr

hasRightEdges :: Graph -> Bool
hasRightEdges gr = fromIntegral (numbOfEdges gr) == ((fromIntegral (numbOfVert gr * (numbOfVert gr - 1))) / (fromIntegral 2))

hasNoDoubleRoad :: Graph -> Bool
hasNoDoubleRoad gr = not (or [hasRoad v gr| v <- [0..(length gr) - 1]])

hasRoad :: Int -> Graph -> Bool
hasRoad v gr = or [elem v (gr !! x) | x <- (gr !! v)] 




-- Задача 3 ------------------------------------
isTransitive :: Graph -> Bool 
isTransitive gr = and [isVerTrans v gr | v <- [0..(length gr)-1]]

isVerTrans :: Int -> Graph -> Bool
isVerTrans v gr = (gr !! v) `hasAllElem` (concatMap (gr !!) (gr!!v))

hasAllElem :: [Int] -> [Int] -> Bool
hasAllElem _ [] = True
hasAllElem xs ys = and [x `elem` xs | x <- ys]

-- Задача 4 ------------------------------------
buildTransitive :: Graph -> Graph 
buildTransitive gr = until isTransitive iterGraph gr

iterGraph :: Graph -> Graph 
iterGraph gr = map (buildEdges gr) [0..(length gr)-1] 

buildEdges :: Graph -> Int -> [Int]
buildEdges gr v = (gr !! v) ++ absent (gr !! v) (concatMap (gr !!) (gr!!v))

absent :: Eq a => [a] -> [a] -> [a]
absent _ [] = []
absent xs ys = removeDublicates [e | e <- ys, not (e `elem` xs)]

removeDublicates :: Eq a => [a] -> [a]
removeDublicates [] = []
removeDublicates (x:xs) = x : removeDublicates (filter (x /= ) xs)


-- Задача 5 ------------------------------------
longWay :: Graph -> Int -> Int -> Maybe [Int] 
longWay gr vx vy 
                | vx == vy = Just [vx]
                | (allWaysAB gr vx vy) == [] = Nothing
                | otherwise = Just (myReverse (longestWay (allWaysAB gr vx vy)))


longestWay :: [[Int]] -> [Int]
longestWay [] = []
longestWay wsx =  let curr = head wsx 
                      maxTail = longestWay (tail wsx)
                  in if length curr > length maxTail then curr else maxTail  

-- Задача 6 ------------------------------------
gamiltonWay :: Graph -> Maybe [Int]
gamiltonWay = undefined

-- Задача 7 ------------------------------------
isAcyclic :: Graph -> Bool 
isAcyclic = undefined

-- Задача 8 ------------------------------------
topolSort :: Graph -> Maybe [Int] 
topolSort = undefined

-- Задача 9------------------------------------
isTopolSort :: Graph -> [Int] -> Bool 
isTopolSort = undefined

---------------------Тестові дані - Графи -------

gr1, gr2, gr3, gr4:: Graph
gr1 = [[1,2,3],[2,3],[3,4],[4],[]]
gr2 = [[3,4],[0,3],[0,1,4],[2,4],[1]]
gr3 = [[1],[2],[3],[1],[0,3]]
gr4 = [[1,2,3],[1,2,3],[1,2,3],[1,2,3],[0,1,2,3]]

getAdjEdges :: Int -> [Int] -> [(Int,Int)]
getAdjEdges _ [] = []
getAdjEdges v ed = (v, head ed) : (getAdjEdges v (tail ed)) 

edges :: Graph -> [(Int,Int)]
edges gr = concat (zipWith getAdjEdges [0..(length gr) - 1] gr)

allWaysAB :: Graph -> Int -> Int -> [[Int]]
allWaysAB gr a b = filter (\x -> head x == b) (concat (allWays gr a))
--allWaysAB g a b = [filter (\x -> head x == b) i|i <- allWays g a]
 
allWays :: Graph -> Int -> [[[Int]]]
allWays gr v = until (\x -> null (head x)) (stepW gr) [[[v]]]

stepW :: Graph -> [[[Int]]] -> [[[Int]]]
stepW gr wss@(wsn:_) = [t:w | w@(x:xs) <- wsn, notElem x xs, t<- gr!!x] : wss
tepW _ []  = error "allWays:stepW"

myReverse::[Int] -> [Int]
myReverse g | null g = [] 
            | otherwise = last g : myReverse (init g)
