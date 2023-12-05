{-# OPTIONS_GHC -Wall #-}
module Solovei00 where
import Control.DeepSeq (NFData(rnf))
import Distribution.Simple.Setup (trueArg)
import GHC.Exts.Heap (TsoFlags(TsoFlagsUnknownValue))

data Command = Z Int | S Int | T Int Int | J Int Int Int  deriving (Show, Eq)
type Program = [Command]
type ConfigС = (Int, Int, [Int])

type Graph  = [[Int]]

data BinTreeM a = EmptyM 
                | NodeM a Int (BinTreeM a) (BinTreeM a)
                   deriving (Show, Eq) 

-- Задача  1 -----------------------------------------
maximReg :: Program -> Int 
maximReg [] = 0
maximReg [x] = maxRegInCom x
maximReg (x:xs) = max maxInCom maxInPr 
                where 
                        maxInCom = (maxRegInCom x) 
                        maxInPr = (maximReg xs)

maxRegInCom :: Command -> Int
maxRegInCom (Z rn) = rn
maxRegInCom (S rn) = rn
maxRegInCom (T rn1 rn2) = max rn1 rn2
maxRegInCom (J rn1 rn2 _) = max rn1 rn2

-- Задача 2 -----------------------------------------
ini :: Program -> [Int] -> [Int] 
ini pr xs | maximReg pr > length xs = xs ++ generateZeroList ((maximReg pr) - (length xs)) 
          | otherwise = take (maximReg pr) xs

generateZeroList :: Int -> [Int]
generateZeroList n = take n [0,0..] 

upd :: [Int] -> Int -> Int -> [Int]
upd reg r v  | r >= length reg = reg
             | otherwise = (take r reg) ++ [v] ++ (drop (r+1) reg) 

-- Задача 3 -----------------------------------------
stepC :: Program -> ConfigС -> ConfigС
stepC pr (nm, st, rs)  | nm <= length pr = let command = commSel pr nm in (nextCommInd command (nm, st, rs),st + 1, applyComm command rs)
                       | otherwise = (nm, st,rs)
commSel :: Program -> Int -> Command 
commSel pr cn = pr !! (cn - 1)

applyComm :: Command -> [Int] -> [Int]
applyComm (Z rn) rs = upd rs (rn - 1) 0         -- We are bulding a list with needed length, so there is no need to check whether rn < length rs   
applyComm (S rn) rs = upd rs (rn - 1) ((rs !! (rn - 1)) + 1)
applyComm (T rn1 rn2) rs = upd rs (rn2 - 1) (rs !! (rn1 - 1))
applyComm _ rs = rs

nextCommInd :: Command -> ConfigС -> Int
nextCommInd (J r1 r2 c) (nm, _, rg) = if rg !! (r1 - 1) == rg !! (r2 - 1) then c else nm + 1
nextCommInd _ (nm, _, _) = nm + 1  
--- Задача 4 ----------------------------------------
evalC :: Program -> Int -> [Int] -> Maybe Int 
evalC pr m inp = evalCRec pr (1, 0, ini pr inp) m

evalCRec :: Program -> ConfigС -> Int -> Maybe Int
evalCRec pr (nm, st, rs) m | m == st = if nm > length pr then Just (head rs) else Nothing
                           | nm > length pr = Just (head rs)
                           | otherwise = evalCRec pr (stepC pr (nm, st,rs)) m 

-- Задача 5 -----------------------------------------
palindrom10 :: Int -> Int -> [Int] 
palindrom10 n m = filter isPal [n+1, n+2 .. (m -1)]

isPal :: Int -> Bool
isPal n = isStrPal (show n)


isStrPal :: String -> Bool
isStrPal xs = xs == reverse xs 

-- Задача 6 -----------------------------------------
maxSuf :: [Int] -> Int
maxSuf xs = getMaxSum [drop x xs| x <- [0,1..length xs]]

getMaxSum :: [[Int]] -> Int
getMaxSum [] = 0  
getMaxSum [x] = sumFr x 
getMaxSum (x:xs) = let sumx = sumFr x; sumxs = getMaxSum xs in max sumx sumxs  

sumFr :: [Int] -> Int
sumFr xs = foldr (+) 0 xs 

-- Задача 7 -----------------------------------------
encode :: String -> [(Int,Char)]
encode s = encodeRec s [] 

encodeRec :: String -> [(Int,Char)] -> [(Int,Char)]
encodeRec [] ens = ens
encodeRec (x:xs) ens = encodeRec xs (addSymb x ens) 

addSymb :: Char -> [(Int, Char)] -> [(Int, Char)]
addSymb c [] = [(1, c)]
addSymb c ens = if snd (last ens) == c then (init ens) ++ [(fst (last ens) + 1, c)] else (ens ++ [(1, c)])

-- Задача 8 -----------------------------------------
maxComSuf :: String -> String -> Int
maxComSuf s1 s2 = let sms = if length s1 > length s2 then s2 else s1; bgs = if length s1 > length s2 then s1 else s2 
                        in head [length sms - x| x <- [0..length sms], drop x sms == drop (x + (length bgs - length sms)) bgs] 

-- Задача 9 -----------------------------------------
groupChar :: String -> [String] 
groupChar = decode . encode 

decode :: [(Int, Char)] -> [String]
decode = map charsToStr

charsToStr :: (Int, Char) -> String
charsToStr (0, _) = []
charsToStr (1, c) = [c]
charsToStr (n, c) = c : charsToStr (n-1, c)

--- Задача 10 ----------------------------------------
eccentricity :: Graph -> Int -> Int 
eccentricity gr a = (maxNum $ map length (concat [shortWayAll gr a v | v <-[0 .. (length gr - 1)]])) - 1

--- Задача 11 ----------------------------------------
findDiameter :: Graph -> Int 
findDiameter gr = maxNum [eccentricity gr v | v <-[0 .. (length gr - 1)] ]

maxNum :: [Int] -> Int
maxNum [x] = x 
maxNum (x:xs) = max x (maxNum xs)
maxNum _ = 0

findRadius :: Graph -> Int 
findRadius gr = minNum [eccentricity gr v | v <-[0 .. (length gr - 1)] ]

minNum :: [Int] -> Int
minNum [x] = x 
minNum (x:xs) = min x (minNum xs)
minNum _ = 0

--- Задача 12 ----------------------------------------
findCenter :: Graph -> [Int] 
findCenter gr = let rad = findRadius gr in [v | v <- [0 .. (length gr - 1)], eccentricity gr v == rad]

--- Задача 13 ----------------------------------------
shortWayAll :: Graph -> Int -> Int -> [[Int]]
shortWayAll gr a b = map reverse $ minimalWays $
                         filter (\ l -> head l == b) (concat (allWays gr a))
{-                         
shortWayAll :: Graph -> Int -> Int -> [[Int]] 
shortWayAll gr a b = shortWayRec gr a b [(a,[[a]])]

shortWayRec :: Graph -> Int -> Int -> [(Int, [[Int]])] -> [[Int]]
shortWayRec gr a b ways | b `elem` (map fst ways) = head [ws | (v, ws) <- ways, v == b]
                        | otherwise = shortWayRec gr a b (nextWays gr ways) 

nextWays :: Graph -> [(Int, [[Int]])] -> [(Int, [[Int]])]
nextWays gr ways = foldl addNewVert [] ways

addNewVert :: [(Int, [[Int]])] -> (Int, [[Int]]) -> [(Int, [[Int]])] 
addNewVert = undefined 
-}
minimalWays :: [[a]] -> [[a]]
minimalWays [] = []
minimalWays [x] = [x]
minimalWays (x:xs) | a < b = [x]
                   | a == b = x:oldMinWays
                   | otherwise = oldMinWays
    where (a, b) = (length x, length $ head oldMinWays)
          oldMinWays = minimalWays xs

allWays :: Graph -> Int -> [[[Int]]]
allWays gr v = until condW (stepW gr) [[[v]]]

condW :: ([[[Int]]]) -> Bool
condW wss = null (head wss)

stepW :: Graph -> [[[Int]]] -> [[[Int]]]
stepW gr wss@(wsn:_) = [t:w | w@(x:xs) <- wsn, notElem x xs, t<- gr !! x] : wss
stepW _ [] = error "allWays:stepW"

--- Задача 14 ----------------------------------------
isSearch :: (Ord a) => BinTreeM a -> Bool
isSearch EmptyM = True
isSearch (NodeM val n tl tr) = not (hasBiggerThan tl val || hasSmallerThan tr val) && n > 0 && isSearch tl && isSearch tr    

hasBiggerThan :: (Ord a) => BinTreeM a -> a -> Bool
hasBiggerThan EmptyM _ = False
hasBiggerThan (NodeM nval n tl tr) val = nval >= val || tl `hasBiggerThan` val || tr `hasBiggerThan` val  

hasSmallerThan :: (Ord a) => BinTreeM a -> a -> Bool
hasSmallerThan EmptyM _ = False
hasSmallerThan (NodeM nval n tl tr) val = nval <= val || tl `hasSmallerThan` val || tr `hasSmallerThan` val 
--- Задача 15 ----------------------------------------
elemSearch :: (Ord a) => BinTreeM a -> a -> Bool
elemSearch EmptyM _ = False
elemSearch (NodeM nval n tl tr) val = (nval == val && n > 0) || elemSearch tl val || elemSearch tr val

--- Задача 16 ----------------------------------------
insSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a 
insSearch EmptyM val = NodeM val 1 EmptyM EmptyM
insSearch (NodeM nval n tl tr) val | val == nval = (NodeM nval (n+1) tl tr) 
                                   | val > nval = (NodeM nval n tl (insSearch tr val))
                                   | val < nval = (NodeM nval n (insSearch tl val) tr)

--- Задача 17 ----------------------------------------
{-delSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a 
delSearch EmptyM _ = EmptyM
delSearch (NodeM nval n tl tr) val | val == nval = if n > 1 then NodeM nval (n-1) tl tr else delZeroVal (NodeM nval 0 tl tr)
                                   | val < nval = delSearch tl val
                                   | val > nval = delSearch tr val

delZeroVal :: (Ord a) => BinTreeM a -> BinTreeM a
delZeroVal EmptyM = EmptyM
delZeroVal (NodeM _ 0 EmptyM tr) = tr
delZeroVal (NodeM nval 0 tl tr) = undefined
delZeroVal tr = tr
-}
delMin :: (Ord a) => BinTreeM a -> (a, Int, BinTreeM a)
delMin (EmptyM) = error "empty tree in minimum deletion"
delMin (NodeM a cnt EmptyM right) = (a, cnt, right)
delMin (NodeM a cnt left right) = (x, num, NodeM a cnt leftCorr right)
    where (x, num, leftCorr) = delMin left

delSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a
delSearch (EmptyM) _ = EmptyM
delSearch tree e | not (elemSearch tree e) = tree
                 | e > a = NodeM a cnt left (delSearch right e)
                 | e < a = NodeM a cnt (delSearch left e) right
                 | otherwise = if cnt > 1
                               then NodeM a (pred cnt) left right
                               else if right /= EmptyM
                                    then let (minEl, minCnt, corrTree) = delMin right
                                         in NodeM minEl minCnt left corrTree
                                    else left
    where (NodeM a cnt left right) = tree
 
--- Задача 18 ----------------------------------------
sortList :: (Ord a) => [a] -> [a]
sortList = toList . fromList
    where fromList :: (Ord a) => [a] -> BinTreeM a
          fromList [] = EmptyM
          fromList (x:xs) = insSearch (fromList xs) x
          toList :: (Ord a) => BinTreeM a -> [a]
          toList EmptyM = []
          toList (NodeM a cnt left right) =
              concat [toList left, take cnt $ repeat a, toList right]

---------------------Тестові дані 
---------------------Тестові дані - Програми МНР ---------------------------
notSignum, addition, subtraction :: Program 
-- функція notSignum x
notSignum = [Z 2, J 1 2 5, Z 1, J 1 1 6, S 1] 

-- функція додавання  addition x y = x+y
addition = [Z 3, J 3 2 6, S 1, S 3, J 1 1 2]

-- функція віднімання subtraction x y = x-y, визначена для x>=y 
subtraction = [Z 3, J 1 2 6, S 2, S 3, J 1 1 2, T 3 1]

---------------------- Графи -------
gr1, gr2:: Graph
gr1 = [[1,2,3],[0,3,4],[0,4,5],[0,1],[1,2],[2]]
gr2 = [[1,3],[0,2],[1,3],[0,2],[5,6],[4,6],[4,5],[]]

---  Бінарні дерева 
bm :: BinTreeM Char
bm = NodeM  't' 2  
            (NodeM 'a' 1  EmptyM 
                    (NodeM 'e' 1 
                             (NodeM 'd' 2 EmptyM EmptyM)
                             (NodeM 'f' 1 EmptyM EmptyM)
                    )
            ) 
            (NodeM 'w' 2  EmptyM EmptyM)   