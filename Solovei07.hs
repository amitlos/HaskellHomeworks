{-# OPTIONS_GHC -Wall #-}
module Solovei07 where

type PolinomOne = [(Int,Rational)]
type Linear   = [Row]
type Row      = [Rational]
data Solution = Empty | One Row  | Many [PolinomOne]
                 deriving (Show, Eq)

-- Task 1.a -----------------------------------------
coef :: Rational -> PolinomOne -> PolinomOne
coef _ [] = []
coef 0 _ = []
coef n pol = case pol of
                ((i, k) : xs) -> (i, k*n) : coef n xs

-- Task 1.b -----------------------------------------
add :: PolinomOne -> PolinomOne -> PolinomOne
add p1 p2 = unify (p1 ++ p2)

-- Task 1.c -----------------------------------------
unify :: PolinomOne -> PolinomOne
unify = normalise . sortf f
    where f = \ (x, _) (y, _) -> compare y x
          normalise [] = []
          normalise [a] = [a]
          normalise ((x, a):(y, b):s) | x == y = if a + b /= 0
                                                 then normalise $ (x, a + b) : s
                                                 else normalise s
                                      | otherwise = (x, a) : (normalise $ (y, b) : s)

sortf :: (a -> a -> Ordering) -> [a] -> [a]
sortf _ [] = []
sortf f list = let x = head list in
            (sortf f (filter (\ e -> f x e == LT) (tail list))) ++ (x : filter (\ e -> f x e == EQ) (tail list)) ++ (sortf f (filter (\ e -> f x e == GT) (tail list)))

-- Task 2.a -----------------------------------------
findFree :: [PolinomOne] -> [Int]
findFree pol = [fst p | [p] <- pol, length [p] == 1]

-- Task 2.b -----------------------------------------
iswfCommon ::  [PolinomOne]  -> Bool
iswfCommon pol | length pol == 0 = True
               | pol == remove pol (findFree pol) = False
               | otherwise = iswfCommon (remove pol (findFree pol))
    where remove p free = filter (/= []) (map (filter (\(a, _) -> not (elem a free))) $ p)

-- Task 3.a -----------------------------------------
isSimple :: Linear -> Bool
isSimple le = length (filter (\x -> (length x) /= 1) le) == 0

-- Task 3.b -----------------------------------------
solveSimple :: Linear -> Maybe [PolinomOne]
solveSimple le  = if and [length row == 1 && head row == 0 | row <- le] then Just [] else Nothing

-- Task 4.a -----------------------------------------
findRow :: Linear -> Maybe Int
findRow le = if not (null (filter (\x -> head x /= 0) le)) then Just (snd (head ( filter (\x -> head (fst x) /= 0) (zip le [1..])))) else Nothing

-- Task 4.b -----------------------------------------
exchangeRow :: [a] -> Int -> [a]
exchangeRow le i | i > length le = error "out of range"
                 | i == 1 = le
                 | otherwise = [le!!(i-1)] ++ (tail (take (i - 1) le)) ++ [head le] ++ (drop i le)

-- Task 5.a -----------------------------------------
forwardStep :: Row -> Linear -> Linear
forwardStep fs rs = [[row !! k - ((fs !! k / fs !! 0) * row !! 0 ) | k <- [1..(length fs) - 1]] | row <- rs]

-- Task 5.b -----------------------------------------
reverseStep :: Row -> [PolinomOne] -> [PolinomOne]
reverseStep fs vs = coef ((-1) / head fs) (add [(0, - (last fs))] (foldl add [] [coef (fs !! i) (vs !! (i - 1)) | i <- [1..(length fs) - 2]])) : vs

-- Task 6 -------------------------------------------
gauss :: Int -> Linear -> Maybe [PolinomOne]
gauss _ [] = Just []
gauss i le = if isSimple le then solveSimple le else case findRow le of
                Just row -> let exch = exchangeRow le row in case gauss (i + 1) (forwardStep (head exch) (tail exch)) of
                    Just subrow ->  Just (reverseStep (head exch) subrow)
                    _ -> Nothing
                _ -> case gauss (i + 1) ([[q !! el |el <- [1..(length q) - 1]]| q <- le]) of
                    Just subrow -> Just ([(i, 1)]:subrow)
                    _ -> Nothing

-- Task 7.a -----------------------------------------
testEquation :: [PolinomOne] -> Row -> Bool
testEquation pos rw = let test = foldr add [] [coef (rw !! i) (pos !! i) | i <- [0..length pos - 1]] in length test == 1 && fst (head test) == 0 && snd (head test) == last rw

-- Task 7.b -----------------------------------------
testLinear :: [PolinomOne] -> Linear -> Bool
testLinear = all . testEquation

-- Task 8 -------------------------------------------
solving :: Linear -> Solution
solving le = case gauss 1 le of
        Just sol -> case and [length pol == 1 && fst (head pol) == 0 | pol <- sol] of
            True -> One [snd (head pol) | pol <- sol] 
            False -> Many sol
        Nothing -> Empty

-----------------------------------------------------
pol0, pol1, pol2, pol3, pol4 :: PolinomOne
pol0 = [(0,3/5), (3,1), (3,-2/7), (2,3), (0,-7/3), (4,0)]
pol1 = [(5,3/4), (0,7), (4,3/2), (5,-2/3), (0,1/2)]
pol2 = [(0,15), (4,3),(5,1)]
pol3 = [(0,-10), (2,7), (4,-3)]
pol4 = [(0,-26/15), (2,3), (3,5/7)]

test0, test1, test2, test3, test3a, test4 :: Linear
test0 = [[0,-2,-1,2],[0,-4,-5,3],[1,2,4,5]]
test1 = [[4,-3,2,-1,8],[3,-2,1,-3,7],[5,-3,1,-8,1]]
test2 = [[7,-2,-1,2],[6,-4,-5,3],[1,2,4,5]]
test3 = [[2,3,-1,1,1],[8,12,-9,8,3],[4,6,3,-2,3],[2,3,9,-7,3]]
test3a = [[0,-5,4,-1], [0,5,-4,1],[0,10,-8,2]]
test4 = [[6,1,2,21], [4,-6,16,2], [3,8,1,2]]

res3, res4 :: [PolinomOne]
res3 = [[(0,3/5),(2,-3/2),(4,-1/10)],[(2,1)],[(0,1/5),(4,4/5)],[(4,1)]]
res4 = [[(0,62/15)], [(0,-17/15)], [(0,-4/3)]]

sol1,sol2,sol3,sol4 :: Solution
sol1 = Empty
sol2 = Empty
sol3 = Many res3
sol4 = One [62/15,-17/15,-4/3]
