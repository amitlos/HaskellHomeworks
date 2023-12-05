{-# OPTIONS_GHC -Wall #-}
module Solovei09 where

import Data.Char(isLower)
import Language.Haskell.TH (valD)

data BExp = Bvalue Bool | Bvar Char | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)
type Env = [(Char, Bool)]

type NodeId = Int
type BDDNode =  (NodeId, (Char, NodeId, NodeId))
type BDD = (NodeId, [BDDNode])

-- Задача 1 -----------------------------------------
checkSat :: BDD -> Env -> Bool
checkSat (1, _) _ =  True
checkSat (0, _) _ =  False
checkSat (topid, nodes) xenv = case head (filter ((==topid) . fst) nodes) of
                                (_, (var, fls, tr)) ->  if snd (head (filter ((==var) . fst) xenv)) then checkSat (tr, nodes) xenv else checkSat(fls, nodes) xenv
-- Задача 2 -----------------------------------------
sat :: BDD -> [[(Char, Bool)]]
sat tree = [env | env <- takeAllEnv . takeVars $ tree, checkSat tree env] 

removeDublicates :: Eq a => [a] -> [a]
removeDublicates [] = []
removeDublicates (x:xs) = x : removeDublicates (filter (x /= ) xs)

takeVars :: BDD -> [Char]
takeVars (_,nodes) = removeDublicates [ case node of (_, (var, _, _)) -> var | node <- nodes]  

takeAllEnv :: [Char] -> [Env]
takeAllEnv [] = []
takeAllEnv (var:[]) = [[(var, False)], [(var, True)]]
takeAllEnv vars = (map ([(head vars, True)]++) (takeAllEnv (tail vars))) ++ (map ([(head vars, False)]++) (takeAllEnv (tail vars)))

-- Задача 3 -----------------------------------------
simplify :: BExp -> BExp
simplify (Not (Bvalue x)) = Bvalue (not x)
simplify (And (Bvalue x) (Bvalue y)) = Bvalue (x && y)
simplify (Or (Bvalue x) (Bvalue y)) = Bvalue (x || y)
simplify tree = tree


-- Задача 4 -----------------------------------------
restrict :: BExp -> Char -> Bool -> BExp
restrict e var val = simplifyAll (restrictNoSimpl e var val)

restrictNoSimpl :: BExp -> Char -> Bool -> BExp  
restrictNoSimpl (Bvalue val) _ _ = Bvalue val 
restrictNoSimpl (Bvar var1) var2 val = if var1 == var2 then Bvalue val else Bvar var1
restrictNoSimpl (Not e) var val = Not (restrictNoSimpl e var val)
restrictNoSimpl (Or e1 e2) var val = Or (restrictNoSimpl e1  var val) (restrictNoSimpl e2 var val)
restrictNoSimpl (And e1 e2) var val = And (restrictNoSimpl e1 var val) (restrictNoSimpl e2 var val)

simplifyAll :: BExp -> BExp 
simplifyAll (Not (Bvalue x)) = Bvalue (not x)
simplifyAll (Or (Bvalue x) (Bvalue y)) = Bvalue (x || y)
simplifyAll (And (Bvalue x) (Bvalue y)) = Bvalue (x && y)
simplifyAll (Not t) = simplify $ Not (simplifyAll t)
simplifyAll (Or t1 t2)  = simplify $ Or (simplifyAll t1) (simplifyAll t2) 
simplifyAll (And t1 t2) = simplify $ And (simplifyAll t1) (simplifyAll t2) 
simplifyAll x = x
-- Задача 5 -----------------------------------------
-- Передумова: Кожна змінна (буква) в булевому виразі (BExp) з"являється 
--    точно один раз в списку змінних (Char); немає інших елементів
buildBDD :: BExp -> [Char] -> BDD
buildBDD e xs = buildBDD' e 2 xs 

buildBDD' :: BExp -> NodeId -> [Char] -> BDD
buildBDD' e _ [] | e == Bvalue False = (0, [])
                 | e == Bvalue True  = (1, [])
buildBDD' e n (x:xs) = (n, (n, (x, ldata, rdata)) : lnodes ++ rnodes)
                        where (ldata, lnodes) = buildBDD' (restrict e x False) (2 * n) xs
                              (rdata, rnodes) = buildBDD' (restrict e x True) (2 * n + 1) xs
buildBDD' _ _ [] = error "illegal state"  

-- Задача 6 -----------------------------------------
-- Передумова: Кожна змінна (буква) в булевому виразі (BExp) з"являється 
--    точно один раз в списку змінних (Char); немає інших елементів
buildROBDD :: BExp -> [Char] -> BDD
buildROBDD e xs = buildROBDD' e 2 xs 

buildROBDD' :: BExp -> NodeId -> [Char] -> BDD
buildROBDD' expr _ [] | expr == Bvalue True = (1, [])
                    | expr == Bvalue False = (0, [])
                   
buildROBDD' expr n (x:xs)  = if n1 /= n2 then (n, (n, (x, n1, n2)) : lnds ++ rnds) else (n, lnds ++ rnds)
                          where (n1, lnds) = buildROBDD' (restrict expr x False) (2 * n) xs
                                (n2, rnds) = buildROBDD' (restrict expr x True) (2 * n + 1) xs

buildROBDD' _ _ [] = error "ups... something has gone wrong in the buildROBDD" 

-- Задача 7 -----------------------------------------
fullBexp :: String -> Maybe BExp 
fullBexp s = do (expr, rest) <- bexp s
                if null rest 
                then return expr
                else Nothing    

bexp :: String -> Maybe (BExp,String)
bexp s = do res <- bcon s
            manyCon res

bcon :: String -> Maybe (BExp,String)
bcon s = do res <- bdis s
            manyDis res

manyCon :: (BExp,String) -> Maybe (BExp,String)
manyCon (expr, s) | null s = Just (expr, s)
                  | head s == '|' = do (expr1, rest) <- bcon (tail s)
                                       manyCon (Or expr expr1, rest) 
                  | otherwise = Just (expr, s)

bdis :: String -> Maybe (BExp,String)
bdis "" = Nothing
bdis (x:xs) | isLower x = Just (Bvar x, xs)  
            | x == '!' = (\(p,k) -> (Not p, k)) <$> bdis xs   
            | x == 'T' = Just (Bvalue True, xs)
            | x == 'F' = Just (Bvalue False, xs)
            | x == '(' = do (expr,rest) <- bexp xs
                            newRest <- (\r -> if null r || head r /= ')' then Nothing else Just (tail r)) rest
                            return (expr, newRest)       
            | otherwise = Nothing

manyDis :: (BExp,String) -> Maybe (BExp,String)
manyDis (expr, s) | null s = Just (expr, s)
                  | head s == '&' = do (expr1, rest) <- bdis (tail s)
                                       manyDis (And expr expr1, rest) 
                  | otherwise = Just (expr, s)


------------------------------------------------------
-- Приклади для тестування..
bs1, bs2, bs3, bs4, bs5, bs6, bs7, bs8, bs9 :: String
bs1 = "F"
bs2 = "!(x&(F|y))"
bs3 = "u&T"
bs4 = "d&(x|!y)"
bs5 = "!(d&(x|!y))"
bs6 = "u&x|y&z" 
bs7 = "!y|(x|!e)"
bs8 = "u|!u"
bs9 = "z&(y|!y&x)"

b1, b2, b3, b4, b5, b6, b7, b8, b9 :: BExp
b1 = Bvalue False
b2 = Not (And (Bvar 'x') (Or (Bvalue False) (Bvar 'y')))
b3 = And (Bvar 'u') (Bvalue True)
b4 = And (Bvar 'd') (Or (Bvar 'x') (Not (Bvar 'y')))
b5 = Not (And (Bvar 'd') (Or (Bvar 'x') (Not (Bvar 'y'))))
b6 = Or (And (Bvar 'u') (Bvar 'x')) (And (Bvar 'y') (Bvar 'z'))
b7 = Or (Not (Bvar 'y')) (Or (Bvar 'x') (Not (Bvar 'e')))
b8 = Or (Bvar 'u') (Not (Bvar 'u'))
b9 = And (Bvar 'z') (Or (Bvar 'y') (And (Not (Bvar 'y')) (Bvar 'x')))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8, bdd9 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(2,('x',4,5)),(4,('y',1,1)),(5,('y',1,0))])
bdd3 = (5,[(5,('u',0,1))])
bdd4 = (2,[(2,('x',4,5)),(4,('y',8,9)),(8,('d',0,1)),(9,('d',0,0)),
           (5,('y',10,11)),(10,('d',0,1)),(11,('d',0,1))])
bdd5 = (3,[(4,('y',8,9)),(3,('x',4,5)),(8,('d',1,0)),(9,('d',1,1)),
           (5,('y',10,11)),(10,('d',1,0)),(11,('d',1,0))])
bdd6 = (2,[(2,('u',4,5)),(4,('x',8,9)),(8,('y',16,17)),(16,('z',0,0)),
           (17,('z',0,1)),(9,('y',18,19)),(18,('z',0,0)),(19,('z',0,1)),
           (5,('x',10,11)),(10,('y',20,21)),(20,('z',0,0)),(21,('z',0,1)),
           (11,('y',22,23)),(22,('z',1,1)),(23,('z',1,1))])
bdd7 = (6,[(6,('x',4,5)),(4,('y',8,9)),(8,('e',1,1)),(9,('e',1,0)),
           (5,('y',10,11)),(10,('e',1,1)),(11,('e',1,1))])
bdd8 = (2,[(2,('u',1,1))])
bdd9 = (2,[(2,('x',4,5)),(4,('y',8,9)),(8,('z',0,0)),(9,('z',0,1)),(5,('y',10,11)),(10,('z',0,1)),(11,('z',0,1))])



