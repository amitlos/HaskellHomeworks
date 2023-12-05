{-# OPTIONS_GHC -Wall #-}
module Solovei10 where

--  Пакет parsec може бути закритим (hidden), 
--  щоб відкрити його потрібно завантажити файл з опцією -package parsec
--  ghci xxxx.hs -package parsec // ghc xxxx.hs -package parsec 

import Text.Parsec.String
import Text.Parsec   --- parse

data Recur = Zero | Succ | Sel Int Int
           | Super Recur [Recur]
           | Prim Recur Recur
           | Mini Recur Int
           | Name String  deriving (Show, Eq)
type System = [(String,Recur)]

-- Задача 1 ------------------------------------
isNumbConst :: System -> Recur -> Bool
isNumbConst _ (Zero) = True
isNumbConst syst (Name fname) = case funcSel syst fname of
                                Just defFunc -> isNumbConst syst defFunc
                                Nothing -> False
isNumbConst syst (Super Succ lst) = (null (tail lst)) && (isNumbConst syst (head lst))
isNumbConst _ _ = False


funcSel :: System -> String -> Maybe Recur
funcSel syst fname = let recurs = [ funcdef | (sfname, funcdef) <- syst, sfname == fname] in (if (length recurs) /= 1 then Nothing else (Just (head recurs)))

hasCorrDef :: System -> String -> Bool
hasCorrDef syst fname = case funcSel syst fname of
                                Just _ -> True
                                Nothing -> False

-- Задача 2 ------------------------------------
evRank :: System -> Recur -> Int
evRank _ Zero = 1
evRank _ Succ = 1
evRank _ (Sel n _) = n
evRank syst (Super _ flst) = evRank syst (head flst)
evRank syst (Prim _ h) = (evRank syst h) - 1
evRank syst (Mini f _) = (evRank syst f) - 1
evRank syst (Name fname) = case funcSel syst fname of
                                Just defFunc -> evRank syst defFunc
                                Nothing -> 0

-- Задача 3 ------------------------------------
isNames :: System -> Bool
isNames syst = (isUniq syst) && (isCorrDef syst)


isUniq :: System -> Bool
isUniq syst = allUniq [fname | (fname, _) <- syst]

isCorrDef :: System -> Bool
isCorrDef [] = True
isCorrDef syst = ((snd (last syst)) `canBeFuncIn` (init syst)) && (isCorrDef (init syst))

canBeFuncIn :: Recur -> System -> Bool
canBeFuncIn Zero _ = True
canBeFuncIn Succ _ = True
canBeFuncIn (Sel _ _) _ = True
canBeFuncIn (Name fname) syst = hasCorrDef syst fname
canBeFuncIn (Super f glist) syst = (f `canBeFuncIn` syst) && (and [(canBeFuncIn g syst) | g <- glist])
canBeFuncIn (Prim g h) syst = (g `canBeFuncIn` syst) && (h `canBeFuncIn` syst)
canBeFuncIn (Mini f _) syst = f `canBeFuncIn` syst

allUniq :: Eq a => [a] -> Bool
allUniq [] = True
allUniq [_] = True
allUniq (x:xs) = (notElem x xs) && (allUniq xs)
-- Задача 4 ------------------------------------
isRecur :: System -> Recur -> Bool
isRecur syst func = canBeFuncIn func syst

-- Задача 5 ------------------------------------
eval :: System -> Recur -> [Int] -> Int
eval _ Zero _ = 0
eval _ Succ args = (head args) + 1
eval _ (Sel _ k) args = if k > length args then -1 else args !! (k - 1)
eval syst (Super f gs) args = eval syst f [eval syst g args| g <- gs]  

eval syst (Prim g h) args | last args == 0 = eval syst g (init args)
                          | otherwise = let preargs = init args ++ [last args - 1] in eval syst h (preargs ++ [eval syst (Prim g h) preargs]) 
eval syst (Mini f t) args = evalMini syst (Mini f t) args 0
eval syst (Name fname) args = case funcSel syst fname of
                        Just func -> eval syst func args
                        Nothing -> 0 

evalMini :: System -> Recur -> [Int] -> Int -> Int
evalMini syst (Mini f t) args t0 | t0 > t = 0
                                 | eval syst f (args ++ [t0]) == 0 = t0 
                                 | otherwise = evalMini syst (Mini f t) args (t0 + 1)
evalMini _ _ _ _ = 0 

-- Задача 6 ------------------------------------
evalPart :: System -> Recur -> [Int] -> Maybe Int
evalPart _ Zero _                     = Just 0
evalPart _ Succ args                  = if length args == 1
                                        then Just $ head args + 1
                                        else error "Illegal expression: Succ"
evalPart _ (Sel n k) args             = if n > 0 && k > 0 && k <= n && length args == n
                                        then Just $ args !! (k - 1)
                                        else error "Illegal expression: Sel"
evalPart system fun@(Super f al) args = if isRecur system fun
                                            && evRank system fun == length args
                                        then evalPart system f
                                            [eval system recur args | recur <- al]
                                        else error "Illegal expression: Super"
evalPart system fun@(Prim g h) args   = if isRecur system fun
                                            && evRank system fun == length args
                                        then case larg of
                                                 0 -> evalPart system g args'
                                                 _ -> case recurArg of
                                                          Just rec ->
                                                              evalPart system h (args'' ++ [rec])
                                                          Nothing  -> Nothing
                                        else error "Illegal expression: Prim"
    where larg     = last args
          args'    = init args
          args''   = args' ++ [larg']
          larg'    = pred larg
          recurArg = evalPart system fun args''
evalPart system fun@(Mini g k) args   = if isRecur system fun
                                            && evRank system fun == length args
                                        then findZero $ map (evalPart system g) [args ++ [t] | t <- [0..k]]
                                        else error "Illegal expression: Mini"
    where findZero :: [Maybe Int] -> Maybe Int
          findZero [] = Nothing
          findZero (x:xs) = case x of
                                Nothing -> Nothing
                                Just 0  -> Just 0
                                _       -> findZero xs >>= return . succ
evalPart system (Name name) args      = case funcSel system name of
                                            Nothing -> Just (-1)
                                            Just function -> if isRecur system function
                                                             then evalPart system function args
                                                             else Just (-1)





-- Задача 7 ------------------------------------
parseRec :: String -> Maybe System
parseRec = undefined


---------------------Тестові дані -  -------
syst1, syst2 :: System
syst1 = [("const0", Zero)
   , ("const0v2", Super Zero [Sel 2 1])
   , ("const0v3", Super Zero [Sel 3 1])
   , ("const1v2", Super Succ [Super Zero [Sel 2 1]])
   , ("const2", Super Succ [Super Succ [Zero]])
   , ("addition", Prim (Sel 1 1) (Super Succ [Sel 3 3 ]))
   , ("multiplication", Prim Zero (Super (Name "addition") [Sel 3 3, Sel 3 1]))
   , ("notSignum", Prim (Super Succ [Zero]) (Super Zero [Sel 2 1]))
   , ("subtract1", Prim Zero (Sel 2 1))
   , ("subtraction", Prim (Sel 1 1) (Super (Name "subtract1") [Sel 3 3]))
   , ("subtractionRev", Super (Name "subtraction") [Sel 2 2, Sel 2 1])
   , ("subtractionAbs", Super (Name "addition") [Name "subtraction", Name "subtractionRev"])
   , ("subtractionAbs3", Super (Name "subtractionAbs") [Sel 3 1, Super (Name "addition") [Sel 3 2, Sel 3 3]])
   , ("subtractionPart", Mini (Name "subtractionAbs3") 100)
   ]

syst2 = [("f1", Super Succ [Zero])
        ,("f2", Super Succ [Name "f2"])
        ]


sysStr1,sysStr2 :: String
sysStr1 = " const0 = z1; const0v2  = (z1 : s21); const0v3 = (z1:s31);\n\
          \  const1v2 = (a1 : (z1 : s21));  \n\
          \  const2= (a1:(a1:z1)); addition = [s11, (a1:s33)] ;\n\
          \  multiplication = [z1 , (addition: s33,s31)]; \n\
	  \  notSignum = [(a1:z1),(z1:s21)];\n\
	  \  subtract1 = [z1,s21]; subtraction = [s11, (subtract1:s33)];\n\
	  \  subtractionRev = (subtraction : s22, s21);\n\
          \  subtractionAbs = (addition: subtraction, subtractionRev); \n\
          \  subtractionAbs3=(subtractionAbs:s31, (addition:s32,s33))  ;\n \
          \ subtractionPart = {subtractionAbs3, 100 };"

sysStr2 = " f1 = (a1:z1); f2 = (a1, f2);"
