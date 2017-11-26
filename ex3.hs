import Data.List

-- ############################ Q1 ###############################

goldbach a = head $
                     filter (\(x,y) -> isPrime x && isPrime y) $
                     map (\e -> (e, a - e)) [1,3..a `div` 2]
 where
 factors a = filter (isFactor a) [2..a-1]
 isFactor a b = a `mod` b == 0
 isPrime a = null $ factors a


-- ############################ Q2 ###############################

type Node = Int
type Edge = (Node,Node)
type Graph = ([Node],[Edge])
 
dfsbipartite :: Graph -> [(Node, Int)] -> [Node] -> [Node] -> Bool
dfsbipartite ([],_) _ _ _ = True
dfsbipartite (_,_) [] _ _ = True
dfsbipartite (v,e) ((nv, 0):stack) odd even
    | [x|x<-v,x==nv] == [] = dfsbipartite (v, e) stack odd even
    | [] == intersect adjacent even = dfsbipartite (newv, e) ([(x,1)|x<-adjacent] ++ stack) odd (nv : even)
    | otherwise = False
    where
        adjacent = [x | (x,y)<-e,y==nv] ++ [x | (y,x)<-e,y==nv]
        newv = [x|x<-v,x/=nv]
dfsbipartite (v,e) ((nv, 1):stack) odd even
    | [x|x<-v,x==nv] == [] = dfsbipartite (v, e) stack odd even    
    | [] == intersect adjacent odd = dfsbipartite (newv, e) ([(x,0)|x<-adjacent] ++ stack) (nv : odd) even
    | otherwise = False
    where
        adjacent = [x | (x,y)<-e,y==nv] ++ [x | (y,x)<-e,y==nv]
        newv = [x|x<-v,x/=nv]
 
bipartite :: Graph -> Bool
bipartite ([],_) = True
bipartite (top:v,e) = dfsbipartite (top:v, e) [(top,0)] [] []


-- ############################ Q3 ###############################


-- ############################ method 1 ###############################
-- map f = foldr ((:) . f) []
-- recives a function f and returns a functions that recives a list and operates f on that list

-- ############################ method 2 ###############################
-- filter pred = foldr ((++) . sel) []

--               where

--               sel x

--                 | pred x    = [x]

--                 | otherwise = []

 
-- recives a filter pred and does ++ only to args who return true to pred x or empty list.

-- ############################ method 3 ################################
-- reverse = foldl (flip (:)) []
-- flips the order of a given list. reverse is a function that recives a list
-- ############################ method 4 ################################
-- ldr op u xs = foldl (flip op) u (reverse xs)

--  concats the op(list) with the head of the given, the concates that with the rest of the flipped list (flip xs)
-- ############################ method 5 ################################

-- pascal = iterate (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [1]

--  
-- ############################ method 6 ################################

-- import Control.Monad

--          powerSet :: [a] -> [[a]]

--          powerSet = filterM $ const [True, False]

-- 
-- ############################ method 7 ################################


-- myInits = map reverse . scanl (flip (:)) []