import Data.List
import Control.Monad

factorial:: Int -> Int
factorial n = if n>1 then n*factorial(n-1) else 1

data Point = Point Double Double deriving (Eq,Show)

getX:: Point-> Double
getX (Point x y) = x

getY:: Point-> Double
getY (Point x y) = y

data TreeNode = Leaf Int | OneNode TreeNode Int | TwoNode TreeNode TreeNode Int deriving(Eq,Show)

treesum :: TreeNode -> Int
treesum (Leaf x) = x
treesum (OneNode t i) = treesum(t)+i
treesum (TwoNode t r i) = treesum(t)+treesum(r) + i  

doStuff:: [Int] -> [Int]
doStuff [] = [1]
doStuff a = map (+100) a

filterMyList :: (Eq a) =>[a] -> [a]
filterMyList [] = []
filterMyList a = filter (== head a) a


myGroup::(Eq a)=>[a] -> [a]
myGroup [] = []
myGroup (x:xs) = x : takeWhile (== x) xs ++ myGroup (dropWhile (== x) xs)


data BoolNode = OrNode BoolNode BoolNode | AndNode BoolNode BoolNode | NotNode BoolNode | BoolLeaf Bool deriving(Eq,Show)

eval:: BoolNode -> Bool
eval (BoolLeaf a) = a
eval (NotNode a) = not (eval a)
eval (AndNode a b) = (eval a) && (eval b)
eval (OrNode a b) = (eval a) || (eval b)

for :: a -> (a -> Bool) -> (a -> a) -> (a -> IO ()) -> IO ()
for x f g h = do 
    (h x) 
    when (f(g x) == True) $ for (g x) f g h 



-- data Binary = One Binary | Zero Binary | OneEnd | ZeroEnd
-- binaryAdd:: Binary -> Binary -> Binary
-- binaryAdd ZeroEnd ZeroEnd = ZeroEnd
-- binaryAdd ZeroEnd OneEnd = OneEnd
-- binaryAdd OneEnd ZeroEnd = OneEnd
-- binaryAdd OneEnd OneEnd = One ZeroEnd
-- binaryAdd (Zero a) OneEnd = OneEnd

goldbach a = head $
                     filter (\(x,y) -> isPrime x && isPrime y) $
                     map (\e -> (e, a - e)) [1,3..a `div` 2]
 where
 factors a = filter (isFactor a) [2..a-1]
 isFactor a b = a `mod` b == 0
 isPrime a = null $ factors a


 goldbach2 a = head (filter (\(x,y) -> isPrime2 x && isPrime2 y) (map (\e -> (e, a - e)) [1,3..a `div` 2]))
  where
 factors2 a = filter (isFactor a) [2..a-1]
 isFactor2 a b = a `mod` b == 0
 isPrime2 a = null $ factors a