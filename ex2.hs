import Data.Char (digitToInt)
import Data.List (foldl')
import Data.Bits
data Bin = One Bin | Zero Bin | LSB

addWithCarry :: Bool -> Bool -> (Bool, Bool)
addWithCarry x y = (x /= y, x && y)


-- numbers are represented LSB first e.g. 6 is [False,True,True]
addBitsWithCarry :: [Bool] -> [Bool] -> Bool -> [Bool]
-- when we ran out of digits on both args:
addBitsWithCarry []     []     False = []
addBitsWithCarry []     []     True  = [True]
-- when we ran out of digits on one arg, add a zero there:
addBitsWithCarry a      []     c     = addBitsWithCarry a       [False] c
addBitsWithCarry []     b      c     = addBitsWithCarry [False] b       c
-- some digits on both sides
addBitsWithCarry (x:xs) (y:ys) c     = xor (xor x y) c : addBitsWithCarry xs ys (y&&x)
         -- where (z,c') = addWithCarry ???

main = do print (addBitsWithCarry [True, True, False] [False, True, False] False)
