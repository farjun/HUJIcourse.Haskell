import Control.Monad


-- ############################### Q1 #########################################
for :: a -> (a -> Bool) -> (a -> a) -> (a -> IO ()) -> IO ()
for x f g h = do 
    (h x) 
    when (f(g x) == True) $ for (g x) f g h

-- ################################## Q2 #########################################
data Bin = Endzero | Endone | Zero Bin | One Bin deriving (Show)

addB :: Bin -> Bin -> Bin
addB Endzero Endzero = Endzero
addB Endzero Endone = Endone
addB Endone Endzero = Endone
addB Endone Endone = Zero $ Endone
addB (Zero x) (One y) = One $ addB x y
addB (Zero x) (Zero y) = Zero $ addB x y
addB (One x) (Zero y) = One $ addB x y
addB (One x) (One y) =  addB (Zero $ addB x y) (Zero Endone)
addB (Zero x) Endzero = Zero $ addB x Endzero
addB (Zero x) Endone = One $ addB x Endzero
addB (One x) Endzero = One $ addB x Endzero
addB (One x) Endone = Zero $ addB x Endone
addB Endzero (Zero y) = Zero $ addB Endzero y
addB Endone (Zero y) = One $ addB Endzero y
addB Endzero (One y) = One $ addB Endzero y
addB Endone (One y) = One $ addB Endone y

-- example use: addB (Zero $ One $ One $ One $ One $ Zero $ Endone) (One $ One $ One $ Zero $ Endone)

-- ############################### Q3 #########################################
data Elements a = Value a | List [Elements a]


flatten :: Elements a -> [a]
flatten (Value x) = [x]
flatten (List x) = concat $ map flatten x


