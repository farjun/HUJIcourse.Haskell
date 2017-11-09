-- =============================== Group ===========================================
myGroup :: (Eq a) => [a] -> [[a]]
myGroup [] = []
myGroup (x:xs) =  (x:takeWhile(== x) xs) : if xs == []
   then []
   else myGroup (dropWhile(==x) xs)

-- ============================= look and say ====================================
las :: (String, Int, Char) -> Char -> (String,Int,Char)
las (xs,0,'0') x = (xs,1,x)
las (xs,n,y) x = if x == y then (xs,n+1,y) else (xs ++ (show n) ++ (y:""),1,x)

lookAndSay :: String -> String
lookAndSay str = display $ foldl las ("",0,'0') str

display (xs,n,y) = xs ++ (show n) ++ (y:"")

findNVar :: Int -> String
findNVar num = iterate lookAndSay "1" !! (num-1)
   
-- ================================== Histogram ====================================

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (==x)

counts :: (Eq a) => [a] -> [a] -> [Int]
counts list1 list2 = map (flip count list2) list1

histogram :: [Char] -> [Int]
histogram s = counts "0123456789" s

lookAndSayHist n = histogram $ findNVar n 
-- ==================================== Tree =======================================

data TreeNode = Leaf Bool | NotNode (TreeNode) | AndNode [TreeNode] | OrNode [TreeNode] 
                deriving (Eq, Show)

constAndNode :: [TreeNode] -> TreeNode
constAndNode l | (length l) < 2 = error "Invalid list size" 
               | otherwise = AndNode l

constOrNode :: [TreeNode] -> TreeNode
constOrNode l | (length l) < 2 = error "Invalid list size" 
              | otherwise = OrNode l

evalTreeNodeList :: [TreeNode] -> Bool -> Bool
evalTreeNodeList [] isAnd = isAnd
evalTreeNodeList (l:ls) isAnd | isAnd = eval l && evalTreeNodeList ls isAnd
                              | otherwise = eval l || evalTreeNodeList ls isAnd

eval :: TreeNode -> Bool
eval (Leaf b) = b
eval (NotNode t) = not (eval t)
eval (OrNode l) = evalTreeNodeList l False
eval (AndNode l) = evalTreeNodeList l True
