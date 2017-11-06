group :: (Eq a) => [a] -> [[a]]
group [] = []
group (x:xs) =  (x:takeWhile(== x) xs) : 
	if xs == []
		then []
		else group (dropWhile(==x) xs)
