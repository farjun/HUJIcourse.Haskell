--final grade = 87

--Question 1: didnt add Leafs in the quiz (it worked but the method didnt work without leafs)
-- my grade 27/30
data ColorTree = RED [ColorTree]| Blue [ColorTree] | Green [ColorTree] | REDLeaf | BlueLeaf | GreenLeaf


--Question 2: changed the method to working with leafs, same problem -4  plus changing t [a] -> x -2. 
-- my grade = 22/30

hasBlue:: ColorTree-> Bool
hasBlue(BlueLeaf) = True
hasBlue(REDLeaf) =  False
hasBlue(GreenLeaf) =  False
hasBlue(Blue x) =  True
hasBlue(RED x) =  foldr (||) False $ (map (hasBlue) x)
hasBlue(Green x) =  foldr (||) False $ (map (hasBlue) x)

hasGreen:: ColorTree-> Bool
hasGreen(BlueLeaf) = False
hasGreen(REDLeaf) =  False
hasGreen(GreenLeaf) =  True
hasGreen(Blue x) =  foldr (||) False $ (map (hasBlue) x)
hasGreen(RED x) =  foldr (||) False $ (map (hasBlue) x)
hasGreen(Green x) =  True

hasRed:: ColorTree-> Bool
hasRed(BlueLeaf) = False
hasRed(REDLeaf) =  True
hasRed(GreenLeaf) =  False
hasRed(Blue x) =  foldr (||) False $ (map (hasBlue) x)
hasRed(RED x) =  True
hasRed(Green x) =  foldr (||) False $ (map (hasBlue) x)

isGrey:: ColorTree -> Bool
isGrey(x) = hasBlue x && hasRed x && hasGreen x

--the definition i wrote was:
--let treeA = Blue [RED [RED [],Blue []],RED [RED [],Green [],Blue []]]
-- and it was ok so my grade = 28 / 30 
--let treeA = Blue [RED [REDLeaf,BlueLeaf],RED [REDLeaf,GreenLeaf,BlueLeaf]]

