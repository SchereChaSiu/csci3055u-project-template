import Data.Tree

tree = Node "A" [Node "B" [], Node "C" [Node "D" [], Node "E" []]] -- create tree

main = do
    print tree -- print tree
    putStrLn $ drawTree tree -- draws the tree 
    {- A
|
+- B
|
`- C
   |
   +- D
   |
   `- E
-}
    putStrLn $ drawForest $ subForest tree -- draws the forests and sub forests
    {- B

C
|
+- D
|
`- E -}

    print $ flatten tree -- flattens the tree 
    -- > ["A","B","C","D","E"]
    print $ levels tree -- level of tree
    -- >[["A"],["B","C"],["D","E"]]