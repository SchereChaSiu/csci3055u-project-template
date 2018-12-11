import qualified Data.Map as Map

phoneBook = Map.fromList [(1992, "Gajan"), (2018, "Sivanesan")]

main = do
    print phoneBook -- print map
    -- >  [(1992,"Gajan"),(2018,"Sivanesan")]
    print $ Map.lookup 1992 phoneBook -- output element with matching key
    -- > "Gajan"
    print $ (Map.empty :: Map.Map Int Int) -- empty map
    -- > []
    print $ Map.singleton 1 2 -- >  a mapt with a single element 
    -- > [(1,2)]
    print $ Map.insert 4 "abc" Map.empty -- > insert in map, if arleady there replace the element
    -- > [(4,"abc")]
    print $ Map.null phoneBook -- checks if map is empty 
    -- > False
    print $ Map.size phoneBook -- size of the map 
    -- > 2
    print $ Map.toList phoneBook -- convert to a list of key/value pairs
    -- > [(1992,"Gajan"),(2018,"Sivanesan")]
    print $ Map.keys phoneBook -- find the keys of the map 
    -- > [1992,2018]
    print $ Map.elems phoneBook -- find the elements of the map 
    -- > ["Gajan","Sivanesan"]