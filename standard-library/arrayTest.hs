import Data.Array

myArray = array (1, 3) [(1, "a"), (2, "b"), (3, "c")]

main = do
    print myArray -- prints the array
    -- > array (1,3) [(1,"a"),(2,"b"),(3,"c")]
    print $ myArray ! 2 -- value at the given index
    -- > "b"
    print $ bounds myArray -- bounds with which the array is constructed
    -- > (1,3)
    print $ indices myArray -- list of indecies in ascending order
    -- > [1,2,3]
    print $ elems myArray -- list of elements in index order
    -- > ["a","b","c"]
    print $ assocs myArray -- list of associations of an array in index order
    -- > [(1,"a"),(2,"b"),(3,"c")]