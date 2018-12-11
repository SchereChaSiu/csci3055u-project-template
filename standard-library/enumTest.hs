main = do
    print $ succ 'a' -- successor of value
    -- > 'b'
    print $ pred 'b' -- predecessor of value
    -- > 'a'
    print $ (toEnum 65 :: Char) -- convert from an int to a char
    -- > 'A'
    print $ fromEnum 'B' -- convert to an int
    -- > 66

    print $ take 10 $ enumFrom 'a' -- take the first 10 char starting from 'a'
    print $ take 10 $ ['a'..] -- take the first 10 char starting from 'a'
    -- > "abcdefghij"

    print $ take 10 $ enumFromThen 'a' 'c' -- take the first 10 characters with the pattern with 'a' and 'c' (so every other character)
    print $ take 10 $ ['a', 'c'..] -- take the first 10 characters with the pattern with 'a' and 'c' (so every other character)
    -- > "acegikmoqs"

    print $ enumFromTo 'a' 'e' -- sequence of values with the ranges 
    print $ ['a'..'e'] -- sequence of values with the ranges
    -- > "abcde" 

    print $ enumFromThenTo 'a' 'c' 's' -- sequence of every other value stopping at 's'
    print $ ['a', 'c'..'s'] -- sequence of every other value stopping at 's'
    -- > "acegikmoqs"