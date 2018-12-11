data DayOfWeek
    = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday 
    deriving (Eq, Enum, Bounded)

data Month 
    = Janurary | Feburary | March | April | May | June | July | August | September |
    October | November | December
    deriving (Enum, Bounded, Show)

next :: (Eq a, Enum a, Bounded a) => a -> a 
next x | x == maxBound = minBound
       | otherwise     = succ x

pad :: Int -> String
pad day = case show day of 
    [c] -> ['  ', c]
    cs -> cs 

month :: Month -> DayOfWeek -> Int -> String
month m startDay maxDay = show m++ "2018\n" ++ week ++ spaces Sunday 
    where 
        week  = "Su Mo Tu We Th Fr Sa\n"

        spaces currDay | startDay == currDay = days startDay 1
                       | otherwise           = "   " ++ spaces (next currDay)
                       
        days Sunday   n | n > maxDay = "\n"
        days _        n | n > maxDay = "\n\n"
        days Saturday n              = pad n ++ "\n" ++ days Sunday (succ n)
        days day      n              = pad n ++ " " ++ days (next day) (succ n)

year = month Janurary Monday 31
    ++ month Feburary Thursday 28
    ++ month March Thursday 31
    ++ month April Sunday 30
    ++ month May Tuesday 31
    ++ month June Friday 30
    ++ month July Sunday 31
    ++ month August Wednesday 31
    ++ month September Sunday 30
    ++ month October Monday 31
    ++ month November Thursday 30
    ++ month December Saturday 31

main = putStr year