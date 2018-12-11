doubleSmallNumber x = if x > 100
                    then x
                    else x*2

main = putStrLn (show(doubleSmallNumber 5))
       