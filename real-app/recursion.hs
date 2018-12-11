printStringNTimes 0 = return 90
printStringNTimes n =
    do
        putStrLn "a string"
        printStringNTimes (n-1)

main = printStringNTimes 10