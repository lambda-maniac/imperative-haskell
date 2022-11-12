import Imperative

printDayOfTheWeek :: Int -> IO ()
printDayOfTheWeek n =
    switch n [
        1 <=> putStrLn "Sunday",
        2 <=> putStrLn "Monday",
        3 <=> putStrLn "Tuesday",
        4 <=> putStrLn "Wednesday",
        5 <=> putStrLn "Thursday",
        6 <=> putStrLn "Friday",
        7 <=> putStrLn "Saturday"
    ]

-- λ> foreach [1 .. 7] printDayOfTheWeek
-- Sunday
-- Monday
-- Tuesday
-- Wednesday
-- Thursday
-- Friday
-- Saturday
