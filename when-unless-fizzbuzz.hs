import Imperative

fizzbuzz :: [Int] -> IO ()
fizzbuzz xs =
    foreach xs $ \n ->
        let
            isFizz    = n `mod` 3 == 0
            isBuzz    = n `mod` 5 == 0
            n'        = show n
            anyOfThem = isFizz || isBuzz
        in
        putStr "fizz"  `when`  isFizz    >>
        putStr "buzz"  `when`  isBuzz    >>
        putStr   n'   `unless` anyOfThem >>
        putStrLn ""
