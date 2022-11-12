import Imperative

fizzbuzz :: [Int] -> IO ()
fizzbuzz xs =
    foreach xs $ \n ->
        let
            isFizz = n `mod` 3 == 0
            isBuzz = n `mod` 5 == 0
            isBoth = isFizz && isBuzz
        in
        if_ (isBoth) (
            putStrLn "fizzbuzz"
        ) $ if_ (isFizz) (
            putStrLn "fizz"
        ) $ if_ (isBuzz) (
            putStrLn "buzz"
        ) $
            putStrLn $ show n
