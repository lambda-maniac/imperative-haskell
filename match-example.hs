import Imperative

calculate :: String -> Float -> Float -> IO Float
calculate operation a b =
    match operation const [
        "add" =>> (+),
        "sub" =>> (-),
        "mul" =>> (*),
        "div" =>> (/)
    ] >>= (\op -> return $ op a b)
