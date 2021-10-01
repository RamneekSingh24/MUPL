module BigInt (add) where
    fromString:: String -> [Int]
    toString:: [Int] -> String
    add:: String -> String -> Int -> String
    sub:: String -> String -> Int -> String
    mul::String -> String -> Int -> String
    karatsuba:: [Int] -> [Int] -> Int -> [Int]

    fromString s = map digitToInt (reverse s)
    toString digits = map 

