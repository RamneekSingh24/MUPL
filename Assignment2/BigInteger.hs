module BigInteger where 
    import Data.Char
    fromString:: String -> [Int]
    toString:: [Int] -> String
    fromString num = 
        case num of
            [] -> []
            _ ->
                let
                    sgn = if (head num) == '-' then -1 else 1
                    val = if sgn == -1 then (tail num) else num
                    check = foldl (\x y -> (x && y)) True (map isDigit val)
                in
                    if check then reverse (map (\x -> (digitToInt x) * sgn ) val)
                    else error "not a number" -- TODO: throw exception
            
    toString [] = ""
    toString digits = 
        let 
            sgn = if (head digits) < 0 then "-" else ""
            val = map abs (reverse digits)
        in 
            sgn ++ (map intToDigit val) 
    
    add_h:: [Int] -> [Int] -> Int -> [Int]
    add_h [] [] 0 = []
    add_h [] [] c = [c]
    add_h num [] c = (mod ((head num) + c) 10):(add_h (tail num) [] (div ((head num) + c) 10))
    add_h [] num c = (mod ((head num) + c) 10):(add_h [] (tail num) (div ((head num) + c) 10))

    add_h num1 num2 c = 
        let 
            d = (head num1) + (head num2) + c
        in
            (mod d 10):(add_h (tail num1) (tail num2) (div d 10))

    add num1 num2 = toString (add_h (fromString num1) (fromString num2) 0)


    

     
    
            
