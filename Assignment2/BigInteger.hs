module BigInteger where 
    import Data.Char
    
    fromString:: String -> [Int]
    toString:: [Int] -> String
    add:: String -> String -> Int -> String
    negate_l:: [Int] -> [Int]
    removeTrailingZero:: [Int] -> [Int]

    negate_l num = map (\x -> -1 * x) num

    removeTrailingZero num =
        case num of
            [] -> [0]
            x:xs -> if x == 0 then removeTrailingZero xs else x:xs


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
            
    toString [] = "0"
    toString digits = 
        let 
            sgn = if length ((filter (\x -> x < 0) digits)) > 0 then "-" else ""
            val = map abs (removeTrailingZero (reverse digits))
        in 
            sgn ++ (map intToDigit val) 
    
    -- add_h:: [Int] -> [Int] -> Int -> [Int]
    -- add_h [] [] 0 = []
    -- add_h [] [] c = [c]
    -- add_h num [] c = (mod ((head num) + c) 10):(add_h (tail num) [] (div ((head num) + c) 10))
    -- add_h [] num c = (mod ((head num) + c) 10):(add_h [] (tail num) (div ((head num) + c) 10))

    -- add_h num1 num2 c = 
    --     let 
    --         d = (head num1) + (head num2) + c
    --     in
    --         (mod d 10):(add_h (tail num1) (tail num2) (div d 10))

    -- add num1 num2 = toString (add_h (fromString num1) (fromString num2) 0)



    add_h:: [Int] -> [Int] -> Int -> Int -> ([Int], Int)
    -- adds two integers in given in list of digits format, LSD = head of list
    add_h [] [] c _ = ([], c)
    add_h num [] c b = 
        let
            x = (head num) + c + b
            d = mod x b
            new_c = (div x b) - 1 
            (result, carry) = add_h (tail num) [] new_c b
        in
            (d:result, carry)

    add_h [] num c b = 
        let
            x = (head num) + c + b
            d = mod x b
            new_c = (div x b) - 1 
            (result, carry) = add_h [] (tail num) new_c b 
        in
            (d:result, carry)



    add_h num1 num2 c b = 
        let 
            x = (head num1) + (head num2) + c + b
            d = mod x b
            new_c = (div x b) - 1 
            (result, carry) = add_h (tail num1) (tail num2) new_c b 
        in
            (d:result, carry)
    
    
    add num1 num2 base = 
        let 
            (result, carry) = add_h (fromString num1) (fromString num2) 0 base
        in
            if carry == 0 then toString result
            else if carry > 0 then toString (result ++ [carry])
            else 
                let
                    extra = (map (\x -> 0) result) ++ [-carry]
                    (final_res,_) = (add_h extra (negate_l result) 0 base)
                in toString (negate_l final_res)
            
                







    




    

     
    
            
