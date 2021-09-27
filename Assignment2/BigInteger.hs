module BigInteger (add, mul) where
    import Data.Char
    
    fromString:: String -> [Int]
    toString:: [Int] -> String
    negate_l:: [Int] -> [Int]
    removeTrailingZero:: [Int] -> [Int]
    sign::[Int] -> Int
    abs_l::[Int] -> [Int] 

    abs_l num = map abs num

    sign num = if length ((filter (\x -> x < 0) num)) > 0 then -1 else 1

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
            sgn = if sign digits == -1 then "-" else ""
            val = map abs (removeTrailingZero (reverse digits))
        in 
            sgn ++ (map intToDigit val) 
    


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
    
    add_list:: [Int] -> [Int] -> Int -> [Int]
    
    add_list num1 num2 b = 
        fromString (add (toString num1) (toString num2) b)

    add:: String -> String -> Int -> String
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



    rightShift:: [Int] -> Int -> [Int]
    rightShift num x = (map (\x -> 0) [1..x]) ++ num


    padZeroes:: [Int] -> Int -> [Int]
    padZeroes num len = 
        if len < (length num) then error "error"
        else num ++ (map (\x -> 0) [1..(len - (length num))])


    nhp_two:: Int -> Int -> Int
    nhp_two x p2 = if p2 >= x then p2 else nhp_two x (p2*2)
    
    prepare:: ([Int],[Int]) -> ([Int],[Int])
    prepare (a,b) = 
        let 
            len = max (length a) (length b)
            pad_len = nhp_two len 1
        in
            (padZeroes a pad_len, padZeroes b pad_len)


    mul:: String -> String -> Int -> String

    karatsuba:: [Int] -> [Int] -> Int -> [Int]
    --assumes +ve
    karatsuba [x] [y] b = if (x * y) < b then [x * y] else [mod (x*y) b, div (x*y) b]
    karatsuba num1 num2 base = 
        let 
            (x, y) = prepare (num1, num2)
            n = length x
            half_len = div n 2
            xr = take half_len x
            xl = drop  half_len x
            yr = take half_len y
            yl = drop half_len y
            xlyl = karatsuba xl yl base  --1
            xryr = karatsuba xr yr base  --2
            xlpxr = add_list xl xr base
            ylpyr = add_list yl yr base
            cross_p = karatsuba xlpxr ylpyr base  --3
            t1 = rightShift xlyl n
            t2 = rightShift (add_list cross_p (negate_l (add_list xlyl xryr base)) base) half_len
            xy = add_list t1 (add_list t2 xryr base) base
        in
            xy
    
    mul a b base =
        let 
            num1 = fromString a
            sgn1 = sign num1
            num2 = fromString b
            sgn2 = sign num2
            prod = karatsuba (abs_l num1) (abs_l num2) base
            sgn = sgn1 * sgn2
            result = if sgn == 1 then prod else negate_l prod
        in
            toString result




    
            
                







    




    

     
    
            
