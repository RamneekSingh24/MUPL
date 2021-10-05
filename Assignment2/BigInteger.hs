-- TODO: add exception as per specification

module BigInteger (add, mul, karatsuba) where
    import Data.Char
    fromString:: String -> [Int]
    toString:: [Int] -> String
    add:: String -> String -> Int -> String
    mul::String -> String -> Int -> String
    karatsuba:: [Int] -> [Int] -> Int -> [Int]

    fromString s = map digitToInt (reverse s)

    removeTrailingZero:: String -> String
    removeTrailingZero "" = "0"
    removeTrailingZero x = if (head x) == '0' then removeTrailingZero (tail x) else x

    toString digits = removeTrailingZero (map intToDigit (reverse digits))

    prepare:: ([Int], [Int]) -> ([Int], [Int])

    prepare (x, y) = 
        let 
            lx = length x
            ly = length y
            diff = abs (lx - ly)
        in
            if lx > ly then (x, (y ++ (map (\x -> 0) [1..diff])))
            else ((x ++ (map (\x -> 0) [1..diff]), y))


    add_list:: [Int] -> [Int] -> Int -> [Int]

    add_list_h:: [Int] -> [Int] -> [Int] -> Int -> Int -> [Int]

    add_list_h [] [] curr c b = if c == 0 then reverse curr else reverse (c:curr)

    add_list_h x y curr c b =
        let
            d1 = head x
            d2 = head y
            sum = d1 + d2 + c
            d = if sum >= b then sum - b else sum
            new_c = if sum >= b then 1 else 0
        in  
            add_list_h (tail x) (tail y) (d:curr) new_c b
    
    add_list num1 num2 b =
        let
            (x, y) = prepare(num1, num2)
        in
            add_list_h x y [] 0 b

    add x y b = 
        toString (add_list (fromString x) (fromString y) b)


    sub_list:: [Int] -> [Int] -> Int -> [Int]
    sub_list_h:: [Int] -> [Int] -> [Int] -> Int -> Int -> [Int]


    sub_list_h [] [] curr c b = if c == 0 then reverse curr else error "invalid subtract"

    sub_list_h x y curr c b = 
        let 
            d1 = head x
            d2 = head y
            s = d1 - d2 + c
            d = if s < 0 then b + s else s
            new_c = if s < 0 then -1 else 0
        in  
            sub_list_h (tail x) (tail y) (d:curr) new_c b

    sub_list num1 num2 b =
        let 
            (x, y) = prepare(num1, num2)
        in 
            sub_list_h x y [] 0 b


    rightShift:: [Int] -> Int -> [Int]
    rightShift x n = 
        (map (\x -> 0) [1..n]) ++ x

    karatsuba [x] [y] b = 
        let
            m = x * y
        in 
            [(mod m b), (div m b)]

    karatsuba num1 num2 b = 
        let 
            (x, y) = prepare(num1, num2)
            n = length x
            ln = div n 2
            rn = n - ln
            xl = drop rn x
            xr = take rn x
            yl = drop rn y
            yr = take rn y
            xlyl = karatsuba xl yl b
            xryr = karatsuba xr yr b
            cross_p = karatsuba (add_list xl xr b) (add_list yl yr b) b
            t1 = rightShift (sub_list cross_p (add_list xlyl xryr b) b) rn
            t2 = add_list (rightShift xlyl (2 * rn)) xryr b
        in
            add_list t1 t2 b


    mul a b base =
        let 
            (x, y) = prepare ((fromString a), (fromString b))
        in
            toString (karatsuba x y base)
        
            




        


    


        
