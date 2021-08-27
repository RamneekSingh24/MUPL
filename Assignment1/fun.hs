


rev_tr ls = 
    let 
        rev_tr_h rem_ls curr_ls = 
            case rem_ls of
                [] -> curr_ls
                x:xs -> rev_tr_h xs (x:curr_ls)
    in 
        rev_tr_h ls []
            

-- >>> rev_tr [1,2,3]
-- [3,2,1]
-- >>>rev_tr [2]
-- []


merge_tr ls1 ls2 = 
    let 
        merge_tr_h rem1 rem2 curr = 
            case (rem1, rem2) of
                ([],_) -> curr ++ rem2
                (_,[]) -> curr ++ rem1
                (x:xs, y:ys) -> if x < y then merge_tr_h xs rem2 (curr ++ [x])
                                else merge_tr_h rem1 ys (curr ++ [y])
    in
        merge_tr_h ls1 ls2 []

--- >>> merge_tr [3,5,8] [1,2,6]
--- [1,2,3,5,6,8]
---

fib_tr n = 
    let 
        fib_tr_h prev2 prev1 i = 
            if i == n then (prev1 + prev2)
            else fib_tr_h (prev1) (prev1 + prev2) (i + 1)
    in
        if n <= 1 then n
        else fib_tr_h 0 1 2

--- >>> fib_tr 16
--- 987
--- >>> fib_tr 2
--- 0



insertion_sort_tr ls = 
    let
        insert sorted_ls e =
            case sorted_ls of
                [] -> [e]
                x:xs -> if e <= x then e:sorted_ls
                        else x:(insert xs e)
        helper sorted_ls rem = 
            case rem of 
                [] -> sorted_ls
                x:xs -> helper (insert sorted_ls x) xs
        
    in
        helper [] ls

-- >>> insertion_sort_tr [3,2,9,69,22,13,42,24,33,1,-1,2,2]
-- [-1,1,2,2,2,3,9,13,22,24,33,42,69]
-- >>> insertion_sort_tr [1,1,1,1,1,1,1,-6]
-- [-6,1,1,1,1,1,1,1]
--

quick_sort ls = 
    let
        split curr_ls pivot = 
            case curr_ls of
                [] -> ([], [])
                x:xs -> let (lr, gr) = split xs pivot
                        in if x <= pivot then (x:lr, gr)
                        else (lr, x:gr)
        
    in
        case ls of
            [] -> []
            x:[] -> [x]
            x:xs -> let pivot = x
                        (lr, gr) = split xs pivot
                    in 
                        (quick_sort lr) ++ [pivot] ++ (quick_sort gr)


-- >>> quick_sort [3,2,9,69,22,13,42,24,33,1,-1,2,2] 
-- [-1,1,2,2,2,3,9,13,22,24,33,42,69]
-- >>> quick_sort []
-- []
-- >>> [2,3]!!1
-- 3
--

binary_search ls e lo hi = 
    if lo == hi then e == ls!!lo
    else 
        let 
            mid = div (lo + hi) 2
        in
            if e <= ls!!mid
                then binary_search ls e lo mid
            else binary_search ls e (mid + 1) hi


-- >>> binary_search [2,3,4,5,6,7,10] 8 0 7
-- False
-- >>> binary_search [2,3,4,5,6,7,10] 6 0 7
-- True
--

--------------------------------------------------------------------
--- Problem 2


myRNG seed1 seed2 = 
    let 
        x = (seed1 * 1103569345 + seed2 * 49993101 + 3422313)
        y = (seed2 * 97411023341 + seed1 * 88763322+ 5647337)
    in
        (x, y)

modBinPow x y m = 
    if y == 0 then mod 1 m
    else let 
            z = modBinPow x (div y 2) m
         in
            if mod y 2 == 0 then mod (z * z) m
            else mod (z * z * x) m


prime n q = 
        let 
            prime_h n q seed1 seed2 = 
                if q == 0 then True
                else 
                    let 
                        (s1, s2) = myRNG seed1 seed2
                        a = mod seed2 n
                        rem = modBinPow a n n
                    in 
                        if rem == a then prime_h n (q - 1) s1 s2 else False
         in
             prime_h n q 13 1989

--- >>> prime 5210644015679228794060694325390955853335898483908056458352183851018372555735221 1000
--- True
--- >>> prime 5210644015679228794060694325390955853335898483908056458352183851018372555735223 1000
--- False
---











