
rev_tr ls = 
    let 
        rev_tr_h rem_ls curr_ls = 
            case rem_ls of
                [] -> curr_ls
                x:xs -> rev_tr_h xs (x:curr_ls)
    in 
        rev_tr_h ls []

-- >>> rev_tr [1,3,4,5]
-- [5,4,3,1]
--

-- Invariant: the arguments of 
-- rev_tr_h:- rem_ls and curr_ls always follow
-- reverse(curr_ls) ++ rem_ls == ls
-- curr_ls in decreasing in size at each step
-- so finally reverse(curr_ls) == ls and it is
-- returned
-- Time Complexity - O(n)

merge_tr ls1 ls2 = 
    let 
        merge_tr_h rem1 rem2 curr = 
            case (rem1, rem2) of
                ([],_) -> rem2 ++ curr
                (_,[]) -> rem1 ++ curr
                (x:xs, y:ys) -> if x < y then merge_tr_h xs rem2 (x:curr)
                                else merge_tr_h rem1 ys (y:curr)
    in
        rev_tr (merge_tr_h ls1 ls2 [])

--- >>> merge_tr [3,5,8,69,300] [1,2,7,9,30]
--- [1,2,3,5,7,8,9,30,300,69]
---
--- Invariant: multiset(rem1 ++ rem2 ++ curr) == multiset(ls1 ++ ls2)
--- and rem1,rem2, reverse(curr) are always sorted
--- head rem1 and head rem2 >= all elements in curr
--- Time Complexity - O(n)

fib_tr n = 
    let 
        fib_tr_h prev2 prev1 i = 
            if i == n then (prev1 + prev2)
            else fib_tr_h (prev1) (prev1 + prev2) (i + 1)
    in
        if n <= 1 then n
        else fib_tr_h 0 1 2

--- >>> fib_tr 1000
--- 43466557686937456435688527675040625802564660517371780402481729089536555417949051890403879840079255169295922593080322634775209689623239873322471161642996440906533187938298969649928516003704476137795166849228875
---

-- Invariant: 
-- i >= 2, prev1 = fib(i-1), prev2 = fib(i-2)
-- Time Complexity O(n), assuming O(1) addition,
-- If addition of n digit number is O(n)
-- then time complexity = O(n^2)



insertion_sort_tr ls = 
    let
        insert sorted_ls e prev_elems =
            case sorted_ls of
                [] -> (rev_tr prev_elems) ++ [e]
                x:xs -> if e <= x then (rev_tr prev_elems) ++ (e:sorted_ls)
                        else insert xs e (x:prev_elems)
        helper sorted_ls rem = 
            case rem of 
                [] -> sorted_ls
                x:xs -> helper (insert sorted_ls x []) xs
        
    in
        helper [] ls

-- >>> insertion_sort_tr [3,2,9,69,22,13,42,24,33,1,-1,2,2,68,68,-13,198]
-- [-13,-1,1,2,2,2,3,9,13,22,24,33,42,68,68,69,198]
--

-- insert function->
-- all elements in prev_elems <= e
-- (reverse prev_elements) ++ [e] ++ sorted_ls = given ls
-- sorted_ls is always sorted

-- invariant for sort function->
-- sorted_ls is always sorted
-- multiset(sorted_ls ++ rem) == multiset(ls)

-- Time Complexity O(n^2)

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




-- >>> quick_sort [3,3,3,3,2,1,1,1,1,4,4,4,4]
-- [1,1,1,1,2,3,3,3,3,4,4,4,4]
--

-- Split function
-- Assuming it works for smaller size list, 
-- split is called on tail of ls 
-- and head ls is inserted in the one of
-- splitted lists appropiately.

--- quicksort ->
--- Assumming it works for samller lists
--- a given list is split into 3 peices,
--- a pivot element, a list with elements <= pivot,
--- list with elements > pivot
--- then the two smaller lists are sorrted
--- and the three peices are joined appropiately.

--- Time Complexity - O(n^2) worst case
--- O(nlogn) on average


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

--- Assuming binary_search works for smaller lists
--- A given sorted list is split into two non empty halves
--- if e <= last element of first half
--- then e must be searched in the first half
--- otherwise it must be searched in the second half.
--- Time complexity O(n * log (hi - lo))

--------------------------------------------------------------------
--- Problem 2


myRNG seed1 seed2 = 
    let 
        x = (seed1 * 1103569345 + seed2 * 49993101 + 3422313)
        y = (seed2 * 97411023341 + seed1 * 88763322 + 5647337)
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

--- >>> prime 5210644015679228794060694325390955853335898483908056458352183851018372555735221 10
--- True
---
--- >>> prime 5210644015679228794060694325390955853335898483908056458352183851018372555735223 10
--- False
---



-----------------------------------------------------------------
-- Problem 3


double_sum a b c d f =
    let 
        sum_inner lo hi y f =
            if lo > hi then 0
            else f lo y + sum_inner (lo + 1) hi y f
    in
        if a > b then 0
        else sum_inner c d a f + double_sum (a + 1) b c d f


-- >>> double_sum 1 3 1 3 (\x y -> x + y)
-- 36
-- Time Complexity O((b - a) * (d - c))


first_derivate f x =
    let 
        h = 1e-8
    in 
        (f (x + h) - f (x - h)) / (2.0 * h)



newton_root f curr_guess tol = 
    let 
        f' = first_derivate f
    in
        if abs (f curr_guess) < tol then curr_guess
        else 
            let
                new_guess = curr_guess - ((f curr_guess) / (f' curr_guess))
            in
                newton_root f new_guess tol


--- >>> newton_root (\x -> (x * x) - 3) 2 1e-8
--- 1.732050810013831
---



