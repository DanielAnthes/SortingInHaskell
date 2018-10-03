-- Insertion Sort

insertionsort :: (Ord a) => [a] -> [a]
insertionsort []     = []
insertionsort (x:xs) = insertionsort' xs [x]
    where insertionsort' [] ys = ys
          insertionsort' xs ys = insertionsort' (tail xs) (insert (head xs) ys)



-- Bubble Sort

bubblesort :: (Ord a) => [a] -> [a]
bubblesort [] = []
bubblesort xs = bubblesort' [] xs
    where bubblesort' srtd xs
             | tail xs == [] = srtd ++ xs
             | otherwise     = bubblesort' (srtd ++ [head bubbled]) (tail bubbled)
                 where bubbled  = bubble [] xs
                       bubble xs ys
                         |tail ys == []      = xs ++ ys
                       bubble xs (y1:y2:ys)  = if y1 < y2 then bubble (xs ++ [y1]) ([y2] ++ ys)
                                               else bubble (xs ++ [y2]) ([y1] ++ ys)



-- Selection Sort

selectionsort :: (Ord a) => [a] -> [a]
selectionsort [] = []
selectionsort xs = selectionsort' [] xs
    where selectionsort' xs ys
             | tail ys == [] = xs ++ ys
             | otherwise     = selectionsort' (xs ++ [smallest]) (removeFirst smallest ys)
                 where smallest = minimum ys



-- Auxiliary Functions
-- expects an ordered list and an orderable item of the same type, inserts the item at the correct index in the list
insert :: (Ord a) => a -> [a] -> [a]
insert x ys
    | ys == []  = [x]
    | otherwise = insert' [] x ys
    where insert' prev x sort
             | sort == []      = prev ++ [x]
             | x < (head sort) = prev ++ [x] ++ sort
             | otherwise       = insert' (prev ++ [head sort]) x (tail sort)



-- removes first ocurrence in list
removeFirst :: (Eq a) => a -> [a] -> [a]
removeFirst _ [] = []
removeFirst x xs
    | ind == -1 = xs
    | otherwise   = first ++ (tail second)
        where ind    = indexOf x xs
              split  = splitAt ind xs
              first  = fst split
              second = snd split


-- gives index of first occurrence of an item in a list
indexOf :: (Eq a, Integral b) => a -> [a] -> b 
indexOf x xs = indexOf' 0 x xs
    where indexOf' i x xs
            | xs == []       = -1
            | x == (head xs) = i
            | otherwise      = indexOf' (i + 1) x (tail xs)
