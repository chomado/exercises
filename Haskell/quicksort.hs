quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) = 
    let smallerOrEqual = [a | a <- xs, a <= x]
        larger = [a | a <- xs, a > x]
    in quicksort smallerOrEqual ++ [x] ++ quicksort larger

--
quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x : xs) =
    let smallerOrEqual = filter (<= x) xs
        larger = filter (> x) xs
    in quicksort' smallerOrEqual ++ [x] ++ quicksort' larger
 
-- 

quicksort'' :: (Ord a) => [a] -> [a]
quicksort'' [] = []
quicksort'' (x : xs) = quicksort smallerOrEqual ++ [x] ++ quicksort larger
                       where smallerOrEqual = [a | a <- xs, a <= x]
                             larger = [b | b <- xs, b > x]