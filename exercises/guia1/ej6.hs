recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)


sumList :: [Int] -> Int
sumList = recr (\x xs acc -> x + acc) 0

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna a = recr (\x xs acc -> if a == x then xs else x : acc) []
-- No podemos implementar sacarUna con foldr porque no hay "memoria".
-- Cada vez que procesamos un elemento, no sabemos si ya sacamos algún
-- elemento anterior o no. Dicho de otra manera, con foldr no podemos
-- hacer un return temprano y "abortar" el resto del fold.

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado e = recr
    (\x xs rec -> if e <= x  -- If `e` should go before `x`, insert it here
                  then e : x : xs
                  else x : rec)  -- Otherwise, continue recursively
    [e]  -- Start with a list containing `e` to handle the last element case
