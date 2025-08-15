recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)


genLista :: a -> (a -> a) -> Integer -> [a]
genLista n f 1 = [n]
genLista n f k = n : genLista (f n) f (k-1) 


genLista2 :: a -> (a -> a) -> Integer -> [a]
genLista2 n f k = foldl (\rec x -> if null rec then [n] else rec ++ [f (last rec)]  ) [] [1..k]


desdeHasta d h = genLista d (+1) (h-d+1)