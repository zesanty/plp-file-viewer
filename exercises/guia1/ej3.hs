sumNuevo :: [Int] -> Int
sumNuevo xs = foldr (\x y -> x + y) 0 xs

elem2 :: (Foldable t, Eq a) => a -> t a -> Bool
elem2 a xs = foldr (\x rec -> (x == a) || rec) False xs


masmas :: [Integer] -> [Integer] -> [Integer]
masmas xs ys = foldr (:) xs ys

myFilter :: Foldable t => t a -> (a -> Bool) -> [a]
myFilter xs condition = foldr (\x rec -> if condition x then x:rec else rec ) [] xs


myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x rec -> f x : rec) []

-- Devuelve el máximo elemento de la lista según una función de comparación

mejorSegun :: Foldable t1 => (t2 -> t2 -> Bool) -> t1 t2 -> t2
mejorSegun f = foldr1 (\x y -> if f x y then x else y)


sumasParciales :: Num a => [a] -> [a]
sumasParciales xs = foldl (\r x -> r ++ (if null r then [x] else [x + last r])) [] xs

sumaAlt :: Num a => [a] -> a
sumaAlt = foldr (\x y -> x-y) 0

sumaAlt2 :: Num a => [a] -> a
sumaAlt2 = foldl (flip (-)) 0


-- nub :: (Foldable t, Eq a) => t a -> [a]
-- nub list = foldr (\x acc -> if(elem x acc) then acc else x:acc ) [] list

nub2 :: (Foldable t, Eq a) => t a -> [a]
nub2 = foldl (\acc x -> if elem x acc then acc else acc ++ [x]) []