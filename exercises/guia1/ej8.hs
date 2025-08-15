import Control.Arrow (Arrow(first))
import Debug.Trace
miCurry :: ((a, b) -> t) -> a -> b -> t
miCurry f x y = f (x,y)

miUncurry :: (t1 -> t2 -> t3) -> (t1, t2) -> t3
miUncurry f (x,y) = f x y



mapPares :: Foldable t => (t1 -> t2 -> a) -> t (t1, t2) -> [a]
mapPares f = foldr (\x rec -> (miUncurry f x) : rec) []


-- Dado dos listas
-- armo una lista de pares que tiene, en cada posicion, el elemento que corresponde
-- a la posición de la otra

-- [1, 3] [4, 5] --> [(1,3), (3,5)]
armarPares :: [a] -> [b] -> [(a, b)]
armarPares [] _ = []
armarPares _ [] = []
armarPares xs ys = (head xs,head ys):armarPares (tail xs) (tail ys)

armarPares2 :: [a] -> [b] -> [(a, b)]
armarPares2 = foldr (\x rec ys -> if null ys then []
                     else (x, head ys) : rec (tail ys)
                     ) (const [])



-- Input: zipWith (\x y -> 2*x + y) [1..4] [5..8]
-- Output: [7,10,13,16]

mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f = foldr (\x rec ys -> if null ys then []
                            else f x (head ys) : rec (tail ys)) (const [])


mapDobleTest :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDobleTest _ [] _ = []
mapDobleTest _ _ [] = []
mapDobleTest f xs ys = f (head xs) (head ys) : mapDobleTest f (tail xs) (tail ys)
--- f 1 1 :( map2test f [2,3] [2,3] ) =>
--- f 2 2 :  


mapDoble2 :: (Show a, Show b, Show c) => (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble2 f = foldr (\x rec ys ->
                    if null ys
                    then []
                    else let result = f x (head ys) : rec (tail ys)
                         in trace ("x: " ++ show x ++ ", ys: " ++ show ys ++ ", result: " ++ show result) result)
                  (const [])
-- Hagamos un poco de backtracking
--  mapDoble (\x y -> x + y) [1,2,3] [1,2,3]
-- entra al foldr

-- mapDoble2 (\x y -> x + y) [1,2,3] [1,2,3]
-- x: 3, ys: [3], result: [6]
-- x: 2, ys: [2,3], result: [4,6]
-- x: 1, ys: [1,2,3], result: [2,4,6]
-- [2,4,6]