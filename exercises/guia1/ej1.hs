-- Considerar las siguientes defniciones de funciones:
-- max2 (x, y) | x >= y = x
--             | otherwise = y
-- normaVectorial (x, y) = sqrt (x^2 + y^2)
-- subtract = flip (-)
-- predecesor = subtract 1
-- evaluarEnCero = \f -> f 0
-- dosVeces = \f -> f . f
-- flipAll = map flip
-- flipRaro = flip flip

-- i. ¿Cuál es el tipo de cada función? (Suponer que todos los números son de tipo Float).

-- (Float, Float) -> Float
    -- max2 (x,y) ....

-- (Float -> Float) -> Float
    -- normaVectorial()...

subtract2 :: Float -> Float -> Float
subtract2 = flip (-)

-- Float -> Float
predecesor = subtract2 1

-- (a->b)

evaluarEnCero :: (Float -> a) -> a
evaluarEnCero f = f 0

-- (a -> a) -> a -> a
dosVeces :: (a -> a) -> a -> a
dosVeces = \f -> f . f



-- pido una lista y devuelvo una lista
flipAll :: [Float -> Float -> Float] -> [Float -> Float -> Float]
-- MAP TOMA: ((a -> b -> c) -> b -> a -> c) -> [a -> b -> c] -> [b -> a -> c]
--          -                FUNCION        - [pide una lista?] 
flipAll = map flip



-- flip toma una función (a -> b -> c) y la convierte en (b -> a -> c).
--  Si aplicamos flip nuevamente, no vuelve mágicamente a la función original,
--  sino que produce una nueva función que opera con una permutación diferente de argumentos.
--  Esto no equivale a la identidad, como podría pensarse intuitivamente.
tflip :: (a -> b -> c) -> (b -> a -> c)
tflip f x y = f y x

-- flipRaro
-- Esto es diferente a dosVeces

-- (Float -> Float -> Float) -> Float -> Float -> Float
flipRaro :: Float -> (Float -> Float -> Float) -> Float -> Float
flipRaro = flip flip




-- ii. Indicar cuáles de las funciones anteriores no están currifcadas. Para cada una de ellas, defnir la función
-- currificada correspondiente. Recordar dar el tipo de la función.

test :: (a1 -> ((a2 -> b -> c1) -> b -> a2 -> c1) -> c2) -> a1 -> c2
test = flip flip flip
