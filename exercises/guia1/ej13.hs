data AB a = Nil | Bin (AB a) a (AB a)
instance Eq a => Eq (AB a) where
    Nil == Nil = True
    Nil == _ = False
    (Bin izq r der) == (Bin izq2 r2 der2) = (r == r2) && (izq == izq2) && (der == der2)


foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB cNil cBin a = case a of
        Nil -> cNil
        Bin i v d -> cBin (acc i) v (acc d)
        where acc = foldAB cNil cBin


recAB :: b -> (b -> a -> b -> AB a -> AB a -> b) -> AB a -> b
recAB cNil cBin a = case a of
        Nil -> cNil
        Bin i v d -> cBin (acc i) v (acc d) i d
        where acc = recAB cNil cBin

esNil :: AB a -> Bool
esNil Nil = True
esNil _ = False

cantHojas:: Eq a => AB a -> Integer
cantHojas = recAB 0 (\ri x rd i d -> if esNil i && esNil d then 1 else ri + rd  )

ramas :: AB a -> [[a]]
ramas = foldAB [] (\recIzq r recDer -> if null recIzq && null recDer then [[r]] else map (r:) (recIzq++recDer))
-- Concatena las listas y agregales la raiz
-- ramas (Bin (Bin Nil 3 (Bin (Bin Nil 67 Nil) 5 (Bin Nil 19 Nil))) 4 (Bin Nil 84 Nil)) = [[4,3,5,67],[4,3,5,19],[4,84]] 

-- Espejo devuelve otro AB invertido
espejo :: AB a -> AB a
espejo = foldAB Nil (\ri r rd -> Bin rd r ri)


-- mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
-- mapDoble f = foldr (\x rec ys -> if null ys then []
--                         else f x (head ys) : rec (tail ys)) (const [])


-- La funcion espera AB b -> Bool. Para el caso borde donde el arbol que estamos recorriendo recursivamente es nil pero el que le pasamos como parametro TODAVIA NO ES NIL usamos la funcion (\arbol -> esNil arbol)).
mismaEstructura :: AB a -> AB a -> Bool
mismaEstructura = foldAB esNil (\ri r rd b -> case b of
    Nil -> False
    (Bin a' b' c') -> ri a' && rd c')
