import Data.Foldable (Foldable(fold))
import Debug.Trace (trace)

instance Show a => Show (AB a) where
    show Nil = "Nil"
    show (Bin izq v der) = "(" ++ show izq ++ " " ++ show v ++ " " ++ show der ++ ")"

data AB a = Nil | Bin (AB a) a (AB a)
instance Eq a => Eq (AB a) where
    Nil == Nil = True
    Nil == (Bin izq r der) = False
    (Bin izq r der) == (Bin izq2 r2 der2) = (r == r2) && (izq == izq2) && (der == der2)

instance Ord a => Ord (AB a) where
    compare Nil Nil = EQ
    compare Nil (Bin {}) = LT
    compare (Bin {}) Nil = GT
    compare (Bin _ r _) (Bin _ r2 _) = compare r r2

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


esNil:: AB a -> Bool
esNil a = foldAB (True) (\ri v rd -> False) a

altura:: AB a -> Int
altura a = foldAB (0) (\ri v rd -> 1+max ri rd ) a

cantNodos:: AB a -> Int
cantNodos a = foldAB (0) (\ri v rd -> 1 + ri + rd ) a


foldAB2 :: (b -> a -> b -> b) -> AB a -> b
foldAB2 f (Bin izq r der) = f (acc izq) r (acc der)
                                where acc = foldAB2 f


esABB :: Ord a => AB a -> Bool
esABB = recAB True (\ri v rd i d -> mayor i v && menor d v && ri && rd)
        where
          mayor Nil _ = True
          mayor (Bin _ x _) y = x <= y
          menor Nil _ = True
          menor (Bin _ x _) y = y <= x


esABB2 :: (Ord a, Show a) => AB a -> Bool
esABB2 = recAB True (\ri v rd i d ->
    trace ("Checking value: " ++ show v ++ ", left result: " ++ show ri ++ ", right result: " ++ show rd ++
           ", left structure: " ++ show i ++ ", right structure: " ++ show d )
    (mayor i v && menor d v && ri && rd))
  where
    mayor Nil _ = True
    mayor (Bin _ x _) y = trace ("Checking if " ++ show x ++ " <= " ++ show y) (x <= y)
    menor Nil _ = True
    menor (Bin _ x _) y = trace ("Checking if " ++ show y ++ " <= " ++ show x) (y <= x)

{- 
        ghci> esABB2 (Bin (Bin (Bin (Bin Nil 31 Nil) 3 Nil) 3 Nil) 5 (Bin Nil 11 Nil))
        Checking value: 31, left result: True, right result: True, left structure: Nil, right structure: Nil
        Checking value: 3, left result: True, right result: True, left structure: (Nil 31 Nil), right structure: Nil
        Checking if 31 <= 3
        Checking value: 3, left result: False, right result: True, left structure: ((Nil 31 Nil) 3 Nil), right structure: Nil
        Checking if 3 <= 3
        Checking value: 11, left result: True, right result: True, left structure: Nil, right structure: Nil
        Checking value: 5, left result: False, right result: True, left structure: (((Nil 31 Nil) 3 Nil) 3 Nil), right structure: (Nil 11 Nil)
        Checking if 3 <= 5
        Checking if 5 <= 11
        False
-}