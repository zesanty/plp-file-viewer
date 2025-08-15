foldNat :: (Integer -> a -> a) -> a -> Integer -> a
foldNat _ b 0 = b
foldNat f b n = f n (foldNat f b (n-1))

potencia :: Integer -> Integer -> Integer
potencia a = foldNat (\x rec -> rec*a) 1