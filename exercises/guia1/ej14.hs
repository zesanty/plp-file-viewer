data AIH a = Hoja a | Bin (AIH a) (AIH a)


foldAIH :: (a -> b) -> (b -> b -> b) -> AIH a -> b
foldAIH cNil cBin a = case a of
        Hoja a -> cNil a
        Bin i d -> cBin (acc i) (acc d)
        where acc = foldAIH cNil cBin


altura:: AIH a -> Integer
altura = foldAIH (const 1) (\ri rd -> 1 + max ri rd )

tamano :: AIH a -> Integer 
tamano = foldAIH (const 1) (\ri rd -> ri + rd)