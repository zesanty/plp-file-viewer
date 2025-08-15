data RoseTree a = Hoja a | RoseTree [RoseTree a]


foldRose :: (a -> b) -> ([a] -> b) -> RoseTree a -> b
foldRose cNil cBin a = case a of
        Hoja a -> cNil a
        RoseTree arr -> cBin (acc arr)
        where acc = foldRose cNil cBin