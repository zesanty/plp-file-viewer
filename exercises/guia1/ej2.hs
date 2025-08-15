miCurry f x y = f (x,y)

miUncurry f (x,y) = f x y

-- f x y = x + y
-- f' = miUncurry f  => curry se comporta como |  f (x,y) = x + y