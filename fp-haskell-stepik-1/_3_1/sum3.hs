
sum3 :: Num a => [a] -> [a] -> [a] -> [a]

sum3 [] [] [] = [] 

sum3 (x:xs) [] [] = x : sum3 xs [] []
sum3 [] (x:xs) [] = x : sum3 [] xs [] 
sum3 [] [] (x:xs) = x : sum3 [] [] xs

sum3 (x:xs) (y:ys) [] = x + y : sum3 xs ys []
sum3 (x:xs) [] (y:ys) = x + y : sum3 xs [] ys
sum3 [] (x:xs) (y:ys) = x + y : sum3 [] xs ys


sum3 (x:xs) (y:ys) (z:zs) = x + y + z : sum3 xs ys zs 

