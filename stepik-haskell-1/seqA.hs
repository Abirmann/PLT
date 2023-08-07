
seqA :: Integer -> Integer
seqA n
    | n >= 0 = 
        let
            helper (first, second, third) n
               | n == 0    = first
               | otherwise = helper (second, third, third +  second - (2 * first)) (n - 1)  
        in helper (1, 2, 3) n
    | otherwise = error "Sempai, something went wrong!" 
