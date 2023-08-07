
coins = [2, 3, 7]

change n | n < 0     = []
         | n == 0    = [[]]
         | otherwise = [x:xs | x <- coins, xs <- change (n - x)]

