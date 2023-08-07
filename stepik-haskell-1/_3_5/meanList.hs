meanList :: [Double] -> Double
meanList = uncurry (/) . foldr (\x (sum, count) -> (sum + x, succ count)) (0, 0)
