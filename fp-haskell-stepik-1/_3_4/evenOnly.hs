evenOnly :: [a] -> [a]
evenOnly = snd . foldr (\a (xs, ys) -> (a:ys, xs)) ([],[])


