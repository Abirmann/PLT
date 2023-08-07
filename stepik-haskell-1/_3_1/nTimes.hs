
nTimes :: a -> Int -> [a]
nTimes a n = helper a n []
    where
    helper _ 0 ls = ls
    helper a n as = helper a (n - 1) (a : as)
