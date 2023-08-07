
integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = 
    let 
        h = (b - a) / n 
        n = 100000
    in  h * ((((f a) + (f b)) / 2) + sum f (n - 1) 0 a b h)
        where
            sum f 1 acc a b h = acc 
            sum f n acc a b h = sum f (n - 1) (acc + (f (b - h))) a (b - h) h  
