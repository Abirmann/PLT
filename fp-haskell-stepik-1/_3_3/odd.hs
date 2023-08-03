data Odd = Odd Integer 
    deriving (Eq, Show)

instance Enum Odd where
    toEnum n = Odd (toInteger n)

    fromEnum (Odd n) = fromIntegral n

    succ (Odd a) = Odd (a + 2)

    pred (Odd a) = Odd (a - 2)

    enumFrom (Odd a) = Odd a : enumFrom (succ $ Odd a)

    enumFromTo (Odd a) (Odd limit)
        | a <= limit    = Odd a : enumFromTo (succ $ Odd a) (Odd limit)
        | otherwise = []

    enumFromThen (Odd a) (Odd b) = Odd a : enumFromThen (Odd b) (Odd (2*b - a))

    enumFromThenTo (Odd a) (Odd b) (Odd limit)
        | b-a > 0 && a > limit = []
        | b-a < 0 && a < limit = []
        | otherwise            = Odd a : enumFromThenTo (Odd b) (Odd (2*b - a)) (Odd limit) 
