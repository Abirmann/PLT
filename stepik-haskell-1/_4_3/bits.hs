-- Integer can be represented as a list of bits with sign.

-- Implement functions of addition and multiplication of such numbers assuming
-- that lower bits come first in the list and higher buts come last.
-- You can assume that input cannot have numbers with leading zeroes.

data Bit = Zero | One    deriving (Read, Show)
data Sign = Minus | Plus deriving (Read, Show, Eq)
data Z = Z Sign [Bit]    deriving (Read, Show)


add :: Z -> Z -> Z
add (Z s1 b1) (Z s2 b2)
    | s1 == s2    = Z s1 $ sameSignBitsAdd b1 b2 Zero
    | s1 == Minus = Z s1 $ sameSignBitsAdd (map invert b1) b2 Zero
    -- нужно добавить нули в старшие разряды до второго и определять знак по последнему биту
    | s2 == Minus = Z s2 $ sameSignBitsAdd b1 (map invert b2) Zero
    where
        sameSignBitsAdd [] [] Zero = []
        sameSignBitsAdd [] [] One = [One]

        sameSignBitsAdd (b:bs) [] prevC = r : (sameSignBitsAdd bs [] c) 
            where
                r = fst $ addBitWithCarry b prevC Zero 
                c = snd $ addBitWithCarry b prevC Zero

        sameSignBitsAdd [] (b:bs) prevC = r : (sameSignBitsAdd bs [] c)
            where
                r = fst $ addBitWithCarry b prevC Zero 
                c = snd $ addBitWithCarry b prevC Zero

        sameSignBitsAdd (b1:b1s) (b2:b2s) prevC = r : (sameSignBitsAdd b1s b2s c)
            where 
                r = fst $ addBitWithCarry b1 b2 prevC 
                c = snd $ addBitWithCarry b1 b2 prevC

        
        

        addBitWithCarry :: Bit -> Bit -> Bit -> (Bit, Bit)
        addBitWithCarry One One One = (One, One)
        addBitWithCarry One One Zero = (Zero, One)
        addBitWithCarry One Zero One = (Zero, One)
        addBitWithCarry One Zero Zero = (One, Zero)
        addBitWithCarry Zero One One = (Zero, One)
        addBitWithCarry Zero One Zero = (One, Zero)
        addBitWithCarry Zero Zero One = (One, Zero)
        addBitWithCarry Zero Zero Zero = (Zero, Zero)
       
        invert :: Bit -> Bit
        invert Zero = One
        invert One = Zero

mul :: Z -> Z -> Z
mul = undefined
