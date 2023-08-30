-- Integer can be represented as a list of bits with sign.

-- Implement functions of addition and multiplication of such numbers assuming
-- that lower bits come first in the list and higher buts come last.
-- You can assume that input cannot have numbers with leading zeroes.

data Bit = Zero | One    deriving (Read, Show)
data Sign = Minus | Plus deriving (Read, Show, Eq)
data Z = Z Sign [Bit]    deriving (Read, Show)


add :: Z -> Z -> Z
add (Z s1 b1) (Z s2 b2)
    | s1 == s2  = Z s1 $ sameSignBitsAdd b1 b2 Zero
    | otherwise = undefined
    where
        sameSignBitsAdd [] [] Zero = []
        sameSignBitsAdd [] [] One = [One]

        sameSignBitsAdd (bb1:bb1s) [] prevCC = rr : (sameSignBitsAdd bb1s [] cc) 
            where
                rr = fst $ addBitWithCarry bb1 prevCC Zero 
                cc = snd $ addBitWithCarry bb1 prevCC Zero


        sameSignBitsAdd [] (b2:b2s) prevC = r : (sameSignBitsAdd b2s [] c)
            where
                r = fst $ addBitWithCarry b2 prevC Zero 
                c = snd $ addBitWithCarry b2 prevC Zero

        sameSignBitsAdd (b1:b1s) (b2:b2s) prevC = r : (sameSignBitsAdd b1s b2s c)
            where 
                r = fst $ addBitWithCarry b1 b2 prevC 
                c = snd $ addBitWithCarry b1 b2 prevC

        addBitWithCarry :: Bit -> Bit -> Bit -> (Bit, Bit)
        addBitWithCarry One One c = (Zero, One)
        addBitWithCarry One Zero c = (One, c)
        addBitWithCarry Zero One c = (One, c)
        addBitWithCarry Zero Zero c = (Zero, c)


mul :: Z -> Z -> Z
mul = undefined
