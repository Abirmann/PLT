-- Integer can be represented as a list of bits with sign.

-- Implement functions of addition and multiplication of such numbers assuming
-- that lower bits come first in the list and higher buts come last.
-- You can assume that input cannot have numbers with leading zeroes.

data Bit = Zero | One    deriving (Read, Show)
data Sign = Minus | Plus deriving (Read, Show, Eq)
data Z = Z Sign [Bit]    deriving (Read, Show)


add :: Z -> Z -> Z
add (Z s1 b1) (Z s2 b2)
    | s1 == s2    = Z s1 $ sameSignBitsAdd (normalize (b1,b2)) Zero

    -- TODO - инвертирование нормализованного значения и решение о знаке по последнему разряду
    -- | s1 == Minus = Z s1 $ sameSignBitsAdd (normalize (b1,b2)) Zero 
    -- | s2 == Minus = Z s2 $ sameSignBitsAdd (normalize (b1,b2)) Zero 
    where
        sameSignBitsAdd ([],[]) Zero = []
        sameSignBitsAdd ([],[]) One = [One]
        sameSignBitsAdd ((b1:b1s),(b2:b2s)) prevC = r : (sameSignBitsAdd (b1s,b2s) c)
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
          
normalize = unzip . (uncurry norm)
    where
        norm [] [] = []
        norm [] (x:xs) = (Zero,x) : norm [] xs
        norm (x:xs) [] = (x,Zero) : norm xs []
        norm (a:as) (b:bs) = (a,b) : norm as bs



mul :: Z -> Z -> Z
mul = undefined
