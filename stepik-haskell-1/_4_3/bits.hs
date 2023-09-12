-- Integer can be represented as a list of bits with sign.

-- Implement functions of addition and multiplication of such numbers assuming
-- that lower bits come first in the list and higher buts come last.
-- You can assume that input cannot have numbers with leading zeroes.

data Bit = Zero | One    deriving (Read, Show, Eq)
data Sign = Minus | Plus deriving (Read, Show, Eq)
data Z = Z Sign [Bit]    deriving (Read, Show, Eq)

instance Ord Bit where
    compare One Zero = GT
    compare Zero One = LT
    compare _ _      = EQ

compBits [] [] = EQ
compBits [] (b:bs) = LT
compBits (b:bs) [] = GT
compBits (b1:b1s) (b2:b2s) 
        | b1 == b2  = compBits b1s b2s
        -- не оптимально на каждом шаге рекурсии вычислять длинну. Нужно считать один раз
        | length b1s > length b2s = GT
        | length b1s < length b2s = LT  
        | b1 > b2   = GT
        | otherwise = LT 
  


add :: Z -> Z -> Z
add (Z s1 b1) (Z s2 b2)
    | s1 == s2 = Z s1 $ sameSignBitsAdd (normalize (b1,b2)) Zero
    | b1 == b2 = Z Plus []
    | (compBits b1 b2) == GT  = Z s1 $ diffSignBitsAdd (normalize (b1,b2)) Zero
    -- не оптимально дважды разворачивать список ради удаления нулей
    | (compBits b1 b2) == LT  = Z s2 $ (reverse . dropWhile (== Zero) . reverse) (diffSignBitsAdd (normalize (b2,b1)) Zero)
        where 
            sameSignBitsAdd ([],[]) Zero = []
            sameSignBitsAdd ([],[]) One = [One]
            sameSignBitsAdd ((b1:b1s),(b2:b2s)) prevC = (fst resultAndCarry) : (sameSignBitsAdd (b1s,b2s) (snd resultAndCarry))
                where 
                    resultAndCarry = addBitsWithCarry b1 b2 prevC

            diffSignBitsAdd ([],[]) extra = []
            diffSignBitsAdd ((b1:b1s),(b2:b2s)) prevC = (fst resultAndBorrow) : (diffSignBitsAdd (b1s,b2s) (snd resultAndBorrow))
                where 
                    resultAndBorrow = substractBitsWithBorrow b1 b2 prevC

            addBitsWithCarry :: Bit -> Bit -> Bit -> (Bit, Bit)
            addBitsWithCarry a1 a2 c = 
                case (a1,a2,c) of
                    (One,One,One)    -> (One, One)
                    (One,One,Zero)   -> (Zero, One)
                    (One,Zero,One)   -> (Zero, One)
                    (One,Zero,Zero)  -> (One, Zero)
                    (Zero,One,One)   -> (Zero, One)
                    (Zero,One,Zero)  -> (One, Zero)
                    (Zero,Zero,One)  -> (One, Zero)
                    (Zero,Zero,Zero) -> (Zero, Zero)
           
            substractBitsWithBorrow :: Bit -> Bit -> Bit -> (Bit, Bit)
            substractBitsWithBorrow a1 a2 c = 
                case (a1,a2,c) of
                    (One,One,One)    -> (One,One) 
                    (One,One,Zero)   -> (Zero,Zero)
                    (One,Zero,One)   -> (Zero,Zero)
                    (One,Zero,Zero)  -> (One, Zero)
                    (Zero,One,One)   -> (Zero, One)
                    (Zero,One,Zero)  -> (One, One)        
                    (Zero,Zero,One)  -> (One, One)
                    (Zero,Zero,Zero) -> (Zero, Zero)
              
            normalize = unzip . (uncurry norm)
                where
                    norm [] [] = []
                    norm [] (x:xs) = (Zero,x) : norm [] xs
                    norm (x:xs) [] = (x,Zero) : norm xs []
                    norm (a:as) (b:bs) = (a,b) : norm as bs
    

mul :: Z -> Z -> Z
mul = undefined
