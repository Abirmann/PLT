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
    | s1 == s2 = Z s1 $ sameSignBitsAdd Zero (normalize (b1,b2)) 
    | b1 == b2 = Z Plus []
    | (compBits b1 b2) == GT  = Z s1 $ diffSignBitsAdd Zero (normalize (b1,b2)) 
    -- не оптимально дважды разворачивать список ради удаления нулей
    | (compBits b1 b2) == LT  = Z s2 $ (reverse . dropWhile (== Zero) . reverse) (diffSignBitsAdd Zero (normalize (b2,b1)))
         
sameSignBitsAdd Zero ([],[]) = []
sameSignBitsAdd One ([],[]) = [One]
sameSignBitsAdd prevC ((b1:b1s),(b2:b2s)) = (fst resultAndCarry) : (sameSignBitsAdd (snd resultAndCarry) (b1s,b2s))
    where 
        resultAndCarry = addBitsWithCarry prevC b1 b2 

diffSignBitsAdd extra ([],[]) = []
diffSignBitsAdd prevC ((b1:b1s),(b2:b2s)) = (fst resultAndBorrow) : (diffSignBitsAdd (snd resultAndBorrow) (b1s,b2s))
    where 
        resultAndBorrow = substractBitsWithBorrow prevC b1 b2 

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
mul (Z s1 b1) (Z s2 b2) 
    | b1 == [] || b2 == [] = Z Plus []
    | s1 == s2  = Z Plus bits
    | otherwise = Z Minus bits
    where
        bits = foldr (\x y -> sameSignBitsAdd Zero (normalize (x,y))) [] $ partialProducts b1 b2 
        
        partialProducts :: [Bit] -> [Bit] -> [[Bit]]
        partialProducts [] _ = [[]]
        partialProducts (b1:b1s) b2 =
            case b1 of
                One  -> b2 : partialProducts b1s (Zero : b2)
                Zero -> partialProducts b1s (Zero : b2)
