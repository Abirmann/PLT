data Bit = Zero | One    deriving (Read, Show)
data Sign = Minus | Plus deriving (Read, Show)
data Z = Z Sign [Bit]    deriving (Read, Show)


add :: Z -> Z -> Z
add = const

mul :: Z -> Z -> Z
mul = undefined
