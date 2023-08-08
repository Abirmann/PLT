data LogLevel = Error | Warning | Info deriving (Show)

cmp Info Info       = EQ
cmp Warning Warning = EQ
cmp Error Error     = EQ
cmp Info _          = LT
cmp Error _         = GT
cmp _ Info          = GT
cmp _ Error         = LT 

   
