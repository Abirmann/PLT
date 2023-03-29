
fibonacci :: Integer -> Integer
fibonacci n = helper 0 1 n 

helper prevAcc acc n
     | n == 0 = prevAcc 
     | n > 0  = helper acc (prevAcc + acc) (n - 1) 
     | n < 0  = helper acc (prevAcc - acc) (n + 1) 
