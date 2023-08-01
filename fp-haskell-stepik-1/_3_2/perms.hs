{- 
Основательный ответ о подходах к реализации перестановок https://stackoverflow.com/questions/24484348/what-does-this-list-permutations-implementation-in-haskell-exactly-do/24564307#24564307
-}

perms :: [a] -> [[a]]

perms [] = [[]]
perms (x:xs) = concatMap (insertElem x) (perms xs)
    where
        insertElem x [] = [[x]]
        insertElem x (y:ys) = (x:y:ys) : map (y:) (insertElem x ys) 
