-- авторское решение

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (x : xs) = 
    case groupElems xs of
        [] -> [[x]]
        (g : gs) | x == head g -> (x : g) : gs
                 | otherwise   -> [x] : g : gs
                           


-- мое решение.....

-- groupElems :: Eq a => [a] -> [[a]]
-- groupElems [] = []
-- groupElems xs
--     | tailGroup == [] = [headGroup]
--     | otherwise       = headGroup : (groupElems tailGroup) 
--     where headGroup = fst (group ([],xs))
--           tailGroup = snd (group ([],xs))
-- 
-- group :: Eq a => ([a],[a]) -> ([a],[a])
-- group (headGroup,[]) = (headGroup,[])  
-- 
-- group (headGroup,(x:[]))
--     | length headGroup == 0 = ([x],[])
--     | x == head headGroup = (x : headGroup, [])
--     | otherwise           = (headGroup, []) 
-- 
-- group (headGroup,(x1:x2:xs) ) 
--     | x1 == x2  = group (x1 : headGroup, (x2 : xs))
--     | otherwise = (x1 : headGroup, x2:xs)
