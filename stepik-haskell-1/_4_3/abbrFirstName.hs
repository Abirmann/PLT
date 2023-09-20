data Person = Person { firstName :: String, lastName :: String, age :: Int }

-- TODO переписать на решение с синонимом - будет лаконичнее
abbrFirstName p 
    | length name < 2 = p
    | otherwise       = p {firstName = head name : "."} 
    where
        name = firstName p 
