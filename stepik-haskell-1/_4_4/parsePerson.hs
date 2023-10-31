data Error = ParsingError | IncompleteDataError | IncorrectDataError String

data Person = Person { firstName :: String, lastName :: String, age :: Int }

parsePerson :: String -> Either Error Person
parsePerson = undefined  

splitBy :: Char -> String -> [String]
splitBy delimiter = foldr merge [[]] 
    where
        merge c (l:ls) 
            | c == delimiter = [] : (l:ls) 
            | c /= delimiter = (c:l):ls
