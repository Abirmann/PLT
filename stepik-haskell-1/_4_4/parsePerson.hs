data Error = ParsingError | IncompleteDataError | IncorrectDataError String

data Person = Person { firstName :: String, lastName :: String, age :: Int }

parsePerson :: String -> Either Error Person
parsePerson = undefined  

splitOn delimiter = foldr merge [[]] 
    where
        merge c rest@(l:ls) 
            | c == delimiter = [] : (rest) 
            | c /= delimiter = (c : l) : ls
