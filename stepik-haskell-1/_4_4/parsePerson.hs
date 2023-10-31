data Error = ParsingError | IncompleteDataError | IncorrectDataError String

data Person = Person { firstName :: String, lastName :: String, age :: Int }

parsePerson :: String -> Either Error Person
parsePerson string 
    | hasWrongFormat fields = Left ParsongError
    | size fields < 3        = Left IncompleteDataError 
    -- дополнить кейсы - расписать сначала гарды, и лишь потом приступать к реализации функций  
    where
        fields = splitOn '\n' string
        hasWrongFormat fields = undefined

splitOn delimiter = foldr merge [[]] 
    where
        merge c rest@(l:ls) 
            | c == delimiter = [] : (rest) 
            | c /= delimiter = (c : l) : ls
