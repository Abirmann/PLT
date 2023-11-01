data Error = ParsingError | IncompleteDataError | IncorrectDataError String

data Person = Person { firstName :: String, lastName :: String, age :: Int }

parsePerson :: String -> Either Error Person
-- всю валидацию сделать через цепочку вида:
-- validate3 . validate2 . validate1 $ Either Error [String]
-- и подумать, как менять тип у Right конструктора
parsePerson string = undefined 

splitOn delimiter = foldr collect [[]] 
    where
        collect char all@(current:completed) 
            | char == delimiter  = [] : all 
            | char /= delimiter  = (char : current) : completed 
