data Error = ParsingError | IncompleteDataError | IncorrectDataError String

data Person = Person { firstName :: String, lastName :: String, age :: Int }

parsePerson :: String -> Either Error Person
parsePerson string = createPerson . validateAge . getRequiedFields . getAllFields $ splitOn '\n' string


getAllFields :: [String] -> Either Error [(String, String)]

getAllFields [] = Right []
getAllFields strings = undefined

getRequiedFields :: Either Error [(String, String)] -> Either Error (String, String, String) 
getRequiedFields = undefined

validateAge :: Either Error (String, String, String) -> Either Error (String, String, String)
validateAge = undefined

createPerson :: Either Error (String, String, String) -> Either Error Person
createPerson = undefined

splitOn delimiter = foldr collect [[]] 
    where
        collect char all@(current:completed) 
            | char == delimiter  = [] : all 
            | char /= delimiter  = (char : current) : completed 
