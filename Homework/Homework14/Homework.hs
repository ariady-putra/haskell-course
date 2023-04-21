import Data.List (find, isPrefixOf)

-- Question 1
-- Write a program that reads the file Data.txt and checks following properties:
main :: IO ()
main = do
    let filePath        = "Data.txt"
    records <- lines <$> readFile filePath
    let isValidName     = case extractRecord "Name:"     records of
            (Just n)   -> validName     n
            _          -> False
    let isValidEmail    = case extractRecord "Email:"    records of
            (Just e)   -> validEmail    e
            _          -> False
    let isValidPassword = case extractRecord "Password:" records of
            (Just p)   -> validPassword p
            _          -> False
    putStrLn $ case (isValidName, isValidEmail, isValidPassword) of
        (True, True, True) -> "Valid "
        _                  -> "Invalid "
        ++ filePath

extractRecord :: String -> [String] -> Maybe String
extractRecord field records =
    case find (/=Nothing) . map (extractField field) $ records of
        (Just f) -> f
        _ -> Nothing

extractField :: String -> String -> Maybe String
extractField field record =
    if field `isPrefixOf` record
        then Just $ dropWhile (' '==) . tail . dropWhile (':'/=) $ record
        else Nothing
-- >>> extractField "Name:"     "Name:     John Doe"
-- >>> extractField "Email:"    "Email:    John.Doe@iohk.io"
-- >>> extractField "Password:" "Password: Abc123"
-- Just "John Doe"
-- Just "John.Doe@iohk.io"
-- Just "Abc123"

-- - the name parameter contains at least two words
validName :: String -> Bool
validName n = case words n of
    (_:_:_) -> True
    _       -> False
-- >>> validName "John"
-- >>> validName "John Doe"
-- >>> validName "John Doe Iohk Io"
-- False
-- True
-- True

-- - the email parameter contains an @ sign and the domain name contains a dot
validEmail :: String -> Bool
validEmail = elem '.' . dropWhile (/='@')
-- >>> validEmail "John.Doe"
-- >>> validEmail "John.Doe@iohk"
-- >>> validEmail "John.Doe@iohk.io"
-- False
-- False
-- True

-- - the password parameter is at least six characters long and contains at least 
--   one number and a non-number character
validPassword :: String -> Bool
validPassword p = case p of
    (_:_:_:_:_:_:_) ->
        any        (`elem` "1234567890")  p &&
        any (not . (`elem` "1234567890")) p
    _ -> False
-- >>> validPassword "Abcdef"
-- >>> validPassword "123456"
-- >>> validPassword "Abc123"
-- False
-- False
-- True

-- If any of the checks do not pass return an appropriate error message.
-- Use the concept of Railway oriented programming as shown in the lesson.

-- Structure the solution as a cabal project. HINT: The library split that contains 
-- the function splitOn can be added to the cabal file as dependency. 
