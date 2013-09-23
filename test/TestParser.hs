import Parser

program = "Define bool (A : type) : type := Pi (x:A) (y:A), A.\
Define True (A : type) : bool A := \
    (x:A) (y:A) => x."
    

main = do
    
    content <- getContents
    case parse "(stdin)" program of
        Left err -> putStrLn $ "Error: " ++ (show err)
        Right prog -> putStrLn $ "Success: " ++ (show prog)
    putStrLn "Good bye."

